module Contentful.Decode (array, contentType) where

import Contentful.Data exposing (..)
import Dict
import Json.Decode exposing (..)

typeByName : String -> Maybe FieldType
typeByName = [ ("Symbol", Symbol)
             , ("Text", Text)
             , ("Integer", Integer)
             , ("Number", Number)
             , ("Boolean", Boolean)
             , ("Location", Location)
             , ("Date", Date)
             , ("Object", Object)
             , ("Link", Link)
             , ("LinkArray", LinkArray)
             , ("SymbolArray", SymbolArray)
             ] |> Dict.fromList |> (flip Dict.get)

lookupType stype atype =
  if stype == "Array" then
     case atype of
       Just s -> typeByName (s ++ stype)
       Nothing -> Nothing
   else
     typeByName stype

sysId : Decoder String
sysId = at ["sys", "id"] string

field : Decoder Field
field = customDecoder value (\v ->
  let 
      id = decodeValue sysId v
      stype = decodeValue ("type" := string) v
      atype = decodeValue (maybe (at ["items", "type"] string)) v

      triple = Result.map3 (\a b c -> (a, b, c)) id stype atype
      construct (id, stype, atype) =
        case lookupType stype atype of
          Just t -> Ok { id = id, type' = t }
          Nothing -> Err ("Invalid field type " ++ stype)
  in
     Result.andThen triple construct)


contentType : Decoder ContentType
contentType =
  let
      construct id fields = { id = id, fields = fields }
  in
      object2 construct sysId ("fields" := list field)

array : Decoder a -> Decoder (List a)
array f =
  "items" := list f
