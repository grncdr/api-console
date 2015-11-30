module Contentful.Data (ContentType, Field, FieldType(..), lookupType) where 
import Dict
import Json.Decode exposing (Decoder, object3, (:=), maybe, at, string)

{-|
This module implements just enough of the ContentType data model to power
auto-completion for the custom matcher in Contentful.Routes
-}

type FieldType =
  Symbol
  | Text
  | Integer
  | Number
  | Boolean
  | Location
  | Date
  | Object
  | Link
  | LinkArray
  | SymbolArray

type alias Field = { id: String, type': FieldType }

type alias ContentType =
  { id: String
  , fields: List Field
  }

typesByName = Dict.fromList [ ("Symbol", Symbol)
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
                            ]

lookupType stype atype =
  let
      get = (flip Dict.get) typesByName
  in
     if stype == "Array" then
        case atype of
          Just s -> get (s ++ stype)
          Nothing -> Nothing
     else
        get stype

{-
decodeField : Decoder Field
decodeField =
  let 
      (id, stype, atype) = object3 (,)
        ("id" := string)
        ("type" := string)
        (maybe (at ["items", "type"] string))
      type' = lookupType stype atype
  in
     { id = id, type' = type' }

decodeContentType : Decoder ContentType
decodeContentType =
  let
      (id, fields) = object2 (,)
        (at ["sys", "id"] string)
        ("fields" := list decodeField)
  in
     { id = id, fields = fields }

decodeField : Decoder Field
decodeField =
  let 
      (id, stype, atype) = object3 (,)
        ("id" := string)
        ("type" := string)
        (maybe (at ["items", "type"] string))
      type' = lookupType stype atype
  in
     { id = id, type' = type' }
-}
