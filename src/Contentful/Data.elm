module Contentful.Data (Space, ApiKey, ContentType, Field, FieldType(..)) where 

type alias Space = {
  id: String,
  name: String
}

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

type alias ApiKey = { id: String, name: String, token: String }
