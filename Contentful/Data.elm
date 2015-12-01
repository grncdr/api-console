module Contentful.Data (ContentType, Field, FieldType(..)) where 

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
