module Contentful.Routes (deliveryAPI) where

import Matcher exposing (..)
import Contentful.Data exposing (..)
import String

pathVariable name = chain [ static "/", delimitedVariable "/" name ]

deliveryAPI : List ContentType -> Matcher
deliveryAPI contentTypes =
  chain [ static "GET /spaces"
        , pathVariable "space"
        , oneOf [ chain [ static "/entries"
                        , oneOf [ pathVariable "entry", entryQueryParameters contentTypes]
                        ]
                , chain [ static "/assets"
                        , oneOf [ pathVariable "asset", assetQueryParameters ]
                        ]
                , chain [ static "/content_types"
                        , oneOf [ pathVariable "contentType", contentTypeQueryParameters ]
                        ]
                ]
        ]

queryParameters matchers =
  chain [ static "?" , fencePost "&" (oneOf matchers) ]

commonQueryParameters =
  [ simpleQueryParameter "order"
  , simpleQueryParameter "query"
  , typedQueryParameters [ "sys.", "id" ] Symbol
  , typedQueryParameters [ "sys.", "createdAt" ] Date
  , typedQueryParameters [ "sys.", "updatedAt" ] Date
  ]

contentTypeQueryParameters = queryParameters commonQueryParameters

assetQueryParameters =
  queryParameters
    (commonQueryParameters ++ [ fieldParameters { id = "title", type' = Text }
                              , fieldParameters { id = "description", type' = Text }
                              ]
    )

entryQueryParameters contentTypes =
  queryParameters (dynamicEntryQueryParameter contentTypes :: commonQueryParameters)

dynamicEntryQueryParameter contentTypes match =
  let
      contentTypeId =
        List.head << List.filter (\(name, v) -> name == "content_type") <| matchBindings match
      contentType = case contentTypeId of
        Nothing -> Nothing
        Just (_, id) -> List.head << List.filter (\ct -> ct.id == id) <| contentTypes
      extraParams = case contentType of
        Nothing -> [contentTypeParameter contentTypes]
        Just ct -> List.map fieldParameters ct.fields
  in
     oneOf extraParams match

contentTypeParameter contentTypes =
  chain [ static "content_type"
        , static "="
        , choiceVariable (List.map .id contentTypes) "content_type"
        ]

unique items =
  List.foldr
    (\item list -> if List.member item list then list else item :: list)
    []
    items

simpleQueryParameter name =
  chain [ static name, static "=", delimitedVariable "&" name ]

typedQueryParameters : List String -> FieldType -> Matcher
typedQueryParameters path type' =
  let
      nameMatchers = List.map static path
      opMatcher = oneOf (List.map static (validOperators type'))
  in
     chain
     (nameMatchers ++ [ opMatcher
                      , delimitedVariable "&" (String.join "" path)
                      ])
 

fieldParameters : Field -> Matcher
fieldParameters field =
  typedQueryParameters ["fields.", field.id] field.type'

validOperators type' =
  "[exists]=" :: case type' of
    Text        -> [ "[match]=" ]
    Boolean     -> equalityOperators
    Location    -> [ "[near]=", "[within]=" ]
    Object      -> []
    Symbol      -> equalityOperators ++ orderingOperators
    Number      -> equalityOperators ++ orderingOperators
    Integer     -> equalityOperators ++ orderingOperators
    Date        -> equalityOperators ++ orderingOperators
    Link        -> linkOperators
    LinkArray   -> linkOperators
    SymbolArray -> equalityOperators

equalityOperators = [ "=", "[eq]=", "[ne]=", "[in]=", "[nin]=" ]
orderingOperators = [ "[gt]=", "[lt]=", "[gte]=", "[lte]=" ]
linkOperators = List.map (\op -> ".sys.id" ++ op) equalityOperators
