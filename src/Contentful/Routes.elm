module Contentful.Routes (MatcherParams, Var(..), deliveryAPI) where

import Dict exposing (Dict)
import Matcher exposing (..)
import Contentful.Data exposing (..)
import String


type alias SpaceContext = {
  space: Space,
  contentTypes: List ContentType,
  apiKey: ApiKey
}

type alias MatcherParams = Dict String SpaceContext

-- Variable values we can parse from the URL
type Var = SpaceContextV SpaceContext
         | ContentTypeV ContentType
         | StringV String String -- Plain string variables have a name and value

pathVariable name = chain [ static "/", delimitedVariable "/" name ]

spaceChoices spaces =
  Dict.toList spaces |> List.map (\(id, ctx) -> (id, SpaceContextV ctx))

first : (a -> Maybe b) -> List a -> Maybe b
first f list =
  List.head (List.filterMap f list)

spaceContext =
  matchBindings >> first (\var ->
    case var of
      SpaceContextV ctx -> Just ctx
      _ -> Nothing
  )

contentTypeContext =
  matchBindings >> first (\var ->
    case var of
      ContentTypeV ct -> Just ct
      _ -> Nothing
  )

deliveryAPI : MatcherParams -> Matcher Var
deliveryAPI spaces =
  chain [ static "GET /spaces/"
        , choiceVariable (spaceChoices spaces)
        , oneOf [ chain [ static "/entries"
                        , oneOf [ pathVariable (StringV "entry"), entryQueryParameters_ ]
                        ]
                , chain [ static "/assets"
                        , oneOf [ pathVariable (StringV "asset"), assetQueryParameters ]
                        ]
                , chain [ static "/content_types"
                        , oneOf [ pathVariable (StringV "contentType"), contentTypeQueryParameters ]
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
    (commonQueryParameters ++ [ fieldQueryParameters { id = "title", type' = Text }
                              , fieldQueryParameters { id = "description", type' = Text }
                              ]
    )

entryQueryParameters_ : Matcher Var
entryQueryParameters_ match =
  let
    contentTypes = spaceContext match |> Maybe.map .contentTypes |> Maybe.withDefault []
  in
    queryParameters (dynamicEntryQueryParameter contentTypes :: commonQueryParameters) match

entryQueryParameters contentTypes =
  queryParameters (dynamicEntryQueryParameter contentTypes :: commonQueryParameters)

dynamicEntryQueryParameter : List ContentType -> Matcher Var
dynamicEntryQueryParameter contentTypes match =
  let
    matchers = case contentTypeContext match of
      Nothing -> [contentTypeQueryParameter contentTypes]
      Just ct -> [] --List.map fieldQueryParameters ct.fields
  in
    oneOf matchers match

contentTypeQueryParameter : List ContentType -> Matcher Var
contentTypeQueryParameter contentTypes =
  chain [ static "content_type="
        , choiceVariable (List.map (\ct -> (ct.id, ContentTypeV ct)) contentTypes)
        ]

unique items =
  List.foldr
    (\item list -> if List.member item list then list else item :: list)
    []
    items

simpleQueryParameter name =
  chain [ static name, static "=", delimitedVariable "&" (StringV name) ]

typedQueryParameters : List String -> FieldType -> Matcher Var
typedQueryParameters path type' =
  let
      nameMatchers = List.map static path
      opMatcher = oneOf (List.map static (validOperators type'))
  in
     chain
     (nameMatchers ++ [ opMatcher
                      , delimitedVariable "&" (StringV (String.join "" path))
                      ])
 

fieldQueryParameters : Field -> Matcher Var
fieldQueryParameters field =
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
