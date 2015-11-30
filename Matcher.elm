-- Defines the data-types used for matching input to completions
module Matcher ( Match, MatchPart(..), Matcher, matchString, matchPartString, matchBindings
               , static, variable, delimitedVariable, choiceVariable
               , oneOf, oneOfStatic, chain, fencePost) where

import String
import List
import Dict

type MatchPart = Static String | Variable String String

matchPartString part = 
  case part of
    Static s -> s
    Variable _ s -> s

matchString match =
  List.foldr (++) "" (List.map matchPartString match.matched)

matchBindings match =
  let
      getVariable part =
       case part of
        Variable name value -> Just (name, value)
        _ -> Nothing
  in
    List.filterMap getVariable match.matched

type alias Match = { matched: List MatchPart, unmatched: String, stop: Bool }

-- A matcher is a function from one match state to a list of potential matches
type alias Matcher = Match -> List Match

static : String -> Match -> List Match
static str state =
  case prefixCompare str state.unmatched of
    Nothing ->
      if String.isEmpty state.unmatched then
         [ { state | matched = state.matched ++ [ Static str ], stop = True } ]
      else
         []
    Just (stop, remaining) ->
      [ { state | matched = state.matched ++ [ Static str ]
                , unmatched = remaining
                , stop = stop
        }
      ]


prefixCompare prefix input =
  let
      stop = (String.length prefix) > (String.length input)
      shorter = if stop then input else prefix
      longer = if stop then prefix else input
  in
     if String.startsWith shorter longer then
        Just (stop, (String.dropLeft (String.length prefix) input))
      else
        Nothing

variable name state =
  { state | unmatched = "", matched = state.matched ++ [Variable name state.unmatched] }

choiceVariable choices name state =
  let
      nextState value (stop, unmatched) =
        { state | matched = state.matched ++ [Variable name value]
                , unmatched = unmatched
                , stop = stop }
      tryChoice value = Maybe.map (nextState value) (prefixCompare value state.unmatched)
  in
     List.filterMap tryChoice choices

delimitedVariable t name state =
  if String.isEmpty state.unmatched then
     [{ state | stop = True }]
  else
    case List.head (String.indexes t state.unmatched) of
      Nothing ->
        [ { state | unmatched = ""
                  , matched = state.matched ++ [Variable name state.unmatched]
                  , stop = True } ]
      Just i ->
        [ { state | unmatched = String.dropLeft i state.unmatched
                  , matched = state.matched ++ [ Variable name (String.slice 0 i state.unmatched) ]
                  , stop = False
          } ]

oneOf : List Matcher -> Match -> List Match
oneOf matchers state = uniq (List.concatMap (\m -> m state) matchers)

uniq : List a -> List a
uniq =
  List.foldr (\it list -> if List.member it list then list else list ++ [ it ]) []

oneOfStatic : List String -> Matcher
oneOfStatic strings = oneOf (List.map static strings)

chain : List Matcher -> Match -> List Match
chain matchers state = 
  case matchers of
    [] -> []
    -- if this is the last matcher, return it's results directly
    matcher :: [] -> matcher state
    matcher :: matchers' ->
      case matcher state of
        -- exactly one match means we can continue along the chain
        state' :: [] ->
          if state'.stop then [state'] else chain matchers' state'
        alternatives ->
          List.filter .stop alternatives

fencePost : String -> Matcher -> Matcher
fencePost sep matchOne state =
  case matchOne state of
    state' :: [] ->
      if state'.stop then
         [state']
      else
        case static sep state' of
          -- continue looking for items iff the item is followed by our separator
          state'' :: [] -> fencePost sep matchOne state''
          _             -> [state']
    -- There was an ambiguous match from matchOne, return early
    manyStates -> manyStates
