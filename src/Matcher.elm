module Matcher ( Match, MatchPart(..), Matcher, run
               , matchString, matchPartString, matchBindings
               , static, variable, delimitedVariable, choiceVariable
               , oneOf, oneOfStatic, chain, fencePost) where

{-| For suggesting completions one needs dynamic parsers that carry context for
the actual tokens/symbols of the current parse, not just grammar elements. This
module contains primitive parsers and combinators that have this property.

While I'm pretty sure I could have acheived my goals using Parsec, the various
Elm parser combinator libraries aren't super well documented and I was bored.

# Types
@docs MatchPart, Match, Matcher

# Simple parsers
@docs static, variable, delimitedVariable, choiceVariable, oneOfStatic

# Combinators
@docs oneOf, chain, fencePost

# Utils
@docs matchPartString, matchString, matchBindings
-}
import String
import List
import Dict

{-| A piece of matched text is either variable or static.
Each `Variable` has a name and a value.
-}
type MatchPart v = Static String | Variable String v

{-| Represents the current state of *one* parse of the input.
-}
type alias Match v = {
  matched: List (MatchPart v),
  unmatched: String,
  stop: Bool
}

{-| A function that takes  return a *list* of matches to represent multiple possible
completions.
-}
type alias Matcher v = Match v -> List (Match v)

{-| Function that maps the matched string into a value
-}
type alias Decoder v = String -> v

{-| Parser that matches a string exactly
-}
static : String -> Matcher a
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

{-| This internal helper does special prefix comparison.

The first element in the returned tuple represents whether the input was
shorter than the prefix, which indicates that the parse should stop here.

The second element is the matched portion of the input.

The prefix comparison is always done against the longer item (prefix or input)
but I really can't remember exactly why right now... :~(
-}
prefixCompare : String -> String -> Maybe (Bool, String)
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

makeVar decode text =
  Variable text (decode text)

{-| Match a named variable. The match is unbounded so this will eat everything
left in the input.
-}
variable decode state =
  { state
  | unmatched = ""
  , matched = state.matched ++ [ Variable state.unmatched (decode state.unmatched) ]
  }

{-| Match a variable against a list of choices. This will provide each possible
variable as a choice when the input matches more than one -}
choiceVariable : List (String, v) -> Matcher v
choiceVariable choices state =
  let
    nextState (text, value) (stop, unmatched) =
      { state | matched = state.matched ++ [Variable text value]
              , unmatched = unmatched
              , stop = stop }
    tryChoice pair = Maybe.map (nextState pair) (prefixCompare (fst pair) state.unmatched)
  in
    List.filterMap tryChoice choices

{-| Match a variable delimited by some other string. This matches all input up
to the given delimiter.
-}
delimitedVariable : String -> (String -> v) -> Matcher v
delimitedVariable t decode state =
  if String.isEmpty state.unmatched then
     [{ state | stop = True }]
  else
    let var = makeVar decode
    in
    [ case List.head (String.indexes t state.unmatched) of
      Nothing ->
        { state | unmatched = ""
                , matched = state.matched ++ [ var state.unmatched ]
                , stop = True }
      Just i ->
        let text = (String.slice 0 i state.unmatched)
        in { state | unmatched = String.dropLeft i state.unmatched
                   , matched = state.matched ++ [ var text ]
                   , stop = False }
    ]

{-| Matches against each of the given matchers, combining all the results into a
single list.
-}
oneOf : List (Matcher v) -> Matcher v
oneOf matchers state = uniq (List.concatMap (\m -> m state) matchers)

{-| Internal helper to filter a list down to it's unique items.
-}
uniq : List a -> List a
uniq =
  List.foldr (\it list -> if List.member it list then list else list ++ [ it ]) []

{-| Match against a list of static strings -}
oneOfStatic : List String -> Matcher v
oneOfStatic strings = oneOf (List.map static strings)

{-| Chain multiple matchers. Parsing will only continue to the next matcher
when the preceding parser returned exactly one result. In other words, parsing
stops exactly one step past the point the input becomes ambiguous.
-}
chain : List (Matcher v) -> Matcher v
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

{-| Match repeated occurences of the given matcher, separated by `sep`.
-}
fencePost : String -> Matcher v -> Matcher v
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

{-| Extract the matched string from a `MatchPart`
-}
matchPartString : MatchPart v -> String
matchPartString part = 
  case part of
    Static s -> s
    Variable s _ -> s

{-| This helper renders a match to a string
-}
matchString : Match v -> String
matchString match =
  List.foldr (++) "" (List.map matchPartString match.matched)

{-| Extract the variable bindings from the match
-}
matchBindings : Match v -> List v
matchBindings match =
  let
     valueOf part = case part of
         Variable text value -> Just value
         _ -> Nothing
  in
    List.filterMap valueOf match.matched

run : Matcher v -> String -> List (Match v)
run matcher input =
  matcher { unmatched = input, matched = [], stop = False }
