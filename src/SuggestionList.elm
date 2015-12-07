module SuggestionList where

import Matcher exposing (MatchPart(..))


type Action =
  ClearHighlight
  | UseHighlighted
  | MoveHighlight Direction
  | MoveHistory Direction

type alias Model a =
  { suggestions : List a
  , suggestionHtml : a -> Bool -> Html
  , shiftKey : Bool
