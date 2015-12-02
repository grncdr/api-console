module Console (Model, main) where
{-| The console let's you type stuff, and it suggests completions based on a
supplied [`Matcher`](Matcher.elm). It doesn't execute any actions yet because
I'm super lazy.
-}

import Debug
import Dict
import Json.Decode exposing (customDecoder)
import Keyboard
import Maybe exposing (andThen, withDefault)
import StartApp
import Effects

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, type', for, value, class)

import Matcher exposing (MatchPart(..))
import Contentful.Data exposing (FieldType(..))
import Contentful.Routes as Routes

type Action =
  NewInput String
  | SelectNextMatch
  | SelectPreviousMatch
  | UseSelected Matcher.Match
  | SendRequest
  | ShiftState Bool
  | MetaState Bool
  | DoNothing

type alias Model =
  { input : String
  , shift : Bool
  , meta : Bool
  , matcher : Matcher.Matcher
  , matches : List Matcher.Match
  , selectedIndex : Maybe Int
  , history : List { input: String }
  }

emptyModel : Model
emptyModel = { input = ""
             , shift = False
             , meta = False
             , matches = []
             , selectedIndex = Nothing
             , history = []
             , matcher = Routes.deliveryAPI [ { id = "post"
                                              , fields = [ { id = "title" , type' = Symbol }
                                                         , { id = "body", type' = Text }
                                                         ]
                                              }
                                            , { id = "gallery"
                                              , fields = [ { id = "name" , type' = Symbol }
                                                         , { id = "blahblah", type' = Text }
                                                         ]
                                              }
                                            ]
             }

update action model =
  let matchCount = List.length model.matches
      selectNext = defaultMap -1 (nextIndex 1 matchCount)
      selectPrevious = defaultMap 0 (nextIndex -1 matchCount)
  in
  case action of
    ShiftState bool ->
      ( { model | shift = bool }, Effects.none )
    MetaState bool ->
      ( { model | meta = bool }, Effects.none )
    NewInput str ->
      let
          matches = model.matcher { matched = [], unmatched = str, stop = False }
          _ = Debug.watch "Matches" (List.map (\m -> m.matched) matches)
      in
        ( { model | input = str, matches = matches, selectedIndex = Nothing }, Effects.none )
    SelectNextMatch ->
      ( { model | selectedIndex = Just <| selectNext model.selectedIndex }, Effects.none )
    SelectPreviousMatch ->
      ( { model | selectedIndex = Just <| selectPrevious model.selectedIndex }, Effects.none )
    UseSelected match ->
      let
          str = Matcher.matchString match
      in
         ( { model
           | input = str
           , selectedIndex = Nothing
           , matches = model.matcher { unmatched = str, matched = [], stop = False }
           }
         , Effects.none
         )
    SendRequest ->
      ( { model
        | history = model.history ++ [{ input = model.input }]
        , input = ""
        , matches = []
        , selectedIndex = Nothing
        }
      , Effects.none
      )
    _ ->
      ( model, Effects.none )


defaultMap : a -> (a -> b) -> Maybe a -> b
defaultMap d f m =
  case m of
    Nothing -> f d
    Just x -> f x

nextIndex step len i = (i + step) % len

boundedIndex i len =
  if i < 0 then len + i else if i > len then i - len else i

nth n list =
  if n == 0
     then List.head list
     else nth (n - 1) list

view self model =
  let selectedIndex = case model.selectedIndex of
        Nothing -> -1
        Just i -> i
      
      keyCodeToAction code =
        case code of
          9  ->                     -- Tab
            if model.shift then Ok SelectPreviousMatch else Ok SelectNextMatch
          40 -> Ok SelectNextMatch     -- Down Arrow
          38 -> Ok SelectPreviousMatch -- Up Arrow
          13 ->
            case model.selectedIndex of
              Nothing -> Ok SendRequest
              Just i -> 
                case nth i model.matches of
                  Nothing -> Err "selection is invalid"
                  Just m -> Ok (UseSelected m)
          _ -> Err "not handling that key"
  in
    div
      []
      [ viewHistory model.history
      , replInput self model.input keyCodeToAction
      , viewMatches selectedIndex model.matches
      ]

viewHistory log =
  ul [] (List.map (\it -> li [] [ text it.input ]) log)

replInput self val keyCodeToAction =
  let
      actionKeys = customDecoder keyCode keyCodeToAction
      dropEvent = { stopPropagation = True, preventDefault = True }
  in
    input [ type' "text"
          , value val
          , on "input" targetValue (\str -> Signal.message self (NewInput str))
          , onWithOptions "keydown" dropEvent actionKeys (Signal.message self)
          , onWithOptions "keyup" dropEvent actionKeys (\_ -> Signal.message self DoNothing)
          , Html.Attributes.style [("width", "100%"), ("font-size", "20px")]
          ]
          []

dropKeydown self code = Signal.message self DoNothing

viewMatches selectedIndex matches =
  div [] (List.indexedMap (\i m -> viewMatch m (i == selectedIndex)) matches)

viewMatch match isSelected =
  let
    spans = List.map viewMatchPart match.matched
    styles = [("padding", "5px")] ++ if isSelected then [("background-color", "#fafada")] else []
  in
     div [ Html.Attributes.style styles ] spans

viewMatchPart part =
  case part of
    Static s -> span [] [ text s ]
    Variable _ s -> span [ Html.Attributes.style [("color", "green")] ] [ text s ]

app =
    StartApp.start
      { init = (emptyModel, Effects.none)
      , update = update
      , view = view
      , inputs = [ Signal.map ShiftState Keyboard.shift
                 , Signal.map MetaState Keyboard.meta
                 ]
      }

main = app.html
