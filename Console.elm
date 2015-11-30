module Console (Model, main) where

import Debug
import StartApp
import Effects

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, type', for, value, class)

import Matcher exposing (Matcher, Match, MatchPart(..))
import Contentful.Data exposing (FieldType(..))
import Contentful.Routes as Routes

type Action =
  NewInput String
  | SelectNextMatch
  | SelectPreviousMatch
  | UseSelectedMatch
  | SendRequest
  | DoNothing

type alias Model =
  { input : String
  , matcher : Matcher.Matcher
  , matches : List Match
  , selectedMatch : Maybe Match
  }

emptyModel : Model
emptyModel = { input = ""
             , matches = []
             , selectedMatch = Nothing
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
  case action of
    NewInput str ->
      let
          matches = model.matcher { matched = [], unmatched = str, stop = False }
          _ = Debug.watch "Matches" (List.map (\m -> m.matched) matches)
      in
        ( { model | input = str, matches = matches, selectedMatch = Nothing }, Effects.none )
    SelectNextMatch ->
      ( { model | selectedMatch = selectNextMatch model.selectedMatch model.matches }, Effects.none )
    SelectPreviousMatch ->
      ( { model | selectedMatch = selectPreviousMatch model.selectedMatch model.matches }, Effects.none )
    UseSelectedMatch ->
      let
          model' = case model.selectedMatch of
            Nothing ->
              model
            Just m ->
              let
                  str = Matcher.matchString m
              in
                 { model
                 | input = str
                 , selectedMatch = Nothing
                 , matches = model.matcher { unmatched = str, matched = [], stop = False }
                 }
      in
         ( model', Effects.none )
    _ ->
      ( model, Effects.none )

selectNextMatch current matches =
  case current of
    Nothing -> List.head matches
    Just m ->
      let
          next l =
            case l of
              [] -> Nothing
              (m' :: ms) -> if m' == m then
                               if List.isEmpty ms then
                                  List.head matches
                               else
                                  List.head ms
                            else
                              next ms
          selectedMatch = next matches
      in
         next matches

selectPreviousMatch current matches =
  selectNextMatch current (List.reverse matches)

view self model =
  div
    []
    [ replInput self model.input
    , viewMatches model.selectedMatch model.matches
    ]

replInput self val =
  input [ type' "text"
        , value val
        , on "input" targetValue (\str -> Signal.message self (NewInput str))
        , onWithOptions
            "keyup"
            { stopPropagation = True, preventDefault = True }
            keyCode
            (codeToAction self)
        , Html.Attributes.style [("width", "100%"), ("font-size", "20px")]
        ]
        []

codeToAction self code =
  Signal.message self (if code == 38 then SelectPreviousMatch
                       else if code == 40 then SelectNextMatch
                       else if code == 13 then UseSelectedMatch
                       else DoNothing)

viewMatches selectedMatch matches =
  ul [] (List.map (\m -> viewMatch selectedMatch m) matches)

viewMatch selectedMatch match =
  let
    spans = List.map viewMatchPart match.matched
    styles = case selectedMatch of
      Nothing -> []
      Just selection ->
        if selection == match then [("background-color", "#fafada")] else []
  in
     li [ Html.Attributes.style styles ] spans

viewMatchPart part =
  case part of
    Static s -> span [] [ text s ]
    Variable _ s -> span [ Html.Attributes.style [("color", "green")] ] [ text s ]

app =
    StartApp.start
      { init = (emptyModel, Effects.none)
      , update = update
      , view = view
      , inputs = []
      }

main = app.html
