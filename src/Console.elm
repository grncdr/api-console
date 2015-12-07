module Console (Model, Action(ShiftState, MetaState), init, update, view) where
{-| You type stuff in the console, and it suggests completions based on the
supplied [`Matcher`](Matcher.elm).
-}

{-
TODO:
 - Refactor history into 2 separate histories
   1. a history of executions (what currently lives here)
   2. a history of inputs (which can be edited like bash does) this can be
      encapsulated in a new ConsoleInput component that handles up/down itself
-}
import Debug

import Array exposing (Array)
import Dict
import Json.Decode exposing (customDecoder)
import Json.Encode
import Task exposing (Task, andThen)
import StartApp
import Effects exposing (Effects)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, type', for, value, class, style)

import ListHelpers exposing (..)
import Matcher exposing (MatchPart(..))

import Styles

---- MODEL ----

type Direction = Down | Up

type Action var result =
  NewInput String
  | MoveHighlight Direction
  | MoveHistory Direction
  | SelectMatch (Matcher.Match var)
  | Execute
  | RecordResult Int result
  | ShiftState Bool
  | MetaState Bool
  | DoNothing

type alias Model var result =
  { input : String
  , shiftKey : Bool
  , metaKey : Bool
  , runMatcher : String -> List (Matcher.Match var)
  , matches : List (Matcher.Match var)
  , exec : Executor var result
  , resultHtml : (result -> Html)
  , highlightedMatch : Maybe (Matcher.Match var)
  , historyPosition : Int
  , history : Array.Array (Execution var result)
  }

{-| An Executor is a user-supplied function of input & variables to a result
-}
type alias Executor var result = String -> List var -> Task Effects.Never result

{-| An execution represents a submitted command
-}
type Execution var result = Pending String (List var) | Completed String (List var) result

---- INIT ----

init : Matcher.Matcher var -> Executor var result -> (result -> Html) -> Model var result
init matcher exec resultHtml =
  { input = ""
  , shiftKey = False
  , metaKey = False
  , matches = []
  , highlightedMatch = Nothing
  , history = Array.empty
  , historyPosition = 0
  , runMatcher = Matcher.run matcher
  , exec = exec
  , resultHtml = resultHtml
  }

---- UPDATE ----

update : Action var result -> Model var result -> (Model var result, Effects (Action var result))
update action model =
  case action of
    ShiftState bool ->
      ( { model | shiftKey = bool }, Effects.none )
    MetaState bool ->
      ( { model | metaKey = bool }, Effects.none )
    NewInput str ->
      let
        matches = model.runMatcher str
        highlightedMatch = (
          Maybe.map Matcher.matchBindings model.highlightedMatch
          |> Maybe.map (\bindings -> List.filter (Matcher.matchBindings >> (==) bindings) matches)
        ) `Maybe.andThen` List.head
      in
        ( { model
          | input = str
          , matches = matches
          , highlightedMatch = highlightedMatch
          }
        , Effects.none )
    MoveHighlight direction ->
      let
        f = case direction of
          Down -> following
          Up -> preceding
      in
        ( { model | highlightedMatch = f model.highlightedMatch model.matches }, Effects.none )
    MoveHistory direction ->
      let
        i = case direction of
          Down -> model.historyPosition + 1
          Up -> model.historyPosition - 1
        i' = if i < 0
                then 0
                else if i > (Array.length model.history)
                        then Array.length model.history
                        else i
        model' = { model | historyPosition = i' }
        matches = Debug.watch "matching" (getCurrentInput model') |> model'.runMatcher
      in
        ( { model' | matches = matches }, Effects.none )
    SelectMatch match ->
      let
        str = Matcher.matchString match
      in
        ( { model | input = str
                  , highlightedMatch = Nothing
                  , matches = model.runMatcher str }
        , Effects.none
        )
    Execute ->
      let
          variables = List.head model.matches
            |> Maybe.map (Matcher.matchBindings)
            |> Maybe.withDefault []
          idx = Array.length model.history
          execution = Pending (getCurrentInput model) variables
          history = Array.push execution model.history
          line = getCurrentInput model
          task = model.exec line variables |> Task.map (RecordResult idx)
      in
        ( { model
          | history = history
          , historyPosition = idx + 1
          , input = "" 
          , matches = []
          , highlightedMatch = Nothing
          }
        , Effects.task task
        )
    RecordResult idx result ->
      let
          history = model.history
          newModel = case Array.get idx history of
            Just (Pending input vars) ->
              { model | history = Array.set idx (Completed input vars result) history }
            _ -> model
      in
         (newModel, Effects.none)
    _ ->
      ( model, Effects.none )


---- VIEW ----

view : Signal.Address (Action var result) -> Model var result -> Html
view self model =
  let
    wrapper = div 
  in
    div [ style [ ("position", "absolute")
                , ("height", "100%")
                , ("width", "100%")
                , ("display", "flex")
                , ("flex-direction", "column")
                ]
        ]
        [
          replInput self model (keyCodeToAction model)
        , viewMatches model.highlightedMatch model.matches
        , viewHistory model.resultHtml model.history
        ]


replInput self model keyCodeToAction =
  let
      actionKeys = customDecoder keyCode keyCodeToAction
      dropEvent = { stopPropagation = True, preventDefault = True }
  in
    input [ type' "text"
          , value (getCurrentInput model)
          , on "input" targetValue (NewInput >> Signal.message self)
          , onWithOptions "keydown" dropEvent actionKeys (Signal.message self)
          , onWithOptions "keyup" dropEvent actionKeys (\_ -> Signal.message self DoNothing)
          , style [Styles.fullWidth, Styles.bigText]
          ]
          []


viewMatches highlighted matches =
  div [ style [ ("max-height", "50%")
              , ("overflow-y", "scroll") ] ]
      (List.map (\m -> viewMatch m highlighted) matches)


viewMatch match highlighted =
  let
    spans = List.map viewMatchPart match.matched
    styles = Styles.padding5 :: if highlighted == (Just match) then [Styles.selected] else []
  in
     div [ style styles ] spans


viewMatchPart : (Matcher.MatchPart var) -> Html
viewMatchPart part =
  case part of
    Static s -> span [] [ text s ]
    Variable s _ -> span [ Html.Attributes.style [("color", "green")] ] [ text s ]


viewHistory : (result -> Html) -> Array (Execution var result) -> Html
viewHistory resultHtml log =
  let
    scrollTop = Debug.watch "scrollTop" (1000000 * (Array.length log))
  in
  div [ Html.Attributes.property "scrollTop" (Json.Encode.int scrollTop)
      , style [ ("flex", "1 1"), ("overflow-y", "scroll") ]
      ]
      ( log
        |> Array.indexedMap (viewExecution resultHtml)
        |> Array.toList
        |> List.reverse
      )


viewExecution : (result -> Html) -> Int -> Execution var result -> Html
viewExecution resultHtml i item =
  let
      (input, result) = case item of
        Pending input _ -> (input, pre [] [ text "<pending>" ])
        Completed input _ res -> (input, (resultHtml res))
  in
  div [ style [ Styles.padding5, Styles.topBorder ] ] [
    div [ style [ Styles.padding5 ] ] [ text ("In[" ++ (toString i) ++ "]: " ++ input) ],
    div [ style [ Styles.padding5 ] ] [ text ("Out[" ++ (toString i) ++ "]: "), result ]
  ]


---- HELPERS ----

getExecInput e =
  case e of
    Pending input _ -> input
    Completed input _ _ -> input


getCurrentInput model =
  Array.get model.historyPosition model.history
  |> Maybe.map getExecInput
  |> Maybe.withDefault model.input

keyCodeToAction model code =
  case code of
    9  -> -- Tab
      if model.shiftKey then Ok (MoveHighlight Up) else Ok (MoveHighlight Down)
    40 -> -- Down Arrow
      Ok (MoveHistory Up)
    38 -> -- Up Arrow
      Ok (MoveHistory Down)
    13 -> -- Return
      case model.highlightedMatch of
        Nothing -> Ok Execute
        Just m -> Ok (SelectMatch m)
    _ -> Err "not handling that key"
