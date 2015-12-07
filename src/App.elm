module App (main) where

import Debug
import Effects exposing (Effects, Never)
import Signal
import String
import Task exposing (Task)

import Html exposing (Html, div, text)
import Http
import Keyboard
import StartApp

import LocalStorage
import Location
import Matcher

import Console exposing (Action(ShiftState, MetaState))
import Oauth

import Contentful.Console
import Contentful.Data as CD
import Contentful.Routes exposing (Var(..))
import Contentful.Oauth

---- HELPERS ----

noEffect : Model -> (Model, Effects Action)
noEffect = (flip (,)) Effects.none

exec : String -> List Var -> Task Never String
exec input variables =
  let
    token = variables
      |> List.filterMap
        (\var -> case var of
          SpaceContextV ctx -> Just ctx.apiKey.token
          _ -> Nothing
        )
      |> List.head
  in
    case token of
      Nothing ->
        Task.succeed "CDA requests must include space ID and token"
      Just t ->
        Http.send Http.defaultSettings {
          verb = "GET",
          url = ("http://cdn.contentful.com" ++ String.dropLeft 4 input),
          headers = [("Authorization", "Bearer " ++ t)],
          body = Http.empty
        }
        |> Task.map (\response -> case response.value of
                        Http.Text str -> str
                        _ -> "<binary Blob>")
        |> (flip Task.onError) (\err -> Task.succeed (toString err))

resultHtml result =
  Html.pre [] [ text result ]

---- INPUTS ----

hashToken : Signal Action
hashToken = Location.hash |> Signal.map (Oauth.parseHash >> SetToken)

shiftState = Signal.map (ShiftState >> ConsoleAction) Keyboard.shift
metaState = Signal.map (MetaState >> ConsoleAction) Keyboard.meta

---- MODEL ----

type alias ConsoleResult = String

type Action =
  SetToken (Maybe String)
  | SetMatcher (Matcher.Matcher Var)
  | ConsoleAction (Console.Action Var ConsoleResult)


-- Concrete type instances for sub-components
type alias OauthModel = Oauth.Model Contentful.Oauth.Scope
type alias ConsoleModel = Console.Model Contentful.Routes.Var ConsoleResult

-- The App model is simply a tuple of each subcomponents model
type alias Model = (OauthModel, Maybe ConsoleModel)

init : Model
init =
  ( Oauth.init
      Contentful.Oauth.provider
      "b2a4ef4588cde9be33fd6d5184bd4918c652e196408b988e72af37bee229f632"
      "http://stephensugden.com/api-console.html"
      [Contentful.Oauth.ContentManagementRead]
  , Nothing
  )

---- UPDATE ----

update : Action -> Model -> (Model, Effects.Effects Action)
update action (oauth, maybeConsole) =
  case (action, maybeConsole) of
    (SetToken (Just token), _) ->
      let
        newOauth = Oauth.update (Just (Debug.watch "token" token)) oauth
        task = Contentful.Console.buildMatcher token |> Task.map SetMatcher
      in
        ((newOauth, Nothing), Effects.task task)
    (SetToken Nothing, _)  ->
      noEffect (Oauth.update Nothing oauth, Nothing)

    (SetMatcher matcher, _) ->
      noEffect (oauth, Just (Console.init matcher exec resultHtml))

    (ConsoleAction act, Just console) ->
      let (console, effect) = Console.update act console 
      in ((oauth, Just console), Effects.map ConsoleAction effect)
    (ConsoleAction _, Nothing) ->
      noEffect (oauth, Nothing)

---- VIEW ----

view : Signal.Address Action -> Model -> Html
view self (oauth, maybeConsole) =
  case maybeConsole of
    Nothing -> Oauth.view self oauth
    Just console ->
      Console.view (Signal.forwardTo self ConsoleAction) console

app = StartApp.start {
      init = (init, Effects.none),
      update = update,
      view = view,
      inputs = [hashToken, shiftState, metaState]
    }

main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks = app.tasks
