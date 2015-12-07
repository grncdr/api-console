module LoginForm where

import Debug

import Effects exposing (Effects)
import List
import String
import Task exposing (Task, andThen)

import Http

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, type', for, value, class, href)

import StartApp
import LocalStorage
import Location


type alias Model = {
  authUrl: String,
  token: Maybe String
}

type alias Token = Maybe String

type Action = DoNothing
            | SetToken Token
            | SetAuthUrl String

{-| A signal of tasks that represent the parsing/loading of Oauth tokens from
either the URL hash fragment or local storage
-}
hashToken : Signal Token
hashToken =
  let
    parseToken pair =
      case pair of
        "access_token" :: token :: [] -> Just token
        _ -> Nothing
    parseHash =
      String.dropLeft 1 >> String.split "&"
        >> List.map (String.split "=")
        >> List.filterMap parseToken
        >> List.head
        
  in
    Location.hash |> Signal.map parseHash 

authUrl : String -> String -> String -> Signal String
authUrl endpoint scope clientId =
  Location.url |> Signal.map (\currentUrl ->
    Http.url endpoint [
      ("response_type", "token"),
      ("client_id", clientId),
      ("redirect_uri", currentUrl),
      ("scope", scope)
    ])

update : Action -> Model -> (Model, Effects Action)
update action model =
  case Debug.watch "action" action of
    DoNothing ->
      (model, Effects.none)
    SetAuthUrl s ->
      ({ model | authUrl = s }, Effects.none)
    SetToken token ->
      case token of
        Nothing -> (model, Effects.none)
        Just it -> (
          { model | token = token },
          Effects.task (
            LocalStorage.set "token" it |> Task.toMaybe |> Task.map (\_ -> DoNothing)
          )
        )

view self {token, authUrl} =
  case token of
    Just token ->
      div [] [ text ("Using token " ++ token) ]
    Nothing ->
      a [ href authUrl ] [ text "Login with Contentful" ]

app =
  let
    loadTokenFromStorage : Task never Token
    loadTokenFromStorage =
      LocalStorage.get "token"
        |> Task.map (Debug.watch "loadTokenSuccess")
        |> Task.mapError (Debug.watch "loadTokenError")
        |> (flip Task.onError) (\x -> Task.succeed Nothing)

    clientId = "b2a4ef4588cde9be33fd6d5184bd4918c652e196408b988e72af37bee229f632"
    loginUrl = "https://be.contentful.com/oauth/authorize"
    scope = "content_management_read"
  in
    StartApp.start {
      init = (
        { authUrl = "", token = Nothing },
        Effects.task (Task.map SetToken loadTokenFromStorage)
      ),
      update = update,
      view = view,
      inputs = [
        hashToken |> Signal.map SetToken,
        authUrl loginUrl scope clientId |> Signal.map SetAuthUrl
      ]
    }

main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
      app.tasks
