module Oauth (Provider, Model, init, update, view, parseHash) where

import Effects exposing (Effects)
import List
import String

import Http

import Html exposing (a, span, text)
import Html.Attributes exposing (href)


type alias AccessToken = String

type alias Provider scope = {
  name: String,
  loginUrl: String,
  scopeToString: (scope -> String),
  styles: List (String, String)
}

type Action = Maybe AccessToken

type alias Model scope = {
  provider: Provider scope ,
  token: Maybe AccessToken,
  authUrl: String
}



init provider clientId thisUrl scopes = {
    provider = provider,
    token = Nothing,
    authUrl = authUrl provider scopes clientId thisUrl
  }

authUrl : Provider scope -> List scope -> String -> String -> String
authUrl provider scopes appId redirectUrl =
  Http.url provider.loginUrl [
    ("response_type", "token"),
    ("client_id", appId),
    ("redirect_uri", redirectUrl),
    ("scope", scopes |> List.map provider.scopeToString |> String.join ",")
  ]

update : (Maybe AccessToken) -> Model s -> Model s
update newToken model =
  case newToken of
    Nothing -> model
    Just token -> { model | token = newToken }

view self {provider, token, authUrl} =
  case token of
    Just token ->
      span [] [ text ("Authenticated with " ++ provider.name) ]
    Nothing ->
      a [ href authUrl ] [ text ("Login with " ++ provider.name) ]


---- Helper functions ----


parseHash : String -> Maybe String
parseHash =
  let
    parseToken pair =
      case pair of
        "access_token" :: token :: [] -> Just token
        _ -> Nothing
  in
    String.dropLeft 1 >> String.split "&"
      >> List.map (String.split "=")
      >> List.filterMap parseToken
      >> List.head
