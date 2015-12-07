module LoginForm2 where

import Debug
import Task exposing (Task)

import Effects
import StartApp
import LocalStorage
import Location

import Oauth
import Contentful.Oauth

type Action = DoNothing
            | SetToken (Maybe String)
            | SetAuthUrl String

type alias Model = {
  oauth: Ouath.Model Contentful.Oauth.Scope
}

update : Action -> Model -> (Model, Effects.Effects)
update action model =
  case action of
    DoNothing -> (model, Effects.none)
    SetToken token -> (
      { model | oauth = Oauth.update (Just token) },
      Effects.none -- TokenStore.put model.oauth.provider.name t 
    )


app =
  let
    loadTokenFromStorage : Task never (Maybe String)
    loadTokenFromStorage =
      LocalStorage.get "token"
        |> Task.map (Debug.watch "loadTokenSuccess")
        |> Task.mapError (Debug.watch "loadTokenError")
        |> (flip Task.onError) (\x -> Task.succeed Nothing)

    oauthModel =
      Oauth.init
        Contentful.Oauth.provider
        "b2a4ef4588cde9be33fd6d5184bd4918c652e196408b988e72af37bee229f632"
        "http://localhost:8000/src/LoginForm2.elm"
        [Contentful.Oauth.ContentManagementRead]
  in
    StartApp.start {
      init = (
        { oauth = oauthModel },
        Effects.task (Task.map SetToken loadTokenFromStorage)
      ),
      update = update,
      view = view,
      inputs = [
        hashToken |> Signal.map SetToken,
        Signal.map (authUrl loginUrl scope clientId) Location.url |> Signal.map SetAuthUrl
      ]
    }

main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
      app.tasks
