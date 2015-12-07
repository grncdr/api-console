module Contentful.Console (buildMatcher) where
{-| This module contains helper functions for binding Contentful.Routes and
Contentful.Client to the interfaces required by Console.
-}

import Dict exposing (Dict)
import List
import Task exposing (Task, andThen, onError)

import Http

import Contentful.Data exposing (..)
import Contentful.Client as Client
import Contentful.Routes as Routes

hardcodedMatcher = Routes.deliveryAPI <|
  Dict.singleton "cfexampleapi" { space = { id = "cfexampleapi"
                                          , name = "Example Space"
                                          }
                                , contentTypes = []
                                , apiKey = { id = "cfexamplekey"
                                           , name = "Example Key"
                                           , token = "b4c0n73n7fu1"
                                           }
                                }

buildMatcher token =
  Client.cmaConfig token
    |> getMatcherParameters
    |> Task.map Routes.deliveryAPI
    |> (flip Task.onError) (\_ -> Task.succeed hardcodedMatcher)

getMatcherParameters : Client.Config -> Task Http.Error Routes.MatcherParams
getMatcherParameters config =
  let
    getOne space =
      Task.mapError (Debug.watch "apiKeys error") (Client.getApiKeys config space.id
        `onError` \_ -> Task.succeed []
      )
        `andThen` \keys -> case List.head keys of
          Nothing -> Task.succeed Nothing
          Just apiKey ->
            Client.getContentTypes config space.id `andThen` \types ->
              Task.succeed (Just (space.id, {
                space = space,
                contentTypes = types,
                apiKey = apiKey
              }))
  in
    Client.getSpaces config `andThen` (List.map getOne >> Task.sequence)
      |> Task.map (List.filterMap identity >> Dict.fromList)

