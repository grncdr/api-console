module Contentful.Client (Config, cdaConfig, cmaConfig, getSpaces, getContentTypes, getApiKeys) where

import List
import Dict exposing (Dict)

import String
import Task exposing (Task, andThen)

import Http
import Json.Decode exposing (Decoder)

import Contentful.Data exposing (..)
import Contentful.Decode exposing (..)

type alias Config = {
  token: String,
  baseUrl: String
}

cdaConfig space token =
  { space = space, token = token, baseUrl = "https://cdn.contentful.com" }

cmaConfig token =
  { token = token, baseUrl = "https://api.contentful.com" }

send : Config -> Decoder a -> String -> List String -> List (String, String) -> Http.Body -> Task Http.Error a
send config decoder verb path query body =
  let
      request = Http.send Http.defaultSettings {
        verb = verb,
        url = Http.url ((config.baseUrl) ++ "/" ++ (String.join "/" path)) query,
        headers = [("Authorization", "Bearer " ++ config.token)],
        body = body
      }
  in
     Http.fromJson decoder request

get config decoder path = 
  send config decoder "GET" path [] Http.empty


getSpaces : Config -> Task Http.Error (List Space)
getSpaces config =
  get config (array space) ["spaces"] 

getContentTypes : Config -> String -> Task Http.Error (List ContentType)
getContentTypes config space =
  get config (array contentType) ["spaces", space, "public", "content_types"]

getApiKeys : Config -> String -> Task Http.Error (List ApiKey)
getApiKeys config space =
  get config (array apiKey) ["spaces", space, "api_keys"]
