module Contentful.Client (DeliveryToken, ManagementToken, url, getContentTypes) where

import Http
import Contentful.Decode exposing (array, contentType)
import String

type ManagementToken = ManagementToken String
type DeliveryToken = DeliveryToken String

type alias Config = {
  token: String,
  baseUrl: String
}

url config parts query =
  Http.url
    ((config.baseUrl) ++ (String.join "/" parts))
    (("access_token", config.token) :: query)

getContentTypes config space =
  Http.get
        (array contentType)
        (url config ["spaces", space, "content_types"] [])
