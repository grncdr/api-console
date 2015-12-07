module Contentful.Oauth (provider, Scope(..)) where

import Oauth exposing (Provider)

type Scope = ContentManagementRead
           | ContentManagementManage

scopeToString scope =
  case scope of
    ContentManagementRead -> "content_management_read"
    ContentManagementManage -> "content_management_manage"

provider : Provider Scope
provider = {
    name = "Contentful",
    loginUrl = "https://be.contentful.com/oauth/authorize",
    scopeToString = scopeToString,
    styles = []
  }

{-
loadTokenFromStorage : Task never (Maybe String)
loadTokenFromStorage =
  LocalStorage.get "token"
  |> Task.map (Debug.watch "loadTokenSuccess")
  |> Task.mapError (Debug.watch "loadTokenError")
  |> (flip Task.onError) (\x -> Task.succeed Nothing)
-}
