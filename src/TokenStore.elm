module TokenStore (get, set) where

key provider =
  "oauth-token-for:" ++ provider

get provider =
  LocalStorage.get (key provider)

set provider token =
  LocalStorage.set (key provider) token

parseHash : String -> (Maybe String)
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

