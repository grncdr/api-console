module Location (url, hash) where

import Native.Location

url : Signal String
url = Native.Location.url

hash : Signal String
hash = Native.Location.hash
