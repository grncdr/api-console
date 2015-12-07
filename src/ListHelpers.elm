module ListHelpers (following, preceding) where

import List exposing (head, reverse)

following current list =
  let
    search l = case l of
      [] -> Nothing
      some :: rest ->
        if current == (Just some) then head rest else search rest
  in
     case search list of
       Just it -> Just it
       Nothing -> List.head list
  

preceding current list =
  reverse list |> following current
