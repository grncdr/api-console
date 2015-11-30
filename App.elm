type Matcher = String -> 
getCompletions : List Matcher -> String -> List Console.Completion
getCompletions matchers input =
  let
     segments = List.filter (not String.isEmpty) (String.split "/" input)
  in



app =
    StartApp.start
      { init = (emptyModel, Effects.none)
      , update = update
      , view = view
      , inputs = []
      }

main = app.html

