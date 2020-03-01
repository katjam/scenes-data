module Screenplay exposing (ScreenplayData, parseScreenplay)


type alias ScreenplayData =
    { title : String
    , characters : List String
    , locations : List String
    , text : String
    }


parseScreenplay : String -> ScreenplayData
parseScreenplay content =
    { title = "title"
    , characters = [ "" ]
    , locations = [ "" ]
    , text = content
    }
