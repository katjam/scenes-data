module Screenplay exposing (ScreenplayData, parseScreenplay)

import Parser exposing ((|.), (|=), Parser, chompUntil, getChompedString, keyword, oneOf, spaces, succeed)


type alias ScreenplayData =
    { title : String
    , characters : List String
    , locations : List Location
    , rawText : String
    }


type IntExt
    = Int
    | Ext


type alias Location =
    { intext : IntExt
    , description : String
    }


parseScreenplay : String -> ScreenplayData
parseScreenplay content =
    { title = "title"
    , characters = [ "" ]
    , locations = [ { intext = Int, description = "" } ]
    , rawText = content
    }



-- Page is content between newline-Integer-. and next newline-Integer-.
-- Locations is any caps following INT-. or EXT.-. to end of line
-- Character is any caps on own line that do not start with INT or EXT
