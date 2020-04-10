module Screenplay exposing (ScreenplayData, parseScreenplay)

import Parser exposing ((|.), (|=), Parser, chompUntil, chompWhile, end, getChompedString, keyword, loop, oneOf, spaces, succeed, symbol)


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


step : List String -> Parser (Parser.Step (List String) (List String))
step locations =
    let
        finish location next =
            next (location :: locations)
    in
    succeed finish
        |. keyword "INT"
        |. symbol "."
        |. spaces
        |= zeroOrMore (not << isNewLine)
        |= oneOf
            [ succeed Parser.Loop
                |. symbol "\n"
            , succeed (Parser.Done << List.reverse)
                |. end
            ]


list : Parser (List String)
list =
    loop [] step


zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    succeed ()
        |. chompWhile isOk
        |> getChompedString


isNewLine : Char -> Bool
isNewLine char =
    char == '\n'



-- Page is content between newline-Integer-. and next newline-Integer-.
-- Locations is any caps following INT-. or EXT.-. to end of line
-- Character is any caps on own line that do not start with INT or EXT
