module Screenplay exposing (ScreenplayData, parseScreenplay)

import Parser exposing ((|.), (|=), Parser, chompUntil, chompWhile, end, getChompedString, keyword, loop, oneOf, spaces, succeed, symbol)


type alias ScreenplayData =
    { title : String
    , characters : List String
    , locations : List String
    , lines : Result (List Parser.DeadEnd) (List String)
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
    , locations = [ "" ]
    , lines = Parser.run list content
    , rawText = content
    }


step : List String -> Parser (Parser.Step (List String) (List String))
step lines =
    let
        finish line next =
            next (line :: lines)
    in
    succeed finish
        --|. keyword "INT."
        --|. spaces
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



-- Title is the first line (maybe later until 'by')
-- Page is content between newline-Integer-. and next newline-Integer-.
-- Locations is any caps following INT-. or EXT.-. to end of line
-- Character is any caps on own line that do not start with INT or EXT
