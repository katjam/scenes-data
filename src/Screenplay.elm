module Screenplay exposing (ScreenplayData, parseScreenplay)

import Breakdown
import Parser exposing ((|.), (|=), Parser, chompUntil, chompWhile, end, getChompedString, keyword, loop, oneOf, spaces, succeed, symbol)


type alias ScreenplayData =
    { title : String
    , lines : Result (List Parser.DeadEnd) (List Breakdown.Element)
    , rawText : String
    }


parseScreenplay : String -> ScreenplayData
parseScreenplay content =
    { title = "title"
    , lines = Parser.run list content
    , rawText = content
    }


step : List Breakdown.Element -> Parser (Parser.Step (List Breakdown.Element) (List Breakdown.Element))
step lines =
    let
        finish line next =
            next (line :: lines)
    in
    succeed finish
        |= value
        |= oneOf
            [ succeed Parser.Loop
                |. symbol "\n"
            , succeed (Parser.Done << List.reverse)
                |. end
            ]


list : Parser (List Breakdown.Element)
list =
    loop [] step



-- Title always the 1st line
-- Location starts with INT. or EXT.
-- Page starts with 1. (int + fullstop)


value : Parser Breakdown.Element
value =
    oneOf
        [ locationLine
        , ignorableLine
        ]


locationLine : Parser Breakdown.Element
locationLine =
    Parser.succeed Breakdown.Location
        |= oneOf
            [ succeed "INT"
                |. keyword "INT."
            , succeed "EXT"
                |. keyword "EXT."
            ]
        |. spaces
        -- Todo better time parser sometimes multiple dashes
        |= zeroOrMore (not << isDash)
        |. symbol "-"
        |= zeroOrMore (not << isNewLine)


ignorableLine : Parser Breakdown.Element
ignorableLine =
    Parser.succeed Breakdown.IgnorableLine
        |. blank (not << isNewLine)


zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    succeed ()
        |. chompWhile isOk
        |> getChompedString


blank : (Char -> Bool) -> Parser ()
blank isOk =
    succeed ()
        |. chompWhile isOk


isDash : Char -> Bool
isDash char =
    char == '-'


isNewLine : Char -> Bool
isNewLine char =
    char == '\n'



-- Title is the first line (maybe later until 'by')
-- Page is content between newline-Integer-. and next newline-Integer-.
-- Locations is any caps following INT-. or EXT.-. to end of line
-- Character is any caps on own line that do not start with INT or EXT
