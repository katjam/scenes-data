module Breakdown exposing (Element(..), elementToString)


type Element
    = PageNumber Float
    | Location String String String
    | IgnorableLine


elementToString : Element -> String
elementToString element =
    case element of
        PageNumber page ->
            String.fromFloat page

        Location insideout place time ->
            insideout ++ " - " ++ place ++ "(" ++ time ++ ")"

        IgnorableLine ->
            ""
