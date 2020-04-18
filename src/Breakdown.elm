module Breakdown exposing (Element(..), elementToString)


type Element
    = Location String String String
    | IgnorableLine


elementToString : Element -> String
elementToString element =
    case element of
        Location insideout place time ->
            insideout ++ " - " ++ place ++ "(" ++ time ++ ")"

        IgnorableLine ->
            ""
