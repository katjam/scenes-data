module Breakdown exposing (Element(..), elementToString)


type Element
    = Location String String
    | Character String
    | IgnorableLine


elementToString : Element -> String
elementToString element =
    case element of
        Location insideout place ->
            insideout ++ " - " ++ place

        Character name ->
            name

        IgnorableLine ->
            ""
