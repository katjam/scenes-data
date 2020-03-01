module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = viewDocument
        }


type alias Model =
    { title : String
    , characters : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { title = ""
      , characters = [ "" ]
      }
    , Cmd.none
    )


type Msg
    = Loaded


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded ->
            ( { model | title = "Parse top line" }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Scene data", body = [ view model ] }


view : Model -> Html Msg
view model =
    div []
        [ if model.title == "" then
            div [] [ text "Input txt file" ]

          else
            h2 [] [ text model.title ]
        , h2 [] [ text "Scene locations" ]
        , div [] [ text "INFO" ]
        , h2 [] [ text "Character Names" ]
        , div [] [ text "INFO" ]
        ]
