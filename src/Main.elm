module Main exposing (main)

import Browser
import File
import File.Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Screenplay
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = viewDocument
        }


type alias Model =
    { screenplay : Maybe Screenplay.ScreenplayData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing
    , Cmd.none
    )


type Msg
    = ScreenplayRequested
    | ScreenplaySelected File.File
    | ScreenplayLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScreenplayRequested ->
            ( model
            , File.Select.file [ "text/csv" ] ScreenplaySelected
            )

        ScreenplaySelected file ->
            ( model
            , Task.perform ScreenplayLoaded (File.toString file)
            )

        ScreenplayLoaded content ->
            ( { model | screenplay = Just (Screenplay.parseScreenplay content) }
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
    case model.screenplay of
        Nothing ->
            button [ onClick ScreenplayRequested ] [ text "Load Screenplay" ]

        Just screenplayData ->
            div []
                [ p [] [ text screenplayData.title ]
                , h2 [] [ text "Locations" ]
                , h2 [] [ text "Characters" ]
                , p [ style "white-space" "pre" ] [ text screenplayData.text ]
                ]
