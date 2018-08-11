module Main exposing (..)

import Html exposing (Html, div, input, text, button, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html
import Base64


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = view, update = update }


type alias Model =
    { rawData : String
    , formattedData : String
    }


model : Model
model =
    Model "" ""


type Msg
    = Change String
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newData ->
            { model
                | rawData = newData
                , formattedData =
                    (case (Base64.decode newData) of
                        Ok o ->
                            o

                        Err e ->
                            ("Error: " ++ e)
                    )
            }

        Reset ->
            { model
                | rawData = ""
                , formattedData = ""
            }


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Base64 Content"
            , onInput Change
            , value model.rawData
            ]
            []
        , pre [ id "base64_result_box" ] [ Html.text model.formattedData ]
        , button [ onClick Reset ] [ text "reset" ]
        ]
