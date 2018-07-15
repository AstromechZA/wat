module Main exposing (..)

import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Basics


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = view, update = update }


type alias Model =
    { rawData : String
    , bytes : Float
    }


model : Model
model =
    Model "0" 0


type Msg
    = Change String
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newData ->
            { model
                | rawData = newData
                , bytes = String.toLower newData
                  |> String.toFloat 
                  |> Result.toMaybe 
                  |> Maybe.withDefault 0
            }

        Reset ->
            { model
                | rawData = "0"
                , bytes = 0
            }

round : Int -> Float -> Float 
round d v = 
    let 
        x = toFloat (10 ^ d)
    in
        (toFloat (Basics.round (v * x))) / x


fmtMetric : Int -> Float -> String 
fmtMetric z i =
    i / (toFloat (1000 ^ z))
    |> round 4 
    |> toString


fmtISO : Int -> Float -> String 
fmtISO z i =
    i / (toFloat (1024 ^ z))
    |> round 4 
    |> toString


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Number of Bytes"
            , onInput Change
            , value model.rawData
            ]
            []
        , div [] [ text ((fmtMetric 0 model.bytes) ++ " B") ]
        , div [] [ text ((fmtMetric 1 model.bytes) ++ " KB") ]
        , div [] [ text ((fmtISO 1 model.bytes) ++ " KiB") ]
        , div [] [ text ((fmtMetric 2 model.bytes) ++ " MB") ]
        , div [] [ text ((fmtISO 2 model.bytes) ++ " MiB") ]
        , div [] [ text ((fmtMetric 3 model.bytes) ++ " GB") ]
        , div [] [ text ((fmtISO 3 model.bytes) ++ " GiB") ]
        , div [] [ text ((fmtMetric 4 model.bytes) ++ " TB") ]
        , div [] [ text ((fmtISO 4 model.bytes) ++ " TiB") ]
        , button [ onClick Reset ] [ text "reset" ]
        ]
