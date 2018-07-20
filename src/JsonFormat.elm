module Main exposing (..)

import Html exposing (Html, div, input, text, button, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html
import Json.Decode
import Dict exposing (Dict)


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
    Model "{}" "{}"


type Msg
    = Change String
    | Reset


type InternalJson
    = JsonString String
    | JsonNumber Float
    | JsonBool Bool
    | JsonObject (Dict String InternalJson)
    | JsonList (List InternalJson)


decodeToInternalJson : Json.Decode.Decoder InternalJson
decodeToInternalJson =
    Json.Decode.oneOf
        [ Json.Decode.map JsonString Json.Decode.string
        , Json.Decode.map JsonNumber Json.Decode.float
        , Json.Decode.map JsonBool Json.Decode.bool
        , Json.Decode.map JsonObject (Json.Decode.lazy (\_ -> Json.Decode.dict decodeToInternalJson))
        , Json.Decode.map JsonList (Json.Decode.lazy (\_ -> (Json.Decode.list decodeToInternalJson)))
        ]


stringify : String -> String
stringify value =
    case Json.Decode.decodeString decodeToInternalJson value of
        Err e ->
            e

        Ok v ->
            internalJsonToString "" "  " v


internalJsonToString : String -> String -> InternalJson -> String
internalJsonToString pad pad2 json =
    case json of
        JsonString str ->
            "\"" ++ str ++ "\""

        JsonNumber num ->
            Basics.toString num

        JsonBool bool ->
            if bool then
                "true"
            else
                "false"

        JsonObject object ->
            Dict.toList object
                |> List.map (\( key, value ) -> pad2 ++ "\"" ++ key ++ "\": " ++ (internalJsonToString pad2 (pad2 ++ "  ") value))
                |> String.join ",\n"
                |> (\x -> "{\n" ++ x ++ "\n" ++ pad ++ "}")

        JsonList list ->
            List.map (internalJsonToString pad2 (pad2 ++ "  ")) list
                |> List.map (\x -> (pad2 ++ x))
                |> String.join ",\n"
                |> (\x -> "[\n" ++ x ++ "\n" ++ pad ++ "]")


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newData ->
            { model
                | rawData = newData
                , formattedData = (stringify newData)
            }

        Reset ->
            { model
                | rawData = "{}"
                , formattedData = "{}"
            }


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Json Content"
            , onInput Change
            , value model.rawData
            ]
            []
        , pre [ id "json_result_box" ] [ Html.text model.formattedData ]
        , button [ onClick Reset ] [ text "reset" ]
        ]
