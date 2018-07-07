import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String

main : Program Never Model Msg
main =
  Html.beginnerProgram
    { model = model, view = view, update = update}

type alias Model =
  { 
      rawData : String,
      bytes : Int
  }

model : Model
model =
  Model "0" 0

type Msg
  = Change String | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newData ->
      { model | 
        rawData = newData,
        bytes = (String.toInt (String.toLower newData)) |> Result.toMaybe |> Maybe.withDefault 0
      }
    Reset -> 
      { model | 
        rawData = "0",
        bytes = 0
      }

view : Model -> Html Msg
view model =
  div []
    [ 
        input [ 
            placeholder "Number of Bytes", 
            onInput Change,
            value model.rawData
        ] [], 
        div [] [ text ((toString model.bytes) ++ " B") ],
        div [] [ text ((toString (model.bytes // (1000 ^ 1))) ++ " KB") ],
        div [] [ text ((toString (model.bytes // (1024 ^ 1))) ++ " KiB") ],
        div [] [ text ((toString (model.bytes // (1000 ^ 2))) ++ " MB") ],
        div [] [ text ((toString (model.bytes // (1024 ^ 2))) ++ " MiB") ],
        div [] [ text ((toString (model.bytes // (1000 ^ 3))) ++ " GB") ],
        div [] [ text ((toString (model.bytes // (1024 ^ 3))) ++ " GiB") ],
        div [] [ text ((toString (model.bytes // (1000 ^ 4))) ++ " TB") ],
        div [] [ text ((toString (model.bytes // (1024 ^ 4))) ++ " TiB") ],
        button [ onClick Reset ] [ text "reset"]
    ]
