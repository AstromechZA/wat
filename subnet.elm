module Main exposing (..)

import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, div, input, text, button)
import String
import Result
import List
import Basics
import Bitwise


type alias CIDR =
    { anchorIP : Int
    , maskShorthand : Int
    }


parseIpSegment : String -> Result String Int
parseIpSegment i =
    case String.toInt i of
        Err e ->
            Err e

        Ok v ->
            if 0 <= v && v <= 255 then
                Ok v
            else
                Err "value not in range 0-255"


isError : ( Int, Result String Int ) -> Bool
isError ( i, r ) =
    case r of
        Err _ ->
            True

        Ok _ ->
            False



-- convert ip like 1.2.3.4 into the 32bit int


ipStringToInt : String -> Result String Int
ipStringToInt i =
    let
        indexedPartResults =
            String.split "." i
                |> List.map parseIpSegment
                |> List.indexedMap (,)

        numParts =
            List.length indexedPartResults
    in
        if numParts /= 4 then
            Err "does not contain 4 dot-separated parts"
        else
            case List.head (List.filter isError indexedPartResults) of
                Just ( index, j ) ->
                    Err
                        ("part "
                            ++ (Basics.toString (index + 1))
                            ++ ": "
                            ++ (case j of
                                    Err e ->
                                        e

                                    Ok _ ->
                                        "unknown failure"
                               )
                        )

                Nothing ->
                    indexedPartResults
                        -- unwrap results (default to 0 but this should never happen)
                        |> List.map (\( a, b ) -> ( a, (Result.withDefault 0 b) ))
                        -- convert index into bitshift value
                        |> List.map (\( a, b ) -> ( (numParts - 1 - a), b ))
                        -- convert value item by shifting it into the power of 2 space
                        |> List.map (\( a, b ) -> (b * (256 ^ a)))
                        |> List.sum
                        |> Ok



-- convert subnet like 1.2.3.4/32 into ip int and subnet int


subnetStringToCIDR : String -> Result String CIDR
subnetStringToCIDR i =
    let
        parts =
            String.split "/" i

        numParts =
            List.length parts
    in
        if numParts /= 2 then
            Err "must contain exactly one /"
        else
            let
                ipResult =
                    List.head parts
                        |> Maybe.withDefault ""
                        |> ipStringToInt

                netResult =
                    List.tail parts
                        |> Maybe.withDefault []
                        |> List.head
                        |> Maybe.withDefault ""
                        |> String.toInt
            in
                case ( ipResult, netResult ) of
                    ( Ok r, Ok s ) ->
                        if s >= 0 && s <= 32 then
                            Ok (CIDR r s)
                        else
                            Err "mask value is out of range 0-32"

                    ( Err e, _ ) ->
                        Err ("could not parse ip value: " ++ e)

                    ( _, Err e ) ->
                        Err ("could not parse mask value: " ++ e)


ipIntToString : Int -> Result String String
ipIntToString i =
    if i < 0 || i > 4294967295 then
        Err "out of range"
    else
        List.range 0 3
            |> List.map (\x -> (Bitwise.shiftRightZfBy (x * 8) i) % 256)
            |> List.reverse
            |> List.map Basics.toString
            |> String.join "."
            |> Ok


maskShorthandToMaskInt : Int -> Int
maskShorthandToMaskInt i =
    4294967295 - ((2 ^ (32 - i)) - 1)


maskShorthandToSize : Int -> Int
maskShorthandToSize i =
    2 ^ (32 - i)


cidrToLowerBound : CIDR -> Int
cidrToLowerBound c =
    Bitwise.and c.anchorIP (maskShorthandToMaskInt c.maskShorthand)


cidrToUpperBound : CIDR -> Int
cidrToUpperBound c =
    (cidrToLowerBound c) + (cidrToNumAddresses c)


cidrToNumAddresses : CIDR -> Int
cidrToNumAddresses c =
    maskShorthandToSize c.maskShorthand



--Bitwise.and (Bitwise.shiftLeftBy (32 - i) 4294967295) 4294967295


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = view, update = update }


type alias Model =
    { rawData : String }


model : Model
model =
    Model ""


type Msg
    = Change String
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newData ->
            { model | rawData = newData }

        Reset ->
            { model | rawData = "" }


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "IP address"
            , onInput Change
            , value model.rawData
            ]
            []
        , div []
            [ case subnetStringToCIDR model.rawData of
                Err e ->
                    text ("Error: " ++ e)

                Ok c ->
                    text
                        ((Basics.toString c.anchorIP)
                            ++ " "
                            ++ (Basics.toString (ipIntToString (maskShorthandToMaskInt c.maskShorthand)))
                            ++ " "
                            ++ (Basics.toString (ipIntToString c.anchorIP))
                            ++ " "
                            ++ (Basics.toString (ipIntToString (cidrToLowerBound c)))
                            ++ " "
                            ++ (Basics.toString (ipIntToString (cidrToUpperBound c)))
                            ++ " "
                            ++ (Basics.toString (cidrToNumAddresses c))
                        )
            ]
        , button [ onClick Reset ] [ text "reset" ]
        ]
