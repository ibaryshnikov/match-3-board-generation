module View exposing (line)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List

import Helpers exposing ((=>), px)
import Shared exposing (Msg(..))

item: String -> Html Msg
item value = div [
    style [
        "display" => "inline-block",
        "border" => "1px solid black",
        "width" => px 30,
        "height" => px 30,
        "line-height" => px 30,
        "text-align" => "center",
        "cursor" => "pointer",
        "margin" => px 1,
        "background" => "yellow",
        "color" => "green"
    ],
    onClick Roll
 ] [ text value ]

line: List String -> Html Msg
line list = div [ style [
    "margin" => "0 auto",
    "width" => px ((List.length list) * 34)
 ] ] <| List.map item list
