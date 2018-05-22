module View exposing (line)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List

import Helpers exposing ((=>), px)
import Shared exposing (Line, Cell, Msg(..))

type StyleType = Red | Pink | Blue | Green

extractStyle: String -> Html.Attribute msg
extractStyle color =
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
    "color" => color
 ]

getStyle: StyleType -> Html.Attribute msg
getStyle styleType = extractStyle (case styleType of
 Red   -> "red"
 Pink  -> "pink"
 Blue  -> "blue"
 Green -> "green")

extractValue: Cell -> String
extractValue s = case s of
    Just a -> a
    Nothing -> "0"

item: Cell -> Html Msg
item value = div [
    getStyle Green,
    onClick Roll
 ] [ text (extractValue value) ]

line: Line -> Html Msg
line list = div [ style [
    "margin" => "0 auto",
    "width" => px ((List.length list) * 34)
 ] ] <| List.map item list
