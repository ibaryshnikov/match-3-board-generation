module View exposing (render)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Dict

import Helpers exposing ((=>), px, exclusiveRange)
import Shared exposing (Position, Board, Model, Msg(..))

itemStyle: Html.Attribute Msg
itemStyle =
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
 ]

extractValue: Position -> Board -> String
extractValue position board =
 case Dict.get position board of
 Just s  -> s
 Nothing -> ""

renderItem: Position -> Board -> Html Msg
renderItem position board =
 let gem = extractValue position board
 in div [
    itemStyle,
    onClick Roll
 ] [ text gem ]

renderLine: Int -> Int -> Board -> Html Msg
renderLine i width board =
 let list     = exclusiveRange 0 width
     elements = List.map (\j -> renderItem (i, j) board) list
 in div [ style [
    "margin" => "0 auto",
    "width" => px (width * 34)
 ] ] elements

renderBoard: Model -> Html Msg
renderBoard { width, height, board } =
 let list     = exclusiveRange 0 height
     elements = List.map (\i -> renderLine i width board) list
 in div [ style [
    "-moz-user-select" => "none",
    "user-select"      => "none"
  ] ] elements

render: Model -> Html Msg
render model = renderBoard model
