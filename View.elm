module View exposing (render)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Dict

import Helpers exposing ((=>), px, exclusiveRange)
import Shared exposing (Model, Board, Msg(..))

cellStyle: Html.Attribute Msg
cellStyle =
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

extractCell: Int -> Int -> Board -> String
extractCell i j board =
 case Dict.get (i, j) board of
 Just s  -> s
 Nothing -> ""

renderItem: Int -> Int -> Board -> Html Msg
renderItem i j board =
 let s = extractCell i j board
 in div [
    cellStyle,
    onClick Roll
 ] [ text s ]

renderLine: Int -> Int -> Board -> Html Msg
renderLine i width board =
 let list     = exclusiveRange 0 width
     elements = List.map (\j -> renderItem i j board) list
 in div [ style [
    "margin" => "0 auto",
    "width" => px (width * 34)
 ] ] elements

renderBoard: Model -> List (Html Msg)
renderBoard { width, height, board } =
 let list = exclusiveRange 0 height
 in List.map (\i -> renderLine i width board) list

render: Model -> Html Msg
render model =
  let elements = renderBoard model
  in div [ style [
    "-moz-user-select" => "none",
    "user-select"      => "none"
  ] ] elements
