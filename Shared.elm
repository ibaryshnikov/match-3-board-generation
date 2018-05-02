module Shared exposing (Model, Board, Line, Msg(..))

type alias Line = List String
type alias Board = List Line
type alias Model = { list: Board }

type Msg =
 Roll |
 NewList Board
