module Shared exposing (Model, Board, Line, Cell, Msg(..))

type alias Cell = Maybe String
type alias Line = List Cell
type alias Board = List Line
type alias Model = { board: Board, width: Int, height: Int }

type Msg =
 Roll |
 NewBoard Board
