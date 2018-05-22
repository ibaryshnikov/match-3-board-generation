module Shared exposing (Model, Board, Line, Msg(..))

type alias Line = List (Maybe String)
type alias Board = List Line
type alias Model = { board: Board }

type Msg =
 Roll |
 NewBoard Board
