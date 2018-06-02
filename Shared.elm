module Shared exposing (Position, Board, Model, Msg(..))

import Dict exposing (Dict)

type alias Position = (Int, Int)
type alias Board = Dict Position String
type alias Model = { board: Board, width: Int, height: Int }

type Msg =
 Roll |
 NewBoard Board
