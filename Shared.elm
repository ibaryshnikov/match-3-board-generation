module Shared exposing (Position, Board, Gems, Model, Msg(..))

import Dict exposing (Dict)

type alias Position = (Int, Int)
type alias Board = Dict Position String
type alias Gems = List String
type alias Model = {
    width:  Int,
    height: Int,
    board:  Board,
    gems:   Gems
}

type Msg =
 Roll |
 NewBoard Board
