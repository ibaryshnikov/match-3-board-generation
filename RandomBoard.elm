module RandomBoard exposing (generate)

import Array exposing (Array)
import Dict
import Random exposing (Generator)

import Helpers exposing (exclusiveRange)
import Shared exposing (Position, Board, Model, Msg(..))

type alias Gems = List String

defaultPalette: Gems
defaultPalette = ["a", "b", "c", "d", "e"]

getPalette: Gems -> Gems -> Gems
getPalette palette matched =
  List.filter (\i -> not <| List.member i matched) palette 

insertValue: Gems -> Int -> Position -> Board -> Board
insertValue source index position board =
 let array = Array.fromList source
 in case Array.get index array of
  Just value -> Dict.insert position value board
  Nothing    -> board

getMatched: Board -> Position -> Position -> Gems
getMatched board p1 p2 =
 let first  = Dict.get p1 board
     second = Dict.get p2 board
 in case (first, second) of
 (Just v1, Just v2) -> if v1 == v2 then [v1] else []
 _ -> []

getSource: Position -> Board -> Gems
getSource (i, j) board =
 let partial = getMatched board
     m1      = partial (i, j - 1) (i, j - 2)
     m2      = partial (i - 1, j) (i - 2, j)
     matched = List.append m1 m2
 in getPalette defaultPalette matched

generateItem: Position -> Board -> Generator Board
generateItem position board =
  let source = getSource position board
      max    = List.length source - 1
      gen    = Random.int 0 max
  in Random.map (\i -> insertValue source i position board) gen

genItem: Position -> Generator Board -> Generator Board
genItem position gen =
 let next board = generateItem position board
 in Random.andThen next gen

generateLine: Int -> Int -> Generator Board -> Generator Board
generateLine i width chained =
  let list = exclusiveRange 0 width
  in List.foldl (\j -> genItem (i, j)) chained list

generateBoard: Int -> Int -> Generator Board
generateBoard width height =
 let list    = exclusiveRange 0 height
     initial = Random.map(\x -> Dict.empty) Random.bool
 in List.foldl (\i -> generateLine i width) initial list

generate: Model -> Cmd Msg
generate { width, height } =
 Random.generate NewBoard <| generateBoard width height
