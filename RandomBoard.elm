module RandomBoard exposing (generate)

import Array exposing (Array)
import Dict
import Random exposing (Generator)

import Helpers exposing (exclusiveRange)
import Shared exposing (Position, Board, Gems, Model, Msg(..))

filterGems: Gems -> Gems -> Gems
filterGems palette matched =
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

getSource: Gems -> Position -> Board -> Gems
getSource gems (i, j) board =
 let partial = getMatched board
     m1      = partial (i, j - 1) (i, j - 2)
     m2      = partial (i - 1, j) (i - 2, j)
     matched = List.append m1 m2
 in filterGems gems matched

generateItem: Gems -> Position -> Board -> Generator Board
generateItem gems position board =
  let source = getSource gems position board
      max    = List.length source - 1
      gen    = Random.int 0 max
  in Random.map (\i -> insertValue source i position board) gen

chainItem: Gems -> Position -> Generator Board -> Generator Board
chainItem gems position gen =
 let next board = generateItem gems position board
 in Random.andThen next gen

generateLine: Gems -> Int -> Int -> Generator Board -> Generator Board
generateLine gems i width chained =
  let list = exclusiveRange 0 width
  in List.foldl (\j -> chainItem gems (i, j)) chained list

generateBoard: Model -> Generator Board
generateBoard { width, height, gems } =
 let list    = exclusiveRange 0 height
     initial = Random.map(\x -> Dict.empty) Random.bool
 in List.foldl (\i -> generateLine gems i width) initial list

generate: Model -> Cmd Msg
generate model =
 Random.generate NewBoard (generateBoard model)
