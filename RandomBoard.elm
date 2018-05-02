module RandomBoard exposing (generate)

import Maybe
import List exposing (append)
import Array exposing (Array)
import Random exposing (Generator, andThen)

import Shared exposing (Model, Board, Line, Msg(..))

type alias Palette = List String
type alias Matched = List String
type alias PairMaybe = (Maybe String, Maybe String)
type alias ListMaybe = List (Maybe String)
type alias PairListMaybe = (ListMaybe, ListMaybe)

empty: Maybe a
empty = Nothing

emptyPair: (Maybe a, Maybe a)
emptyPair = (Nothing, Nothing)

checkAgainstMatched: String -> Matched -> Bool
checkAgainstMatched i matched =
 List.all (\m -> i /= m) matched

defaultPalette: Palette
defaultPalette = ["a", "b", "c", "d", "e"]

getPalette: Palette -> Matched -> Palette
getPalette palette matched =
  List.filter (\i -> checkAgainstMatched i matched) palette 

push: List a -> a -> List a
push list x = append list [x]

extractSourceItem: Palette -> Int -> String
extractSourceItem source i =
 let a = Array.fromList source
     s = Array.get i a
 in case s of
 Just value -> value
 Nothing -> "a" -- terrible hack to not pass monad everywhere

generateFromSource: Palette -> Generator String
generateFromSource source =
 Random.map (\i -> extractSourceItem source i)
  <| Random.int 0
  <| (List.length source) - 1

unwrapPair: PairMaybe -> List String
unwrapPair prev = case prev of
 (Just i1, Just i2) -> if i1 == i2 then [i1] else []
 _ -> []

getMatched: PairMaybe -> PairMaybe -> Matched
getMatched top left =
  let l1 = unwrapPair top
      l2 = unwrapPair left
  in List.append l1 l2

gg: PairMaybe -> (PairMaybe, Line) -> Generator (PairMaybe, Line)
gg pair ((f, s), list) =
 let source = getPalette defaultPalette (getMatched pair (f, s))
 in
  Random.map (\x -> ((s, Just x), push list x))
   <| generateFromSource source

processItem: PairMaybe -> Generator (PairMaybe, Line)
 -> Generator (PairMaybe, Line)
processItem pair gen =
 andThen (gg pair) gen

getEmptyLines: PairListMaybe
getEmptyLines = (List.repeat 10 empty, List.repeat 10 empty)

toMonadList: Line -> ListMaybe
toMonadList list = List.map (\a -> Just a) list

generateLine: PairListMaybe -> Generator Line
generateLine (line1, line2) =
  Random.map (\(prev, line) -> line)
  <| List.foldl processItem
  (Random.map (\x -> (emptyPair, [])) Random.bool)
  <| List.map2 (,) line1 line2

generateBoard: Int -> PairListMaybe -> Board -> Generator Board
generateBoard length (first, second) board = case length of
 10 -> Random.map (\x -> board) Random.bool
 _  -> andThen (\x -> x)
  <| Random.map (\line -> generateBoard (length + 1)
  (second, toMonadList line) (push board line))
  <| generateLine (first, second)

generate: Model -> Cmd Msg
generate model =
 Random.generate NewList (generateBoard 0 getEmptyLines [])
