module Helpers exposing ((=>), px, exclusiveRange)

(=>): a -> b -> (a, b)
(=>) = (,)

px: Int -> String
px n = toString n ++ "px"

exclusiveRange: Int -> Int -> List Int
exclusiveRange from to = List.range from (to - 1)
