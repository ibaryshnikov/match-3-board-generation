module Helpers exposing ((=>), px)

(=>): a -> b -> (a, b)
(=>) = (,)

px: Int -> String
px n = toString n ++ "px"
