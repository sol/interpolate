module Data.String.Interpolate.Parse where

import           Data.String.Interpolate.Util

data Node = Literal String | Expression String

parseNodes :: String -> [Node]
parseNodes = go ""
  where
    go :: String -> String -> [Node]
    go acc input = case input of
      ""  -> [(lit . reverse) acc]
      '#':'{':xs -> case span (/= '}') xs of
        (ys, _:zs) -> (lit . reverse) acc : Expression ys : go "" zs
        (_, "") -> [(Literal . unescape) (reverse acc ++ input)]
      x:xs -> go (x:acc) xs

    lit :: String -> Node
    lit acc = (Literal . unescape) acc
