module Data.String.Interpolate.Parse where

import           Data.String.Interpolate.Internal.Util

data Node = Literal String | Expression String

parseNodes :: String -> [Node]
parseNodes = go ""
  where
    go :: String -> String -> [Node]
    go acc input = case input of
      ""  -> lit []
      '\\':x:xs -> go (x:'\\':acc) xs
      '#':'{':xs | (ys, '}':zs) <- span (/= '}') xs -> lit $ Expression ys : go "" zs
      x:xs -> go (x:acc) xs
      where
        lit :: [Node] -> [Node]
        lit nodes
          | null acc = nodes
          | otherwise = (Literal . unescape $ reverse acc) : nodes
