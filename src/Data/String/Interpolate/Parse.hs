module Data.String.Interpolate.Parse where

import           Data.String.Interpolate.Internal.Util

data Node a = Literal String | Expression String | Abstraction a

parseNodes :: String -> [Node ()]
parseNodes = go ""
  where
    go :: String -> String -> [Node ()]
    go acc input = case input of
      ""  -> lit []
      '\\':x:xs -> go (x:'\\':acc) xs
      '#':'{':xs | (e, '}':ys) <- span (/= '}') xs -> lit $ expression e : go "" ys
      x:xs -> go (x:acc) xs
      where
        expression e
          | null e = Abstraction ()
          | otherwise = Expression e

        lit :: [Node ()] -> [Node ()]
        lit nodes
          | null acc = nodes
          | otherwise = (Literal . unescape $ reverse acc) : nodes
