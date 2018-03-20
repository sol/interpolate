{-# LANGUAGE TemplateHaskell #-}
module Data.String.Interpolate (
-- * String interpolation done right
-- |
-- The examples in this module use `QuasiQuotes`.  Make sure to enable the
-- corresponding language extension.
--
-- >>> :set -XQuasiQuotes
-- >>> import Data.String.Interpolate
  i
) where

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.Meta.Parse (parseExp)

import           Data.String.Interpolate.Internal.Util
import           Data.String.Interpolate.Parse
import           Data.String.Interpolate.Compat (Q, Exp, appE)

-- |
-- A `QuasiQuoter` for string interpolation.  Expression enclosed within
-- @#{...}@ are interpolated, the result has to be in the `Show` class.
--
-- It interpolates strings
--
-- >>> let name = "Marvin"
-- >>> putStrLn [i|name: #{name}|]
-- name: Marvin
--
-- or integers
--
-- >>> let age = 23
-- >>> putStrLn [i|age: #{age}|]
-- age: 23
--
-- or arbitrary Haskell expressions
--
-- >>> let profession = "\955-scientist"
-- >>> putStrLn [i|profession: #{unwords [name, "the", profession]}|]
-- profession: Marvin the λ-scientist
i :: QuasiQuoter
i = QuasiQuoter {
    quoteExp = toExp . parseNodes . decodeNewlines
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("Data.String.Interpolate.i: This QuasiQuoter can not be used as a " ++ name ++ "!")

    toExp:: [Node] -> Q Exp
    toExp nodes = case nodes of
      [] -> [|""|]
      (x:xs) -> f x `appE` toExp xs
      where
        f (Literal s) = [|showString s|]
        f (Expression e) = [|(showString . toString) $(reifyExpression e)|]

        reifyExpression :: String -> Q Exp
        reifyExpression s = case parseExp s of
          Left _ -> do
            fail "Parse error in expression!" :: Q Exp
          Right e -> return e

decodeNewlines :: String -> String
decodeNewlines = go
  where
    go xs = case xs of
      '\r' : '\n' : ys -> '\n' : go ys
      y : ys -> y : go ys
      [] -> []
