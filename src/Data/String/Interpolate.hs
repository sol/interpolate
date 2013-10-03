{-# LANGUAGE TemplateHaskell #-}
module Data.String.Interpolate (i) where

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.Meta.Parse.Careful (parseExp)

import           Data.String.Interpolate.Util
import           Data.String.Interpolate.Parse
import           Data.String.Interpolate.Compat (Q, Exp, appE, reportError)

i :: QuasiQuoter
i = QuasiQuoter {
    quoteExp = toExp . parseNodes
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
            reportError "Parse error in expression!"
            [|""|]
          Right e -> return e
