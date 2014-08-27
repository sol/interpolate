{-# LANGUAGE TemplateHaskell #-}
module Data.String.Interpolate.IsString (i) where

import           Data.String
import           Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Data.String.Interpolate as I

-- |
-- Like `I.i`, but constructs a value of type
--
-- > IsString a => a
i :: QuasiQuoter
i = QuasiQuoter {
    quoteExp = \s -> [|fromString $(quoteExp I.i $ s)|]
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("Data.String.Interpolate.IsString.i: This QuasiQuoter can not be used as a " ++ name ++ "!")
