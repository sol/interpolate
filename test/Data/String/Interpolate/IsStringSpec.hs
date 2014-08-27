{-# LANGUAGE QuasiQuotes #-}
module Data.String.Interpolate.IsStringSpec (main, spec) where

import           Test.Hspec

import qualified Data.Text as T
import           Data.String.Interpolate.IsString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "[i|...|]" $ do
    it "can be used to construct String literals" $ do
      [i|foo #{23 :: Int} bar|] `shouldBe` "foo 23 bar"

    it "can be used to construct Text literals" $ do
      [i|foo #{23 :: Int} bar|] `shouldBe` T.pack "foo 23 bar"
