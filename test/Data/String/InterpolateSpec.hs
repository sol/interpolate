{-# LANGUAGE QuasiQuotes #-}
module Data.String.InterpolateSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.String.Interpolate

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "[i|...|]" $ do
    it "interpolates an expression of type Int" $ do
      property $ \x y -> [i|foo #{x + y :: Int} bar|] `shouldBe` "foo " ++ show (x + y) ++ " bar"

    it "interpolates an expression of type String" $ do
      property $ \xs ys -> [i|foo #{xs ++ ys} bar|] `shouldBe` "foo " ++ xs ++ ys ++ " bar"

    it "interpolates abstractions" $ do
      [i|foo #{} bar #{} baz|] (23 :: Int) "test" `shouldBe` "foo 23 bar test baz"

    it "accepts character escapes" $ do
      [i|foo \955 bar|] `shouldBe` "foo \955 bar"

    it "accepts character escapes in interpolated expressions" $ do
      [i|foo #{"\955" :: String} bar|] `shouldBe` "foo \955 bar"

    it "dose not strip backslashes (issue #1)" $ do
      [i|foo\\bar|] `shouldBe` "foo\\bar"

    it "allows to prevent interpolation by escaping the hash with a backslash" $ do
      [i|foo \#{23 :: Int} bar|] `shouldBe` "foo #{23 :: Int} bar"

    it "does not prevent interpolation on literal backslash" $ do
      [i|foo \\#{23 :: Int} bar|] `shouldBe` "foo \\23 bar"
