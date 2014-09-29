{-# LANGUAGE QuasiQuotes #-}
module Data.String.InterpolateSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Applicative

import           Data.String.Interpolate

main :: IO ()
main = hspec spec

emptyLine :: Gen String
emptyLine = (++ "\n") <$> listOf (elements " \t")

spec :: Spec
spec = do
  describe "[i|...|]" $ do
    it "interpolates an expression of type Int" $ do
      property $ \x y -> [i|foo #{x + y :: Int} bar|] `shouldBe` "foo " ++ show (x + y) ++ " bar"

    it "interpolates an expression of type String" $ do
      property $ \xs ys -> [i|foo #{xs ++ ys} bar|] `shouldBe` "foo " ++ xs ++ ys ++ " bar"

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

  describe "unindent" $ do
    it "removes indentation" $ do
      let xs = "    foo\n  bar\n   baz  \n"
      unindent xs `shouldBe` "  foo\nbar\n baz  \n"

    it "removes the first line of the string if it is empty" $ do
      forAll emptyLine $ \xs -> do
        let ys = "  foo\nbar\n baz\n"
        unindent (xs ++ ys) `shouldBe` ys

    it "does not affect additional empty lines at the beginning" $ do
      unindent "  \n  \nfoo" `shouldBe` "  \nfoo"

    it "empties the last line if it only consists of spaces" $ do
      let xs = "foo\n  "
      unindent xs `shouldBe` "foo\n"

    it "does not affect other whitespace lines at the end" $ do
      unindent "foo\n  \n  " `shouldBe` "foo\n  \n"

    it "disregards empty lines when calculating indentation" $ do
      let xs = "  foo\n\n \n  bar\n"
      unindent xs `shouldBe` "foo\n\n\nbar\n"

    it "correctly handles strings that do not end with a newline" $ do
      let xs = "foo"
      unindent xs `shouldBe` xs

    it "does not affect lines consisting of whitespace (apart from unindenting)" $ do
      unindent " foo\n  \n bar" `shouldBe` "foo\n \nbar"

    it "is total" $ do
      property $ \xs -> length (unindent xs) `shouldSatisfy` (>= 0)

    context "when all lines are empty" $ do
      it "does not unindent at all" $ do
        forAll emptyLine $ \x -> (forAll $ listOf emptyLine) $ \xs -> do
          let ys = concat xs
          unindent (x ++ ys) `shouldBe` ys
