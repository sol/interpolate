{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.String.Interpolate.ParseSpec (main, spec) where

import           Test.Hspec

import           Data.String.Interpolate.Parse

deriving instance Eq a => Eq (Node a)
deriving instance Show a => Show (Node a)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseNodes" $ do
    it "parses string literals" $ do
      parseNodes "foo" `shouldBe` [Literal "foo"]

    it "parses abstractions" $ do
      parseNodes "#{}" `shouldBe` [Abstraction ()]

    it "parses expressions" $ do
      parseNodes "#{foo}" `shouldBe` [Expression "foo"]

    it "parses embedded expressions" $ do
      parseNodes "foo #{bar} baz" `shouldBe` [Literal "foo ", Expression "bar", Literal " baz"]

    context "when given an unterminated expression" $ do
      it "parses it as a string literal" $ do
        parseNodes "foo #{bar" `shouldBe` [Literal "foo #{bar"]
