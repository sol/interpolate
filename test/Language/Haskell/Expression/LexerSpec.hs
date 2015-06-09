module Language.Haskell.Expression.LexerSpec (spec) where

import           Test.Hspec

import           Language.Haskell.Expression.Lexer

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "accepts identifiers" $ do
      tokenize "foo" `shouldBe` [Identifier "foo"]
