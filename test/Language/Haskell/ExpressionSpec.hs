module Language.Haskell.ExpressionSpec (spec) where

import           Test.Hspec

import           Language.Haskell.Expression

import           Language.Haskell.Meta.Parse.Careful (parseExp)
import           Language.Haskell.TH.Syntax

spec :: Spec
spec = do
  describe "parseExpression" $ do
    it "accepts variable names" $ do
      let e = "foo"
      parseExpression e `shouldBe` parseExp e

    it "accepts type signatures" $ do
      pending
      -- let e = "foo :: Int"
      -- parseExpression e `shouldBe` parseExp e
      -- (Right $ SigE (VarE $ mkName "foo") (ConT $ mkName "Int")) `shouldBe` parseExp e
