module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Language.Haskell.TH
import THInstanceReification
import Main.Prerequisites
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck


main = defaultMain $ testGroup "" $ [
    testCase "existingInstance" $ do
      assertBool "" $ read $( do
          t <- [t| (Int, Int) |]
          r <- isProperInstance ''Show [t]
          stringE $ show $ r
        ),
    testCase "nonExistingInstance" $ do
      assertBool "" $ not $ read $( do
          t <- [t| (Int, A) |]
          r <- isProperInstance ''Show [t]
          stringE $ show $ r
        ),
    testCase "existingInstanceOnMultiParamClass" $ do
      assertBool "" $ read $( do
          int <- [t|Int|]
          char <- [t|Char|]
          stringE . show =<< isProperInstance ''MultiParamClass [int, int, char]
        ),
    testCase "nonExistingInstanceOnMultiParamClass" $ do
      assertBool "" $ not $ read $( do
          int <- [t|Int|]
          char <- [t|Char|]
          a <- [t|A|]
          stringE . show =<< isProperInstance ''MultiParamClass [a, int, char]
        ),
    testCase "compositeTypes" $ do
      assertBool "" $ read $( do
          t <- [t|(Int, Char, Char)|]
          stringE . show =<< isProperInstance ''Show [t]
        ),
    testCase "compositeTypesMultiParam" $ do
      assertBool "" $ read $( do
          t <- [t|(Int, Char, Char)|]
          int <- [t|Int|]
          char <- [t|Char|]
          stringE . show =<< isProperInstance ''MultiParamClass [t, int, char]
        ),
    testCase "polyTypes" $ do
      assertBool "" $ read $( do
          t <- [t|(B Char)|]
          stringE . show =<< isProperInstance ''Show [t]
        ),
    testCase "synonyms" $ do
      assertBool "" $ read $( do
          t <- [t|C|]
          stringE . show =<< isProperInstance ''Show [t]
        )
  ]
