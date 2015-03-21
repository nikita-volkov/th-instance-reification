{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import THInstanceReification.Prelude.Basic
import THInstanceReification.Prelude.TH
import THInstanceReification

main = htfMain $ htf_thisModulesTests


data A = A

test_existingInstance = do
  assertBool $ read $( do
      t <- [t| (Int, Int) |]
      r <- isProperInstance ''Show [t]
      stringE $ show $ r
    )

test_nonExistingInstance = do
  assertBool $ not $ read $( do
      t <- [t| (Int, A) |]
      r <- isProperInstance ''Show [t]
      stringE $ show $ r
    )


class MultiParamClass a b c
instance (Eq a) => MultiParamClass a Int Char

test_existingInstanceOnMultiParamClass = do
  assertBool $ read $( do
      int <- [t|Int|]
      char <- [t|Char|]
      stringE . show =<< isProperInstance ''MultiParamClass [int, int, char]
    )

test_nonExistingInstanceOnMultiParamClass = do
  assertBool $ not $ read $( do
      int <- [t|Int|]
      char <- [t|Char|]
      a <- [t|A|]
      stringE . show =<< isProperInstance ''MultiParamClass [a, int, char]
    )


test_compositeTypes = do
  assertBool $ read $( do
      t <- [t|(Int, Char, Char)|]
      stringE . show =<< isProperInstance ''Show [t]
    )

test_compositeTypesMultiParam = do
  assertBool $ read $( do
      t <- [t|(Int, Char, Char)|]
      int <- [t|Int|]
      char <- [t|Char|]
      stringE . show =<< isProperInstance ''MultiParamClass [t, int, char]
    )

newtype B a = B a deriving (Show)

test_polyTypes = do
  assertBool $ read $( do
      t <- [t|(B Char)|]
      stringE . show =<< isProperInstance ''Show [t]
    )

type C = (B Int, B Char)

test_synonyms = do
  assertBool $ read $( do
      t <- [t|C|]
      stringE . show =<< isProperInstance ''Show [t]
    )


