{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import THInstanceExists.Prelude.Basic
import THInstanceExists.Prelude.TH
import THInstanceExists

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
