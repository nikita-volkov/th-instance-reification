module HTFTestSuite.Prerequisites where

import Prelude

data A = A
newtype B a = B a deriving (Show)
type C = (B Int, B Char)


class MultiParamClass a b c
instance (Eq a) => MultiParamClass a Int Char
