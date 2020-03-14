module THInstanceReification.Prelude.TH
(
  module Exports,

  purify,
  tryToReify,
)
where

import THInstanceReification.Prelude.Basic
import Language.Haskell.TH as Exports
import Language.Haskell.TH.ExpandSyns as Exports
import System.IO.Unsafe


purify :: Q a -> a
purify = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n)
