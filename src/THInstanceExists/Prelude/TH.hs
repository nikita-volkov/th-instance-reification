module THInstanceExists.Prelude.TH
(
  module Exports,

  purify,
  tryToReify,
)
where

import THInstanceExists.Prelude.Basic
import Language.Haskell.TH as Exports


purify :: Q a -> a
purify = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 
