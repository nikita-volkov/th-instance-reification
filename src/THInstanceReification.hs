module THInstanceReification
(
  reifyProperInstances,
  isProperInstance,
  typesSatisfyDecConstraints,
)
where

import THInstanceReification.Prelude.Basic
import THInstanceReification.Prelude.TH
import qualified Data.Map as Map


-- |
-- Same as 'reifyInstances', but also checks the constraints.
reifyProperInstances :: Name -> [Type] -> Q [InstanceDec]
reifyProperInstances n tl = 
  reifyInstances n tl >>= filterM (typesSatisfyDecConstraints tl)

-- |
-- Same as 'isInstance', but also checks the constraints.
isProperInstance :: Name -> [Type] -> Q Bool
isProperInstance n tl = 
  not . null <$> reifyProperInstances n tl

-- |
-- Analyze the constraints of the provided instance dec to be satisfied by types.
-- 
-- Note that this function does not analyze the equality constraints (@F a ~ Bool@).
-- It simply considers them to be true.
typesSatisfyDecConstraints :: [Type] -> InstanceDec -> Q Bool
typesSatisfyDecConstraints tl = \case
  InstanceD c it _ -> let
    ([ConT n], htl) = splitAt 1 $ reverse $ unapplyType it
    in analyze c n htl
  d -> fail $ "Not an instance dec: " <> show d
  where
    analyze c n htl = and <$> mapM analyzeConstraint c
      where
        actualTypeByVarName = \n ->
          Map.lookup n m ?: 
          ($bug $ "Unexpected key: " <> show n <> ", in a map: " <> show m)
          where
            m = Map.fromList $ concat $ map accRecords $ zip tl htl
              where
                accRecords = \case
                  (AppT al ar, AppT hl hr) -> accRecords (al, hl) ++ accRecords (ar, hr)
                  (a, VarT n) -> [(n, a)]
                  (a, h) | a /= h -> fail $ "Unmatching types: " <> show a <> ", " <> show h
                  _ -> []
        analyzeConstraint = \case
          EqualP _ _ -> return True
          ClassP n tl -> do
            let tl' = map (replaceTypeVars actualTypeByVarName) tl
            isProperInstance n tl'

unapplyType :: Type -> [Type]
unapplyType = \case
  AppT l r -> r : unapplyType l
  t -> [t]

-- | Deeply traverse the type signature and replace all vars in it.
replaceTypeVars :: (Name -> Type) -> Type -> Type
replaceTypeVars f = \case
  AppT l r -> AppT (replaceTypeVars f l) (replaceTypeVars f r)
  VarT n -> f n
  t -> t

