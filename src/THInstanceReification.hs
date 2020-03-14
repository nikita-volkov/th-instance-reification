{-# LANGUAGE CPP #-}
-- |
-- Fixed versions of 'reifyInstances' and 'isInstance' as per
-- the following ghc issue:
-- <https://ghc.haskell.org/trac/ghc/ticket/7066>.
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
import Control.Monad
import Data.List.Extras


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
-- Analyze the constraints of the provided instance dec to be satisfied
-- by the provided types.
--
-- Note that this function does not analyze the equality constraints (@F a ~ Bool@).
-- It simply considers them to be true.
typesSatisfyDecConstraints :: [Type] -> InstanceDec -> Q Bool
typesSatisfyDecConstraints tl = \case
  InstanceD _ context instanceType _ -> do
    let ([ConT n], htl) = splitAt 1 $ reverse $ unapplyType instanceType
    -- Expand type synonyms in type signatures,
    -- using 'expandSyns' from the "th-expand-syns" library:
    expandedTypes <- mapM expandSyns tl
    expendedInstanceTypes <- mapM expandSyns htl
    maybe
      (fail $ "Unmatching amounts of types: " <> show expandedTypes <> ", " <>
              show expendedInstanceTypes)
      (analyze context)
      -- 'pair' is a safe version of 'zip' from the "list-extras" library,
      -- which returns 'Nothing', when lists differ in size.
      (pair expandedTypes expendedInstanceTypes)
  d -> fail $ "Not an instance dec: " <> show d
  where
    -- |
    -- Test, whether a list of associations from tested types to types in instance head
    -- satisfies the given context.
    analyze :: Cxt -> [(Type, Type)] -> Q Bool
    analyze context typeAssocs = and <$> mapM analyzePredicate context
      where
        -- |
        -- A partial function,
        -- which returns a tested type by a variable name.
        actualTypeByVarName :: Name -> Type
        actualTypeByVarName = \n ->
          Map.lookup n m ?:
          ($bug $ "Unexpected key: " <> show n <> ", in a map: " <> show m)
          where
            -- A memoization cache.
            m = Map.fromList $ concat $ map accRecords $ typeAssocs
              where
                -- Parallelly expand all associations down to type variables
                -- of instance head,
                -- producing a list of associations of names of those variables
                -- to tested types.
                accRecords = \case
                  (AppT al ar, AppT hl hr) -> accRecords (al, hl) ++ accRecords (ar, hr)
                  (a, VarT n) -> [(n, a)]
                  (a, h) | a /= h -> $bug $ "Unmatching types: " <> show a <> ", " <> show h
                  _ -> []
        -- |
        -- Test a predicate by substituting all type vars with associated
        -- tested types.
        analyzePredicate :: Pred -> Q Bool
        analyzePredicate = \case
#if MIN_VERSION_template_haskell(2,10,0)
          AppT (AppT EqualityT _) _ -> return True
          AppT (ConT n) t -> do
            let t' = replaceTypeVars actualTypeByVarName t
            isProperInstance n [t']
          _ -> return True
#else
          EqualP _ _ -> return True
          ClassP n tl -> do
            let tl' = map (replaceTypeVars actualTypeByVarName) tl
            isProperInstance n tl'
#endif

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

