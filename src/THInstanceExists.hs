module THInstanceExists
(
  reifyInstances',
  isInstance',
)
where

import THInstanceExists.Prelude.Basic
import THInstanceExists.Prelude.TH


reifyInstances' :: Name -> [Type] -> Q [InstanceDec]
reifyInstances' n tl = do
  reify n >>= \case
    ClassI d idl -> case d of
      ClassD c _ vl _ _ -> $notImplemented
      d -> $bug $ "Unexpected declaration: " <> show d
    i -> fail $ "Not a class name: " <> show n

isInstance' :: Name -> [Type] -> Q Bool
isInstance' n tl = not . null <$> reifyInstances' n tl

-- |
-- Analyze the provided types to satisfy the contexts of the instance dec.
isAProperInstanceDec :: [Type] -> InstanceDec -> Q Bool
isAProperInstanceDec tl = \case
  InstanceD c it _ -> case it of
    AppT (ConT n) ht -> analyze c n ht
    t -> $bug $ "Unexpected instance head type: " <> show t
  d -> fail $ "Not an instance dec: " <> show d
  where
    analyze c n ht = and <$> mapM analyzePair (zip tl $ reverse $ unapplyType ht)
      where
        analyzePair (argT, headT) = $notImplemented

unapplyType :: Type -> [Type]
unapplyType = \case
  AppT l r -> r : unapplyType l
  t -> [t]

-- | Deeply traverse the type signature.
traverseType_ :: (Applicative m) => (Type -> m ()) -> Type -> m ()
traverseType_ f = \case
  AppT l r -> traverseType_ f l *> traverseType_ f r
  t -> f t

