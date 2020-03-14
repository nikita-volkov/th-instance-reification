module THInstanceReification.Prelude.Basic
( bug,

  (?:),
  (|>),
  (<|),
  (|$>),
)
where

-- list-extras
import Data.List.Extras as Exports
import Debug.Trace
import Data.Maybe
import Language.Haskell.TH

bug :: ExpQ
bug = [e| error . (msg <>) |]
  where
    msg = "A \"th-instance-reification\" package bug: " :: String


traceM :: (Monad m) => String -> m ()
traceM s = trace s $ return ()

(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINE (?:) #-}

(|>) :: a -> (a -> b) -> b
a |> aToB = aToB a
{-# INLINE (|>) #-}

(<|) :: (a -> b) -> a -> b
aToB <| a = aToB a
{-# INLINE (<|) #-}

-- |
-- The following are all the same:
-- fmap f a == f <$> a == a |> fmap f == a |$> f
--
-- This operator accomodates the left-to-right operators: >>=, >>>, |>.
(|$>) = flip fmap
{-# INLINE (|$>) #-}
