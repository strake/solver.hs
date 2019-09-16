{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Solver where

import Control.Applicative
import Control.Monad.Trans.Key
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor.Compose
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality
import qualified Data.Void as Void
import Numeric.Natural
import Unsafe.Coerce

data Model s = Model { unModel :: ∀ a . Key s a -> a }

newtype SolverT s f a = SolverT { unSolverT :: KeyringT s f (Constraints s, Model s -> a) }
  deriving (Functor)
  deriving (Applicative) via Compose (Compose (KeyringT s f) ((,) (Constraints s))) ((->) (Model s))

newVar :: Applicative f => SolverT s f (Key' s a)
newVar = SolverT $ pure . pure . Key' <$> newKey

instance Monad f => Alternative (SolverT s f) where
    empty = SolverT
        [(Set.singleton Set.empty, Void.absurd . flip unModel k) | k <- newKey]
    SolverT a <|> SolverT b = SolverT
        [((Set.insert . Set.singleton) (False, CB k') aCs `disjoinConstraints`
          (Set.insert . Set.singleton) (True,  CB k') bCs, bool f g =<< flip unModel k)
        | (aCs, f) <- a, (bCs, g) <- b, k <- newKey, let k' = Key' k]

assert :: Applicative f => Bool -> Constraint s -> SolverT s f ()
assert b c = (SolverT . pure) ((Set.singleton . Set.singleton) (b, c), pure ())

type Constraints s = Set (Set (Bool, Constraint s))

disjoinConstraints :: Constraints s -> Constraints s -> Constraints s
disjoinConstraints = (fmap . fmap) Set.fromList (liftA2 (<>) `on` toList)

data Constraint s
  = CB !(Key' s Bool)
  | LessN !(Key' s Natural) !(Key' s Natural)
  | EqualN !(Key' s Natural) !(Key' s Natural)
  deriving (Eq, Ord, Show)

newtype Key' s a = Key' { unKey' :: Key s a }
  deriving newtype (TestEquality)

instance Eq (Key' s a) where a == b = isJust $ testEquality a b
instance Ord (Key' s a) where compare = compare `on` (unsafeCoerce :: _ -> Natural)

instance Show (Key' s a) where
    show (Key' a) = "Key " ++ show (unsafeCoerce a :: Natural)
