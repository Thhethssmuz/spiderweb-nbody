module Vector
  ( Vector ()
  , toVector
  , toList
  , (·)
  , magnitude
  , magnitude2
  ) where

import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F
import Control.DeepSeq


data Vector v = Vector { toList :: [v] } deriving (Read, Eq)

toVector :: (Num v) => [v] -> Vector v
toVector vs = Vector vs

instance (Show v) => Show (Vector v) where
  show (Vector v) = "vector" ++ show v


instance Functor Vector where
  fmap f = Vector . fmap f . toList 


instance Applicative Vector where
  pure = Vector . repeat
  (<*>) v1 v2 = Vector $ zipWith ($) (toList v1) (toList v2)


instance (Num v) => Num (Vector v) where 
  (+) v1 v2 = (+) <$> v1 <*> v2
  (-) v1 v2 = (-) <$> v1 <*> v2
  (*) v1 v2 = (*) <$> v1 <*> v2
  negate = fmap negate
  fromInteger = pure . fromInteger
  abs = fmap abs
  signum = fmap signum


instance (Fractional v) => Fractional (Vector v) where
  (/) v1 v2 = (/) <$> v1 <*> v2
  fromRational = pure . fromRational


instance F.Foldable Vector where
  foldMap f (Vector [])     = mempty
  foldMap f (Vector (x:xs)) = f x `mappend` F.foldMap f xs


instance (NFData a) => NFData (Vector a) where
  rnf (Vector []) = ()
  rnf (Vector (x:xs)) = rnf x `seq` rnf xs


infixl 7 ·
(·) :: (Num v) => Vector v -> Vector v -> v
(·) v1 v2 = F.sum (v1 * v2)

magnitude, magnitude2 :: (Floating v) => Vector v -> v
magnitude = sqrt . F.sum . fmap (^2)
magnitude2 = F.sum . fmap (^2)





-------------------------------------------------------------------------------

v1 = toVector [30,20,10]
v2 = toVector [50,0,0]

main = do

  let p = 0.5
  print $ (1-p) * v1 + p * v2


