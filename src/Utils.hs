module Utils where

import qualified Data.Map as Map

flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

mapMapM :: (Ord k, Monad m) => (a -> m b) -> Map.Map k a -> m (Map.Map k b)
mapMapM f = Map.foldrWithKey folder (return Map.empty)
  where
    folder k v acc = Map.insert k <$> f v <*> acc

mapAndUnzip :: (a -> (b, c)) -> [a] -> ([b], [c])
mapAndUnzip f (x:xs) = (a:as, b:bs)
  where
    (a, b)   = f x
    (as, bs) = mapAndUnzip f xs
mapAndUnzip _ [] = ([], [])

data LetIndex =
  LetIndex { lambdaIndex :: Int
           , localIndex  :: Int
           , letIndex    :: Int
           , innerIndex  :: Int }
  deriving (Show, Eq)

mapLambdaIndex :: (Int -> Int) -> LetIndex -> LetIndex
mapLambdaIndex f (LetIndex lamI locI letI innI) = LetIndex (f lamI) locI letI innI
