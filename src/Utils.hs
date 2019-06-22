module Utils where

flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

data LetIndex =
  LetIndex { lambdaIndex :: Int
           , localIndex  :: Int
           , letIndex    :: Int
           , innerIndex  :: Int }
  deriving (Show, Eq)

mapLambdaIndex :: (Int -> Int) -> LetIndex -> LetIndex
mapLambdaIndex f (LetIndex lamI locI letI innI) = LetIndex (f lamI) locI letI innI
