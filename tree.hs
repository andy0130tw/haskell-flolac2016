{-

FLOLAC 2016 - Exercise #2 (Tree)
Andy Pan, 2016/7/6

-}

data Tree a = Null | Node a (Tree a) (Tree a)

-- p3 8.
mymin :: Int -> Int -> Int
mymin a 1 = 1
mymin 1 b = 1
mymin (1+a) (1+b) = 1 + mymin a b

minT :: Tree Int -> Int
minT Null = 99999999
minT (Node x t u) = min x (min (minT t) (minT u))

mapT :: (a -> b) -> Tree a -> Tree b
mapT f Null = Null
mapT f (Node x t u) = Node (f x) (mapT f t) (mapT f u)

-- Exercise: Prove that (minT . mapT (n+)) == (n+) . (minT) for all n and trees
