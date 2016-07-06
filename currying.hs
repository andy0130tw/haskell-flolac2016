{-

FLOLAC 2016 - Function Currying
Andy Pan, 2016/7/4

-}

sqr :: Int -> Int
sqr x = x * x

-- pow4 = sqr sqr x   -- X, means `(sqr sqr) x` and results an error
-- pow4 = sqr (sqr x) -- O

-- Notice the types!!!
twiceInt :: (Int -> Int) -> Int -> Int
twiceInt f x = f (f x)

-- we can use a variable to extend the functionality
twice :: (a -> a) -> a -> a
-- twice f x = f (f x)
-- can also be written as a composition
twice f = f . f

quad :: Int -> Int
quad x = twice sqr x
