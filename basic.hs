{-

FLOLAC 2016 - Exercise #1
Andy Pan, 2016/7/4

-}

-- 1. `even` is an existing function, using `myeven` as name
myeven :: Int -> Bool
myeven x = (x `mod` 2) == 0
--    alternative def. (guarded expr.)
myeven_alt :: Int -> Bool
myeven_alt x | (x `mod` 2) == 0 = True
             | otherwise        = False

-- 2. converting numeric types with `fromIntegral`
sqr :: Int -> Int
sqr x = x * x

area :: Int -> Double
area r = pi * fromIntegral(sqr r)
         where pi :: Double
               pi = 22 / 7

--    alternative def.
area_alt :: Int -> Double
area_alt r = let pi = 22 / 7
             in pi * fromIntegral(sqr r)

-- 3.
payment :: Int -> Int
payment week | week > 19 = round(fromIntegral(salary) * 1.06)
             | otherwise = salary
             where hours  = week * 7 * 5
                   salary = hours * 180

-- 4. (a) scope of variables; the result is 13
nested :: Int
nested = let x = 3
         in (let x = 5
             in x + x) + x
--    (b) recursive; not-terminating since `x` inside creates a new scope
recursive :: Int
recursive = let x = 3
            in let x = x + 1  -- stuck here even if this line
                              -- is replaced with `let x = x`
               in x

-- 5.
smaller :: Int -> Int -> Int
smaller x y = if x < y then x else y

-- st3 = smaller 3  -- `st3 x` is equivalent to `(smaller 3) x`

-- 6.
poly :: Int -> Int -> Int -> Int -> Int
poly a b c x = a * sqr x + b * x + c

poly1 :: Int -> Int
poly1 = poly 1 2 1

poly2 :: Int -> Int -> Int -> Int
poly2 a b c = poly a b c 2

-- 7.
twice :: (a -> a) -> a -> a
twice f = f . f

quad :: Int -> Int
quad x = twice sqr x

-- 9. explain the types of each functions
--    output `x^2 + 3 x + 2` using lambda expr
forktimes f g x = (f x) * (g x)
-- forktimes :: Num a => (t -> a) -> (t -> a) -> t -> a
-- forktimes (\x -> x + 1) (\x -> x + 2)

lift2 h f g x = h(f x)(g x)
-- lift2 :: (t1 -> t2 -> t) -> (t3 -> t1) -> (t3 -> t2) -> t3 -> t
-- lift2 (*) (\x -> x + 1) (\x -> x + 2)
-- or, more concisely,
-- lift2 (*) (+ 1) (+ 2)

-- `forktimes` can be defined from `lift2`
-- forktimes f g x = (*) (f x)(g x)
--                 = lift2 (*) f g x
-- by η-conversion, removing `x`, `g`, `f` respectively,
-- and we obtain `forktimes = lift2 (*)`

-- Q. given a string `xs`, find all index where `x` occurs
positions :: Char -> String -> [Int]
positions x xs = map snd (filter (\p -> fst(p) == x) (zip xs [0..]))
