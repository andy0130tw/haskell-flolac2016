{-

FLOLAC 2016 - Topics on tail recursions
Andy Pan, 2016/7/4

-}

-- slow fib
fib :: Int -> Int
fib x | x == 0    = 0
      | x < 3     = 1
      | otherwise = fib(x - 1) + fib(x - 2)

-- fib by tail recursion
tailFibAcc :: Int -> Int -> Int -> Int
tailFibAcc a b n | n == 0    = b
                 | otherwise = tailFibAcc (a + b) a (n - 1)

tailFib :: Int -> Int
tailFib = tailFibAcc 1 0
