-- slow fib
fib :: Int -> Int
fib x | x == 0    = 0
      | x < 3     = 1
      | otherwise = fib(x - 1) + fib(x - 2)

-- fib by tail recursion
tailFibAcc :: Int -> Int -> Int -> Int
tailFibAcc a b n | n == 0    = b
                 | otherwise = tailFibAcc (n - 1) (a + b) a

tailFib :: Int -> Int
tailFib = tailFibAcc 1 0
