{-

FLOLAC 2016 - Exercise #2 (List operations)
Andy Pan, 2016/7/5 - 6

-}

-- auxiliary function provided for function `perms`
-- I did not use it though
delete n xs = take n xs ++ drop (n + 1) xs

-- insert e to each position in xs
-- index-based style is not FP enough
-- fan e xs = [take n xs ++ [e] ++ drop n xs | n <- [0 .. length xs]]
fan :: a -> [a] -> [[a]]
fan e [] = [[e]]
fan e (x:xs) = (e:(x:xs)) : (map (x:) (fan e xs))

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (fan x) (perms xs))

subset xs | null xs   = [[]]
          | otherwise = recur ++ (map (++ [last xs]) recur)
                        where recur = subset(init xs)

-- can be written in pattern-matching style
-- myunzip xs | null xs   = ([], [])
--            | otherwise = ((fst . head) xs : fst(recur), (snd . head) xs : snd(recur))
--                        where recur = myunzip(tail xs)

myunzip :: [(a, b)] -> ([a], [b])
myunzip [] = ([], [])
myunzip ((x, y) : l) = (x : xs, y : ys)
                     where (xs, ys) = unzip l
