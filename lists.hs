-- Quick sort
qsort :: [Int] -> [Int]
qsort [] = []
qsort (pivot:others) = (qsort lowers) ++ [pivot] ++ (qsort highers)
    where lowers  = filter (<pivot)  others
          highers = filter (>=pivot) others


-- Merge sort
merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort (x1:x2:[]) = merge [x1] [x2]
msort xs =
    merge (msort ls) (msort rs)
    where
        len = length xs `div` 2
        ls = take len xs
        rs = drop len xs


-- Bubble sort


-- Insertion sort


-- Selection sort






----------------------------
-- Some built-in functions
----------------------------

evens :: Int -> [Int]
evens n = take n [2,4..]


import Data.List

-- setEq [1,2,2] [2,1]
-- exp: True
-- setEq [1,2,3] [2,1]
-- exp: False
setEq :: [Int] -> [Int] -> Bool
setEq xs ys =
    xs' == ys'
    where
        xs' = sort (nub xs)
        ys' = sort (nub ys)

-- freqs [2,2,1,3,4,4,4]
-- exp: [1,2,1,3]
freqs :: [Int] -> [Int]
freqs xs = map length (group $ sort xs)