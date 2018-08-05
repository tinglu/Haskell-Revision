----------------------------
-- Sort
----------------------------

-- Quick sort
qsort :: [Int] -> [Int]
qsort [] = []
qsort (pivot:others) = (qsort lowers) ++ [pivot] ++ (qsort highers)
    where lowers  = filter (<pivot)  others
          highers = filter (>=pivot) others


-- Merge sort
-- msort [1,5,9,8,4,7,2,6,3]
-- exp: [1,2,3,4,5,6,7,8,9]
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
-- Search
----------------------------

-- Linear search
-- search 3 [1,2,3,4]
-- exp: True
-- search 5 [1,2,3,4]
-- exp: False
search :: Int -> [Int] -> Bool
search x [] = False
search x (y:xs) = x == y || search x xs


-- Binary search
-- Since Haskell lists do not provide random access, most list functions and
-- operations (including length and !!) run in linear time in the length of the
-- list. So time complexity of O(nlogn), rather than the usual O(logn).
-- bsearch 3 [1,2,3,5]
-- exp: True
-- bsearch 4 [1,2,3,5]
-- exp: False
bsearch :: Int -> [Int] -> Bool
bsearch _ [] = False
bsearch x [y] = x == y
bsearch x xs =
    let mid = (length xs) `div` 2
        y = xs !! mid
    in
        if x == y then True
        else
            if x < y then bsearch x (take mid xs)
            else bsearch x (drop mid xs)



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