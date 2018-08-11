-----------------------
-- List comprehension
-----------------------

-- [    x^2   | x <- [1..6] ]
--  '--------' '----------'
--  expression  generator

-- [    x^2   | x <- [1..12], even x ]
--  '--------' '-----------' '------'
--  expression   generator    filter

-- Nesting filters and generators
-- l = [ x++" "++y |
--   x <- ["hello", "fly away", "come back"],
--   length x > 7,
--   y <- ["peter", "matthew", "paul"],
--   length y < 7
-- ]
-- l is ["fly away peter","fly away paul","come back peter","come back paul"]


-- Nested loop conversion
-- Main> matches [1,2,3] [1,2,4]
-- 2
-- Main> matches [1,2,3] [1,1,1]
-- 3
-- Main> matches [1,1,2] [1,3,1]
-- 4
matches :: Eq a => [a] -> [a] -> Int
matches xs ys = length [y | x <- xs, y <- ys, x == y]

-- Nested loops and if-statements

bond :: (Char, Char) -> Bool
bond ('A', 'T') = True
bond ('T', 'A') = True
bond ('C', 'G') = True
bond ('G', 'C') = True
bond (_, _)     = False

-- takes two strands of DNA (represented as strings of capital letters
-- 'A', 'G', 'C', or 'T') and returns True if they are complementary,
-- and False otherwise.
compDNA :: String -> String -> Bool
compDNA s1 s2 = and [bond x | x <- zip s1 s2]

-- Pythagorean triples
-- Main> take 5 pyth
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13)]
pyth :: [(Integer,Integer,Integer)]
pyth = [(a,b,c) | c <- [1..], a <- [1..c], b <- [1..c], a^2 + b^2 == c^2]
