module Set
where

-- This module implements sets as ordered lists without duplicates.
-- The implementation is mainly for instructional purposes; a better
-- set implementation (using size balanced binary trees) is available
-- through the Data.Set library.

import           Data.List

newtype Set a = Set [a] deriving (Eq, Show)


-- functions for building sets

set :: Ord a => [a] -> Set a
set es
  = Set (nub (sort es))

emptyset :: Set a
emptyset
  = Set []

setInsert :: (Ord a) => a -> Set a -> Set a
setInsert x (Set es)
  = set (x:es)

setDelete :: Ord a => a -> Set a -> Set a
setDelete x (Set es)
  | x `setElem` Set es = Set (delete x es)
  | otherwise          = Set es


-- functions for investigating sets

setSize :: Set a -> Int
setSize (Set es)
  = length es

setEmpty :: Set a -> Bool
setEmpty (Set es)
  = null es

setElem :: (Ord a) => a -> Set a -> Bool
setElem x (Set es)
  = elem x takeWhile (<=x) es

setElements :: Ord a => Set a -> [a]
setElements (Set es)
  = es


-- functions implementing set operations

setSubset :: (Ord a) => Set a -> Set a -> Bool
setSubset (Set es) b
  = all (`setElem` b) es

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys)
  = set (xs ++ ys)

setIntersect :: Ord a => Set a -> Set a -> Set a
setIntersect (Set []) (Set _) = Set []
setIntersect (Set _) (Set []) = Set []
setIntersect (Set (x:xs)) (Set ys)
    | setElem x (Set ys) = setInsert x (setIntersect (Set xs) (setDelete x (Set ys)))
    | otherwise = setIntersect (Set xs) (Set ys)

setProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
setProduct (Set _) (Set []) = Set []
setProduct (Set []) (Set _) = Set []
setProduct (Set (x:xs)) (Set ys) =
    setUnion (Set [(x, y)|y<-ys]) (setProduct (Set xs) (Set ys))

-- Not working yet...
-- powerset :: (Ord a) => Set a -> Set (Set a)
-- powerset (Set []) = Set [Set []]
-- powerset (Set (x:xs)) =
--     setUnion (Set sub) (Set [setInsert x y | y<-sub])
--     where (Set sub) = powerset (set xs)
