-----------------------
-- Binary search tree
-----------------------

data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving Show

height :: Tree a -> Int
height Empty = 0
height (Node x l r) = 1 + (max (height l) (height r))

elements :: Tree a -> [a]
elements Empty = []
elements (Node x l r) = (elements l) ++ [x] ++ (elements r)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y l r)
    | x == y = Node y l r
    | x < y = Node y (insert x l) r
    | otherwise = Node y l (insert x r)

-- takes a list of items and builds a binary search tree
-- containing those same items as its values.
buildtree :: Ord a => [a] -> Tree a
buildtree [] = Empty
buildtree (x:xs) = insert x (buildtree xs)

-- sort a list using binary search tree
treesort :: (Ord a) => [a] -> [a]
treesort xs = elements $ buildtree xs

-----------------------
-- A binary search tree where each node contains a (key, value) pair,
-- rather than just a key.


-----------------------
-- A non-binary search tree, where each node has a list of subtrees.


-----------------------
-- Any self balancing search tree data structure


-----------------------
-- An AVL tree, where each node has an integer 'balance factor'
-- which is maintained during insertions by a series of tree rotations


-----------------------
-- A 2-3 tree, where each node can have up to 2 values, and insertions
-- are always performed at the leaf level (which may then involve promoting
-- some values up the tree)


-----------------------
-- A red-black tree, a binary search tree where each node has a colour
-- as well as a value, used to help maintain balance