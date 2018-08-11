--------------------
-- Data Constructor
--------------------

-- MyTrue, MyFalse ---> Called constants or nullary data constructors
data MyBool = MyTrue | MyFalse

mynot :: MyBool -> MyBool
mynot MyTrue = MyFalse
mynot MyFalse = MyTrue


-- The first Point is the type; the second Point is the data constructor
data Point = Point Float Float

-- invert: reflect a point (x,y) in the line x = y
invert :: Point -> Point
invert (Point x y) = Point y x


-- Recusive data types

-- Below is like a linked list
data List a = ListNode a (List a) | ListEnd

mymaximum :: Ord a => List a -> a
mymaximum (ListNode x xs) =
    mymaximum' xs x
    where
        mymaximum' :: Ord a => List a -> a -> a
        mymaximum' ListEnd currMax = currMax
        mymaximum' (ListNode x xs) currMax
            | x > currMax = mymaximum' xs x
            | otherwise = mymaximum' xs currMax