module Assignment2 where
import Data.Char (isDigit)

------ Question1.
-- | insertAt - Insert a at the index n in the list xs.
-- >>> insertAt 3 '-' "abcde"
-- "abc-de"
-- >>> insertAt 2 100 [1..5]
-- [1,2,100,3,4,5]
insertAt :: Int -> a -> [a] -> [a]
insertAt n a xs = as ++ [a] ++ bs
                  where (as, bs) = splitAt n xs

------ Question 2.
-- | uniq - Remove duplicates from the list xs.
-- >>> uniq [1,2,2]
-- [1,2]
-- >>> uniq [1,2,3]
-- [1,2,3]
uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq [x]    = [x]
uniq (x:xs) = if x == head (xs) then uniq xs
              else [x] ++ uniq (xs) 

------ Question 3.
-- | join - Join 2 tuples (making a triple), when the first element of each matches.
-- >>> join [(2,"S"),(1,"J")] [(2,True),(3,False)]
-- [(2,"S",True)]
-- >>> join [(2,"S"),(1,"J")] [(2,1),(2,2),(3,4)]
-- [(2,"S",1),(2,"S",2)]
join :: Eq a => [(a,b)] -> [(a,c)] -> [(a,b,c)]
join xs ys = [
               (a, b, d) |
               (a, b) <- xs,
               (c, d) <- ys,
               a == c
             ]

------ Question 4.
-- | ljoin - Create a list of triples from 2 lists of tuples, where the first
--   element matches in the two lists.
-- >>> ljoin [(1, 'a'), (2, 'b')] [(2, True)]
-- [(1,'a',Nothing),(2,'b',Just True)]
-- >>> ljoin [(1, 'a'), (2, 'b'), (3, 'd')] [(1, 20), (2, 30)]
-- [(1,'a',Just 20),(1,'a',Nothing),(2,'b',Nothing),(2,'b',Just 30),(3,'d',Nothing),(3,'d',Nothing)]
ljoin :: Eq a => [(a,b)] -> [(a,c)] -> [(a,b,Maybe c)]
ljoin xs ys = [
                (a, b, x) |
                (a, b) <- xs,
                (c, d) <- ys,
                let x = if a == c then Just d
                        else Nothing
              ]

------ Question 5.
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- | size - Determine the size of a given tree (the number of Leaf nodes).
-- >>> size (Node (Node (Leaf 1)(Leaf 3)) (Leaf 2))
-- 3
-- >>> size (Leaf 1)
-- 1
-- >>> size (Node (Node (Leaf 1)(Leaf 3)) (Node (Node (Leaf 1)(Leaf 3)) (Leaf 2)))
-- 5
size::Tree a -> Int
size (Leaf a)   = 1
size (Node a b) = size a + size b

-- | isBalanced - Determine if a given tree is balanced, meaning that the difference
--   between the height of the left and right subtrees is at most 1.
-- >>> isBalanced (Node (Node (Leaf 1)(Leaf 3)) (Leaf 2))
-- True
-- >>> isBalanced (Node (Node (Leaf 1)(Node (Leaf 1)(Leaf 3))) (Leaf 2))
-- False
isBalanced :: Tree a -> Bool
isBalanced (Leaf a)   = True
isBalanced (Node a b) = let d = (size a - size b)
                        in (abs d) <= 1


------ Question 6.
someDigits :: String -> Bool
someDigits "" = False
someDigits xs = any (isDigit) xs

-- isNumber - test if a string contains a valid number, as defined by
--   number -> .digit+ | digit+ [.digit*] 
--  >>> isNumber ".5"
--  True
--  >>> isNumber "1.5"
--  True
--  >>> isNumber "1"
--  True
--  >>> isNumber "1."
--  True
--  >>> isNumber "0.1"
--  True
--  >>> isNumber "abc"
--  False
isNumber :: String -> Bool
isNumber xs
    | xs == "." = False
    | otherwise = someDigits xs && all (\a -> isDigit a || a == '.') xs 

------ Question 7.
elemOrNothing :: Int -> [a] -> Maybe a
elemOrNothing a [] = Nothing
elemOrNothing a bs
  | a >= length bs = Nothing
  | otherwise = Just (bs!!a)

-- getElems - return the given elements of a list if they exist.
-- >>> getElems [2,4] [1..10]
-- [Just 3,Just 5]
-- >>> getElems [2,4] [1..4]
-- [Just 3,Nothing]
getElems :: [Int] -> [a] -> [Maybe a]
getElems [] []  = []
getElems [] bs  = []
getElems as []  = []
getElems as bs  = [ x |
                    a <- as,
                    let x = elemOrNothing a bs ]

