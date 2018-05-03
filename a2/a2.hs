module A2 where
-- Insert a at the index n in the list xs.
insertAt :: Int -> a -> [a] -> [a]
insertAt n a xs = as ++ [a] ++ bs
                  where (as, bs) = splitAt n xs

-- Remove duplicates from the list xs.
uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq [x]    = [x]
uniq (x:xs) = if x == head (xs) then uniq xs
              else [x] ++ uniq (xs) 

-- Join 2 tuples (making a triple), when the first element of each matches.
join :: Eq a => [(a,b)] -> [(a,c)] -> [(a,b,c)]
join xs ys = [ (a, b, d) |
               (a, b) <- xs,
               (c, d) <- ys,
               a == c ]

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
                let x = if a == c
                         then Just d
                         else Nothing
              ]

