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

ljoin :: Eq a => [(a,b)] -> [(a,c)] -> [(a,b,Maybe c)]
ljoin xs ys = [
                (a, b, x) |
                (a, b) <- xs,
                (c, d) <- ys,
                let x = (\a c -> if a == c
                                 then d
                                 else Nothing)
              ]

