-- Multiply by addition. 
multiply :: Int -> Int -> Int
multiply a 0 = 0
multiply a 1 = a
multiply a b = if a < b then b + multiply b (a - 1)
               else a + multiply b (a - 1)

-- Find the smallest int in a list.
smallest :: [Int] -> Int
smallest []     = error "smallest: list must be non-empty"
smallest [x]    = x
smallest (x:xs) = min x (smallest xs)


