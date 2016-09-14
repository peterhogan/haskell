-- reduce by m
reduce :: Num a => a -> a -> a
reduce n m = n - m

-- reduce length on a list by n
reduceListValue xs n = reduce (length xs) n

-- last element of a list
myLast xs = xs !! (reduceListValue xs 1)

--last but one element of a list
myLastButt xs = xs !! (reduceListValue xs 2)

-- find kth element counting from 1
elementAt xs k = xs !! (k-1)

-- number of elements in a list
myLength [] = 0
myLength (x:xs) = 1 + myLength xs 

myLength1 = fst . last . zip [1..]

myLength2 = sum . map (\_ -> 1)

--reverse a list
myReverse [] = []
myReverse (x:xs) =  (myReverse xs) ++ [x] 

--palindrome checker
isPalindrome xs = xs == reverse xs

-- compress: eliminate consecutive duplicates in a list
compress (x:ys@(y:_))
    | x == y = compress ys
    | otherwise  = x : compress ys
compress ys = ys

compress' [] = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)
