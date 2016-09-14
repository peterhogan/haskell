-- silly function to make a point
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x*y*z

-- compare with 100
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- divideByTen
divByTen :: (Floating a) => a -> a
divByTen = (/10)

---
isUpAlph :: Char -> Bool
isUpAlph = (`elem` ['A'..'Z'])

-- double function
double :: (Num a) => a -> a
double n = n + n

-- apply twice function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- zipwith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- a predicate is a function that tells whether something is True or False
-- filter takes a predicate and a list and returns the elements that are true
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
        | p x           = x : filter' p xs
        | otherwise     = filter' p xs

-- quicksort alt.
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
        let smallerSorted = quicksort' (filter (<=x) xs)
            biggerSorted = quicksort' (filter (>x) xs)
        in smallerSorted ++ [x] ++ biggerSorted

-- nice divisibility functions
largestDivisible :: (Integral a) => a -> a
largestDivisible n = head (filter p [100000,99999..])
        where p x = mod x n == 0

allDivisors :: (Integral a) => a -> [a]
allDivisors n = filter p [100000,99999..0]
        where p x = mod x n == 0

-- collatz!!
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
        | even n = n:chain (n `div` 2)
        | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 25) (map chain [1..100]))

-- sum using a fold
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- like range() in python
range a b = [ x | x <- [a..b] ]

-- fibon witha fold - fibonacci up to n 
-- class decl fibon ::
-- fibon n = foldl (\acc x -> acc + x
-- CANT DO THIS YET.. NOT SURE.

-- elem with folds
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- length with a fold!
length'' :: (Num b, Foldable t) => t a -> b
length'' xs = foldl (\acc x -> acc + 1) 0 xs

--map with a foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- standard functions with folds
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

--filter' :: (a -> Bool) -> [a] -> [a]
--filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

head'' :: [a] -> Int -> [a]
head'' [] _ = error "List Empty"
head'' xs n 
        | length xs > (n-1)    = [ xs !! x | x <- [0..(n-1)]]
        | otherwise             = error "Index larger than list"

-- sqrt sum fn
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- If the expression ends with three parentheses, chances are that if you translate it into function composition, it'll have three composition operators.

