lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, not seven"

sayMe :: (Integral a) => a -> String
sayMe 1 = "ONE"
sayMe 2 = "TWO"
sayMe 3 = "THREE"
sayMe 4 = "FOUR"
sayMe 5 = "FIVE"
sayMe 6 = "SIX"
sayMe 7 = "SEVEN"
sayMe 8 = "EIGHT"
sayMe x = "Not between 1 and 8"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1) 

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "List empty"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "empty"
tell (x:[]) = "One element: " ++ show x
tell (x:y:[]) = "Two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "Many elements, the first are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- define S as successor
s :: (Integral a) => a -> a
s 0 = 1
s n = 1 + s (n-1)

-- try mult with rec
mult _ 0 = 0
mult x y = x + mult x (y-1)

-- same with exp
expo _ 0 = 1
expo x 1 = x
expo x y = x `mult` expo x (y-1)

-- euclids algortithm 
euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (mod a b)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String 
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- bmi exercise
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell  weight height
        | bmi <= skinny = "underweight"
        | bmi <= normal = "Normal"
        | bmi <= fat = "FAT"
        | otherwise = "SUPER FAT"
        where bmi = weight / (height)^2
              (skinny, normal, fat) = (18.5,25.0,30.0)

-- own max func
max' :: (Ord a) => a -> a-> a
max' a b | a > b = a | otherwise = b

-- myComp - my own compare function
myComp :: (Ord a) => a -> a -> Ordering
a `myComp` b
        | a > b         = GT
        | a == b        = EQ
        | otherwise     = LT

-- Initials function
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
        where (f:_) = firstname
              (l:_) = lastname

-- bmi list func
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w/(h)^2, bmi >= 25.0]

-- cylinder function
cylinder :: (RealFloat a) => a -> a-> a
cylinder r h = 
        let sideArea = 2 * pi * r * h
            topArea = pi * r^2
        in sideArea + 2*topArea

-- equivalent statements:
--head' :: [a] -> a
--head' [] = error "list empty"
--head' (x:_) = x
-- or
head'' :: [a] -> a
head'' xs = case xs of [] -> error "list empty"
                       (x:_) -> x
-- describe list using cases
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
        where what [] = "empty."
              what [x] = "a singleton list."
              what xs = "a longer list."

-- fibonacci attempt
fibon 0 = 0
fibon 1 = 1
--fibon 2 = 1
fibon n = fibon (n-1) + fibon (n-2)

-- for loop test
--forl :: 
--forl 100 = 100
--forl n = print n forl (n+1)

--recursve maximum
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- replicate fn
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
        | n <= 0 = []
        | otherwise = x:replicate' (n-1) x

-- list length function
--length'' :: (Int a) => [a] -> a
length'' [] = 0
length'' [x] = 1
length'' (x:xs) = 1 + length'' xs

-- edge funct
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
        | n <= 0        = []
take' _ []              = []
take' n (x:xs)          = x : take' (n-1) xs

-- reverse func
reverse' :: [a] -> [a]
reverse' [] =[]
reverse' (x:xs) = reverse' xs ++ [x]

-- inf list!
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- elem - this is cool
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
        | a == x        = True
        | otherwise     = a `elem'` xs

--quicksort impl
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
        let smallerSorted = quicksort [a | a <- xs, a <= x]
            biggerSorted = quicksort [a | a <- xs, a > x]
        in smallerSorted ++ [x] ++ biggerSorted

-- oh my shit: Often the edge case value turns out to be an identity. The identity for multiplication is 1 because if you multiply something by 1, you get that something back. Also when doing sums of lists, we define the sum of an empty list as 0 and 0 is the identity for addition. In quicksort, the edge case is the empty list and the identity is also the empty list, because if you add an empty list to a list, you just get the original list back.





