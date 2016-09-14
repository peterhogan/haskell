import Data.List

-- useful functions
-- nth triangular number
triangle 1 = 1
triangle n = n + triangle (n-1)

-- all divisors 
divs m = [n | n <- [1..m], (mod m n == 0)]

-- make a number its integral square root
makeint :: Int -> Int
makeint = floor . sqrt . fromIntegral
--generate a list of them
sqrtoddlist :: Int -> [Int]
sqrtoddlist x = [3,5..makeint x]
-- prime checker
isPrime :: Int -> Bool
isPrime x
    | x == 2        = True
    | x == 1        = False
    | x <= 0        = False
    | mod x 2 == 0  = False
isPrime x = not $ elem 0 [ mod x y | y <- sqrtoddlist x ]

-- list all prime factors
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime

-- prime list: list of primes less than a given int
primelist x
    | x <= 1        = []
    | otherwise     = filter (\y -> isPrime y == True) $ 2 : [3,5..x]

--factorial :: Int -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)

--split a number into its digits
split :: Int -> [Int]
split 0 = []
split n = split (div n 10) ++ [mod n 10]

--split' :: Integer -> [Integer]
split' 0 = []
split' n = split (div n 10) ++ [mod n 10]
-- unsplit the split function
unsplit :: [Int] -> Int
unsplit ns = sum [ a * b | (a,b) <- (zip (iterate (*10) 1) (reverse ns))]

-- problem 1
problem1 n = sum $ filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) [1..(n-1)]

-- problem 2
fibs 1 = 1
fibs 2 = 2
fibs n = fibs (n-1) + fibs (n-2)

problem2 k = filter (\x -> mod x 2 == 0) $ takeWhile (\y -> y < k) [fibs n | n<-[1..]]

-- problem 4
isPalindrome n = (split n) == (reverse $ split n)

threePalindromes = maximum $ filter (isPalindrome) [a*b | a<-[100..999],b<-[100..999]]  

-- problem 5
smallestMult n = head [ m | m<-[1..], (sum [mod m n | n<-[1..n]] == 0)]

-- problem 529: 10 - substrings
{- A 10-substring of a number is a substring of its digits that sum to 10. A number is called 10-substring-friendly if every one of its digits belongs to a 10-substring/ 
example: 3523014 is 10-substring-friendly-}

-- takes a list and splits it into sublists in order
takeGroup :: [Int] -> [[Int]]
takeGroup xs = [take x xs | x <- [2..(length xs)]]

-- removes an element at a time from a list
dropEach :: [Int] -> [[Int]]
dropEach xs = [drop x xs | x <- [0..(length xs)-1]]

-- calculates all the ten-substrings of a number
tenSubs :: Int -> [[Int]]
tenSubs xs = filter (\x -> sum x == 10) $ concat $ map takeGroup $ dropEach $ split xs

-- should say whether or not the number is ten-substring friendly but doesn't

{- need to filter the numbers out in a quicker way before doing the final check
NEED TO WORK OUT HOW TO REMOVE FALSE POSITIVES LIKE 999991 THAT COME FROM REPEATED INTEGERS-}

tenSubFriendly :: Int -> Bool
tenSubFriendly x = not $ elem False [elem a (concat $ tenSubs x) | a <- (split x)]

-- counts as per project euler question (when tenSubFriendly works)
tee n = length $ filter (\x -> tenSubFriendly x == True) [1..10^n]


-- Problem 494 - Collatz prefix Families
{- Collatz seq is:
a_(i+1) = a_i/2 if a_i is even and 3a_i+1 if a_i is odd
p(n) with a_1 = n is the sequence of Collatx numbers excluding powers of two (incl 2^0)
p(13) = [13,40,20,10,5]
p(8) = []
S_m is the set of all p(n) sequence prefixes of length m
two sequences (a_m) and (b_m) belong to the same prefix family if a_i < a_j iff b_i < b_j for all 1<=i,j<=m
let f(m) be the number of distinct prefix families in S_m
f(5) = 5
f(10) = 55
f(20) = 6771
f(90) = ?-}

-- collatz
collatz :: Int -> [Int]
collatz 1 = []
collatz n
    | mod n 2 == 0  = n : collatz (div n 2)
    | otherwise     = n : collatz (3*n + 1) 

--powers of two generator
twoToN :: Int -> Int
twoToN n = last $ takeWhile (\x -> x <= n) [2^n | n <-[1..]]

-- collatz without powers of two
pFunct n = filter (\x -> x /= (twoToN x)) $ collatz n 

-- problem 12: divisors of triangular numbers
theLargest = head $ filter (>500) $ map (length . divs . triangle) [1..]

-- problem 16: power digit sum
digitsSum = sum . split
-- problem 35 circular primes
-- SOLVED!
cycle' :: [Int] -> [[Int]]
cycle' xs = xs : cycle' (tail xs ++ [head xs]) 

cyclic n = map unsplit $ take (length $ split n) (cycle' $ split n)

cyclicPrime n = notElem False (map isPrime $ cyclic n) 

cyclicPrimes n = filter (\x -> cyclicPrime x) [1..n]

cyclicPrimesLessThan n = length $ cyclicPrimes n

-- problem 484: Arithmetic Derivative
factors n = head [[x,(div n x)] | x <- [2..], (mod n x == 0)]

arith n
    | isPrime n == True     = 1
    | otherwise             = (arith a)*b + a*(arith b)
    where a = head $ factors n
          b = last $ factors n  
                    
arithGcd m = gcd m (arith m)
-- problem 495

test495 n = filter (\[a,b,c] -> a*b*c == n) [[x,y,z] | x<-[1..(div n 2)],y<-[1..(div n 2)],z<-[1..(div n 2)], x/=y,x/=z,y/=z]
