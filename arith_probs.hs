import Data.List

-- make a number its integral square root
makeint :: Int -> Int
makeint = floor . sqrt . fromIntegral 
--generate a list of them
sqrtoddlist :: Int -> [Int]
sqrtoddlist x = [3,5..makeint x]

-- prime checker
isprime :: Int -> Bool
isprime x
    | x == 2        = True
    | x == 1        = False
    | x <= 0        = False
    | mod x 2 == 0  = False
isprime x = not $ elem 0 [ mod x y | y <- sqrtoddlist x ]

-- find the next prime
nextprime :: Int -> Int
nextprime x
    | isprime x == True     = x
    | isprime x == False    = nextprime (x+1)

-- find the last prime 
lastprime :: Int -> Int
lastprime x
    | isprime x == True     = x
    | isprime x == False    = nextprime (x-1)

-- euclids algortithm 
euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (mod a b)

-- coprime checker
coprime :: Int -> Int -> Bool
coprime x y
    | euclid x y == 1   = True
    | otherwise         = False

-- Euler's totient function
totient :: Int -> Int
totient x = sum [ if coprime x y then 1 else 0 | y <- [1..(x-1)] ]

toti :: Int -> Int
toti x = length $ filter (\y -> coprime x y == True) [1..x]

-- prime list: list of primes less than a given int
primelist x 
    | x <= 1        = []
    | otherwise     = filter (\y -> isprime y == True) $ 2 : [3,5..x]

primesbetween :: Int -> Int -> [Int]
primesbetween a b = filter (\y -> isprime y == True) [a..b]

-- divisor and prime divisor functions
divisors :: Int -> [Int]
divisors x = filter (\y -> mod x y == 0) [1..x]

primedivs :: Int -> [Int]
primedivs x = filter (\y -> isprime y == True) $ divisors x

-- list all prime factors
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime


-- goldbach function
allgoldb :: Int -> [[Int]]
allgoldb n 
    | mod n 2 /= 0  = [[0,0]]
    | otherwise     = let goldbachpairs = [[primelist n !! x, primelist n !! y] | x <- [0..(length $ primelist n)-1], y <- [0..(length $ primelist n)-1]]
                      in let goldpositions = filter (\z -> (map sum $ goldbachpairs) !! z  == n) [0..(length $ goldbachpairs)-1]
                      in [ goldbachpairs !! w | w <- goldpositions ]

goldb :: Int -> [Int]
goldb n 
    | mod n 2 == 0  = let goldbachpairs = [[primelist n !! x, primelist n !! y] | x <- [0..(length $ primelist n)-1], y <- [0..(length $ primelist n)-1]]
                      in let goldpositions = filter (\z -> (map sum $ goldbachpairs) !! z  == n) [0..(length $ goldbachpairs)-1]
                      in goldbachpairs !! (head goldpositions)
    | otherwise     = [0,0]
