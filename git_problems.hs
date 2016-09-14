-- factorial function
fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

pi' = g(1,0,1,1,3,3) where
    g(q,r,t,k,n,l) = 
        if 4*q+r-t<n*t
        then n : g(10*q,10*(r-n*t),t,k,div(10*(3*q+r))t-10*n,l)
        else g(q*k,(2*q+r)*l,t*l,k+1,div(q*(7*k+2)+r*l)(t*l),l+2)

prntList :: Show a => [a] -> [Char]
prntList [] = " "
prntList [x] = show x
prntList (x:xs) = show x ++ prntList xs

-- e spigot function
eee = f(2, 1) where
    f(s,a) =
        s : f(mod (10*s) a+1,a+1)

-- test 
test = f(2, 1, 10) where
    f(s,a,b) =
        s : f((mod (b*s) a+1),a+1,b*10)


stream :: Integral a => (a, a) -> (a, a, a) -> [(a, a, a)] -> [a]
stream (lo, hi) z ~(x:xs) = if lbound == approx z hi
    then lbound : stream (lo, hi) (mul (10, -10*lbound, 1) z) (x:xs)
    else stream (lo, hi) (mul z x) xs
    where lbound = approx z lo
          approx (a,b,c) n = div (a*n + b) c
          mul (a,b,c) (d,e,f) = (a*d, a*e + b*f, c*f)

streamDigits :: (Num a, Integral a, Enum a) => (a -> (a, a, a)) -> (a, a) -> [a]
streamDigits f range = stream range (1,0,1) [(n, a*d, d) | (n,d,a) <- map f [1..]]

stream_pi, stream_e :: [Integer]
stream_pi = streamDigits (\k -> (k, 2*k + 1, 2)) (3, 4)
stream_e  = streamDigits (\k -> (1, k      , 1)) (1, 2)

{- A happy number is defined by the following process. Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers, while those that do not end in 1 are unhappy numbers. Display an example of your output here. Find first 8 happy numbers. -}

split :: Int -> [Int]
split 0 = []
split n = split (div n 10) ++ [mod n 10]

sumsplit = sum . (map (^2)) . split

happy :: Int -> Bool
happy 1 = True
happy 4 = False
happy n = happy $ sumsplit n 

-- binary convert function
toBinary 0 = []
toBinary n = (toBinary $ div n 2) ++ [mod n 2]

fromBinary n = sum $ [z*w | (z,w) <- (zip [2^m | m <-[(length $ split n)-1,(length $ split n)-2..0]] (split n))]















