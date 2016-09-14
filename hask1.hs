doubleMe x = 2 * x
doubleUs x y = x^2 + y^2
doubledouble x y = doubleMe x + doubleMe y
static = 100

doubleSmallNumber x = if x >100
	then x
	else x * 2

doubleSmallNumber' x = (if x > 100 then x else x^2) + 1
myname = "My name's unimportant!"

putADonkOnIt x = "donk":x

steve = "Steve Buscemi"

nthElement x n = x !! n


-- attempt at making an infinite series: maybe the basel problem? 
basel' n = 1/(n^2)
baselDiff n = (pi^2/6 - sum [basel' x | x <- [1..n]])

-- square integers less than 9000
mySquares = [x^2 | x <- [0,1..(sqrt 9000)], x^2 < 9000]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- can't do fizz buzz because you cant mix list data types
-- fizzbuzz xs = [if mod x 3 == 0 then "Fizz!" else x | x <- xs ]

-- own length function
length' xs = sum [1 | _ <- xs ]

-- remove all but uppercase letters, here 'elem' is like "in" or "is an element of..
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- generate some triangles
triangles = [ (a,b,c) | c <- [1..10], b<-[1..10], a<-[1..10] ]
rightTriangles x = [ (a,b,c) | a <- [1..x], b <-[1..x], c <-[1..x], a^2 + b^2 == c^2 ]
rightTrianglesMaxCirc x y = [ (a,b,c) | a <- [1..x], b <-[1..x], c <-[1..x], a^2 + b^2 == c^2, a+b+c == y]

-- declare types beforehand using functionname :: input_type1 -> inputypen -> outputtype1 -> etc
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- assigning some types to functions
factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circ' :: Double -> Double
circ' r = 2*pi*r

-- trying to find out when (if ever) (x+1)^x > x^(x+1)
greaterTest = [ (x,y) | x <- [-100..100], y <- [-100..100], x**y == y**x && x /= y]



