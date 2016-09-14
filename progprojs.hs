e = 2.718281828459045235360287471352662497757247093699959574966967627724076630353

fact :: (Enum a, Eq a, Num a) => a -> a
fact 0 = 1
fact n = product [1..n]

piTerm i = (((fact i)**2)*2**(i+1))/(fact $ (2*i + 1))

piSum n = sum [ piTerm x | x <- [0..n]] 

eTerm i = 1/(fact i)
eSum n = sum [eTerm x | x <- [0..n]]

