import Data.List

-- split an algebraic expression into terms
splitAlg expr = groupBy (\x y -> (y /= '+')&&(y /= '-')) expr

-- function to find variable positions in an equation
whereVar [] var = Nothing 
--whereVar (x:xs) var = 

-- function to classify equations
{-classify :: [Char] -> [Char]
classify expr = -}

-- solve quadratic equations
solveQuad a b c = [((-b) + (sqrt (b**2 + (-4*a*c))))/(2*a), ((-b) - (sqrt (b**2 + (-4*a*c))))/(2*a)] 


