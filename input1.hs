import Data.List

allCoins = [0,0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50]
smallCoins = filter (<= 1) allCoins
largeCoins = filter (>= 1) allCoins

-- need minimum 6 coins to make all possible change combinations

changeFunc cst mon = 8

changeCalc = do
    putStrLn "enter the cost"
    cost <- getLine
    putStrLn "enter your money"
    money <- getLine
    if cost > money
      then do putStrLn "please enter new money amount"
              money <- getLine
              changeCalc
      else do putStrLn "here's your change"
              let changeBase = (read money :: Float) - (read cost :: Float)
              putStrLn ("your change is " ++ (show changeBase))

main = changeCalc

{- main = do
    putStrLn "Please enter the cost of the item:"
    cost <- getLine
    putStrLn "Enter the money paid:"
    money <- getLine -}
