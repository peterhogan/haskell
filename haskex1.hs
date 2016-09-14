import Data.Char

main = do
        putStrLn "Hello, what's your name?"
        firstName <- getLine
        putStrLn "What's your last name?"
        lastName <- getLine
        let bigFirst = map toUpper firstName
            bigLast = map toUpper lastName
        putStrLn $ "hey " ++ bigFirst ++ " " ++ bigLast ++ ", you prick."
