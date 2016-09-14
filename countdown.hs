import Control.Monad
import Data.List

--inDict inDict inWord = if inWord `elem` inDict then True else False
allWords dictionary word = filter (\x -> x `elem` dictionary) . concat $ [ permutations $ take x word | x <- [1..(length word)]]

main = forever $ do
    putStrLn "Please type some letters: "
    inputWord <- getLine
    file <- readFile "/usr/share/dict/cracklib-small"
    let dict = lines file
    let legitWords = allWords dict inputWord
    print legitWords


-- I think I need to split this into a dictionary load then a word finder but I don't know how to do that yet.
