--import Data.Map as Map
import Data.Char
-- factorial function
--factorial :: (Num a) => a -> a
factorial 0 = 1
factorial n = product [1..n]

-- caesar cipher

encode :: Int -> String -> String
encode shift msg =
        let ords = map ord msg
            shifted = map (+ shift) ords
        in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg 

-- phonebook
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ] 

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

--phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
--phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
