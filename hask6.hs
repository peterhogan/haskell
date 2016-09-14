import qualified Data.Map as Map
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
inPhoneBook :: Name -> PhoneNumber -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
        case Map.lookup lockerNumber map of
            Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
            Just (state, code) -> if state /= Taken
                                    then Right code
                                    else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
