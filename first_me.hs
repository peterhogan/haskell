--days :: Int -> String
days 0 = "Monday"
days 1 = "Tuesday"
days 2 = "Wednesday"
days 3 = "Thursday"
days 4 = "Friday"
days 5 = "Saturday"
days 6 = "Sunday"

daysAWeek interval = map days $ take 7 [ mod (x-interval) 7 | x <- [ interval * y | y <- [1..] ] ]

numberWeekdays interval = take 7 [ mod (x-interval) 7 | x <- [ interval * y | y <- [1..] ] ]

--greaterFunction = 

commute mins "weekly" = show ((mins * 10) / 60) ++ " hours a week."
commute mins "monthly" = show ((mins * 10 * 4) / 60) ++ " hours a month."
commute mins "yearly" = show ( round (((mins * 2 * 230) / 60) / 24)) ++ " days a year."
commute mins _ = "Please enter either weekly, monthly or yearly as second argument."
