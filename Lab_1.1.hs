--33. Залишити у першому списку елементи, що входять у другий список тричі.
import Data.List

list_1 :: [Integer]
list_1 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 10]
list_2 :: [Integer]
list_2 = [0, 0, 0, 1, 1, 1, 4, 4, 4, 4, 7, 7, 7, 2, 2, 2, 10, 10 , 10]

maxInt :: Integer
maxInt = maximum list_2

sortedList :: [Integer]
sortedList = (maxInt + 1) : sort list_2 ++ [maxInt + 1]


listThree :: [Integer]
listThree = [x2 | (x1, x2, x3, x4, x5) <- zip5 sortedList (drop 1 sortedList) (drop 2 sortedList) (drop 3 sortedList) (drop 4 sortedList), 
    x2 == x3 && x3 == x4 && x1 /= x2 && x4 /= x5]

listResult :: [Integer]
listResult = [x | x <- list_1, x `elem` listThree]
