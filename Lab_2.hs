--23. Залишити у списку елементи, що входять у нього тричі.
import Data.List

list :: [Integer]
list = [0, 0, 0, 1, 1, 1, 4, 4, 4, 4, 7, 7, 7, 2, 2, 2, 10, 10 , 10]

maxInt :: Integer
maxInt = maximum list

sortedList :: [Integer]
sortedList = (maxInt + 1) : sort list ++ [maxInt + 1]


listThree :: [Integer]
listThree = [x2 | (x1, x2, x3, x4, x5) <- zip5 sortedList (drop 1 sortedList) (drop 2 sortedList) (drop 3 sortedList) (drop 4 sortedList), 
    x2 == x3 && x3 == x4 && x1 /= x2 && x4 /= x5]
