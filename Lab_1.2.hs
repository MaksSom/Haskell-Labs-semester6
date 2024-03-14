--8. Зі списку вилучити N найбільших його елементів.
import Data.List

list :: [Integer]
n :: Int
list = [0, 1, 1, 2, 2, 3, 4, 61, 672, 31, 354, 6, 12, 324, 5, 134, 1, 1, 2, 4, 0, -1]
n = 5

listResult :: [Integer]
listResult = take n (reverse (sort list))
