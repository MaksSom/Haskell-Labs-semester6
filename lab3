--13. Виявити, чи допускає скінчений автомат хоча б одне слово, що може бути подане у 
-- вигляді xxx для деякого слова x. При ствердній відповіді навести приклад відповідного слова xxx.

import Data.List ( isPrefixOf, nub)
import Data.Char (isLower)
import System.IO
    ( hClose, hGetLine, openFile, Handle, IOMode(ReadMode) )
import Data.List.Split (splitOn)


divideRule :: [Char] -> [[Char]]
divideRule word = 
    let(left, _:right) = break (== '-') word 
    in [left, right]

-- transform string into [Char] skipping spaces and . -> []
readInstruction :: String -> [Char] -> [Char]
readInstruction [] answer = answer
readInstruction ['.'] answer = answer
readInstruction (symbol:rest) answer
    |symbol == ' ' = readInstruction rest answer
    |otherwise = readInstruction rest (answer ++ [symbol])

-- Function to read the word and rules from the file
readWordAndRules :: FilePath -> IO ([Char], [[[Char]]])
readWordAndRules filePath = do
    content <- readFile filePath
    let (keyWord:numOfRules:rules) = lines content
        word = readInstruction keyWord []
        numRules = read numOfRules :: Int
        rulesParsed = take numRules rules
    return (word, rewriteRules rulesParsed)

rewriteRules :: [String] -> [[[Char]]]
rewriteRules [] = []
rewriteRules (rule:rx) =
    let index = locateSublist ['-', '>'] (readInstruction rule [])
    in [take index (readInstruction rule []), drop (index + 2) (readInstruction rule [])] : rewriteRules rx



-- word we want to check and rules
findAnswer :: [Char] -> [[[Char]]] -> [Char]
findAnswer [] _ = []
findAnswer keyWord ((y:ys):xs)
    | checkConditions keyWord (searchAllWords ys xs (3 * length keyWord)) = keyWord ++ keyWord ++ keyWord
    | otherwise = []

-- Searching for "xxx" word
checkConditions :: [Char] -> [[Char]] -> Bool
checkConditions keyWord completedWords
    | (keyWord ++ keyWord ++ keyWord) `elem` completedWords = True
    | otherwise = False

-- Input: words, rules, limit
searchAllWords :: [[Char]] -> [[[Char]]] -> Int -> [[Char]]
searchAllWords words rules limitInt
    |   null words = []
    |   otherwise = 
            let nextWords = nextStepOfWords words rules 
            in [completedWord | completedWord <- nextWords, all isLower completedWord] ++ searchAllWords (removeUnnecessaryWords nextWords limitInt) rules limitInt

-- Removing words with length > limit + 1 and completed
removeUnnecessaryWords :: [[Char]] -> Int -> [[Char]]
removeUnnecessaryWords words limit = [word | word <- words, length word <= limit + 1, not(all isLower word)]


--Rules: [[[input], [output]], [[input], [output]], [[input], [output]]....]
--Words are possible words in this step. 
nextStepOfWords :: [[Char]] -> [[[Char]]] -> [[Char]]
nextStepOfWords words rules = deleteRepeatAndNull [ useRule word rule | word <- words, rule <- rules]

deleteRepeatAndNull :: [[Char]] -> [[Char]]
deleteRepeatAndNull = filter (not . null) . nub

--Input: sublist we need to find and then list where we are searching
--Output: index of the beginning if found and -1 if not
locateSublist :: [Char] -> [Char] -> Int
locateSublist [] _ = -1
locateSublist _ [] = -1
locateSublist sublist list = go 0 list
  where
    go _ [] = -1
    go index (x:xs)
      | sublist `isPrefixOf` (x:xs) = index
      | otherwise = go (index + 1) xs

--Input: word that we are checking (applying the rule to) and the rule
-- rule: [[input], [output]]
-- Output: new word created by the rule or the same word if rule can't be applied
useRule :: [Char] -> [[Char]] -> [Char]
useRule [] _ = []
useRule word [inp, out]
    | locateSublist inp word < 0 = []
    | locateSublist inp word >= 0 = 
        let index = locateSublist inp word 
        in take index word ++ out ++ drop (index + length inp) word



main :: IO ()
main = do
    (word, rules) <- readWordAndRules "input.txt"

    putStrLn "Word:"
    print word

    putStrLn "Rules:"
    print rules

    putStrLn "Result:"
    let answer = findAnswer word rules
    
    print answer
