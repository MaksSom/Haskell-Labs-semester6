--13. Забезпечити перевірку LL(1)-властивості граматики

import Data.List ( isPrefixOf, nub)
import System.IO
    ( hClose, hGetLine, openFile, Handle, IOMode(ReadMode) )
import Data.Char (isUpper, isLower)
import Control.Exception

-- transform string into [Char] skipping spaces and . -> []
readInstruction :: String -> [Char] -> [Char]
readInstruction [] answer = answer
readInstruction ['.'] answer = answer
readInstruction (symbol:rest) answer
    |symbol == ' ' = readInstruction rest answer
    |otherwise = readInstruction rest (answer ++ [symbol])

-- Function to read the rules from the file
readWordAndRules :: FilePath -> IO [[[Char]]]
readWordAndRules filePath = do
    content <- readFile filePath
    let (numOfRules:rules) = lines content
        numRules = read numOfRules :: Int
        rulesParsed = take numRules rules
    return (rewriteRules rulesParsed)

rewriteRules :: [String] -> [[[Char]]]
rewriteRules [] = []
rewriteRules (rule:rx) =
    let index = locateSublist ['-', '>'] (readInstruction rule [])
    in [take index (readInstruction rule []), drop (index + 2) (readInstruction rule [])] : rewriteRules rx

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


--Умова лівого: якщо A -> [порожнє]A[непорожнє]
--Проблема неоднозначної граматики невирішувана за скінченну кількусть часу. Тому використаємо пошук слів по кількості ітерацій (наприклад за перші 50 кроків)


--removing all lowercase characters
removeLowercase :: [Char] -> [Char]
removeLowercase  = filter isUpper

--Returns all nonterminal symbols that should be checked for "left rule"
getAllNonterminals :: [[[Char]]] -> [Char]
getAllNonterminals rules = removeLowercase (nub [head (head rule) | rule <- rules]) --head takes the first variable of the list / nub removes duplicate elements

--Checks left rule for all nonterminals in the rules
checkLeftRule :: [[[Char]]] -> Bool
checkLeftRule rules = 
    or ([ searchAllWordsForLeftRule [[nonterm]] rules nonterm | nonterm <- getAllNonterminals rules])

-- Input: words, rules, limit
-- Answering the question is there a loop
searchAllWordsForLeftRule :: [[Char]] -> [[[Char]]] -> Char -> Bool
searchAllWordsForLeftRule words rules nonterm = go words
    where
        go [] = False
        go ws
            | any (checkStartingNonterm nonterm) ws = True --Stop when found left case 
            | otherwise = 
                let nextWords = nextStepOfWords ws rules
                    filteredWorlds = removeTerminalWords nextWords
                in go filteredWorlds

-- Checks if any word from the list starts with Nonterminal
checkStartingNonterm :: Char -> [Char] -> Bool
checkStartingNonterm nonterm word = head word == nonterm

-- Removing words started from termonal
removeTerminalWords :: [[Char]] -> [[Char]]
removeTerminalWords words = [  w : wx | (w:wx) <- words, isUpper w]

--Rules: [[[input], [output]], [[input], [output]], [[input], [output]]....]
--Words are possible words in this step. 
nextStepOfWords :: [[Char]] -> [[[Char]]] -> [[Char]]
nextStepOfWords words rules = deleteRepeatAndNull [ useRule word rule | word <- words, rule <- rules]

deleteRepeatAndNull :: [[Char]] -> [[Char]]
deleteRepeatAndNull = filter (not . null) . nub

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


-- Input: words, rules, limit for № of itrations
searchAllWords :: [[Char]] -> [[[Char]]] -> Int -> [[Char]]
searchAllWords words rules limitInt
    |   limitInt <= 0 = []
    |   null words = []
    |   otherwise = 
            let nextWords = nextStepOfWords words rules 
            in nextWords ++ searchAllWords nextWords rules (limitInt - 1)

-- checking dublicate words
checkAmbiguousGrammar :: [[[Char]]] -> Int -> Bool
checkAmbiguousGrammar ((firstWord:fRule):otherRules) limitInt =
    let allWords = searchAllWords [firstWord] ((firstWord:fRule):otherRules) limitInt
    in length allWords /= length (nub allWords)
    

main :: IO ()
main = do
    rules <- readWordAndRules "input.txt"

    putStrLn "Rules:"
    print rules

    putStrLn "Result:"

    putStr "Left rucursive:  "
    print (checkLeftRule rules)

    putStr "Ambiguity:  "
    print (checkAmbiguousGrammar rules 5)
