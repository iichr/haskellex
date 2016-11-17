-- Scrabble testing
import Data.List

type Score = Int
type Board = [[Maybe Char]]
type Dict = [String]

-- Exercise
boardFromWord:: String -> Board
boardFromWord (x:[]) = [[Just x]]
boardFromWord (x:xs) = [Just x] : boardFromWord xs

-- Exercise
numOcc :: Char -> String -> Int
numOcc x ys = length [y | y <- ys, y == x]

-- Exercise
-- Given two words, determine whether you can make
-- the left word with the letters of the right word. 
-- You do not need to use all letters of the right word. 
-- If it does work, return a
-- string with the leftover letters. They do not need to be in order.
--
-- submultiset "aecdb" "bebdcaxa" = Just "xba"
-- submultiset "aecdeb" "bebdcaa" = Nothing


submultiset :: String -> String -> Maybe String
submultiset xs ys
    | xs \\ ys == [] = Just (ys \\ xs)
    | otherwise = Nothing


{-
"aecdb" "b:edxac" , check b in fist string -> yes -> remove from first string
"aecd" "e:dxac" - yes, ditto
"acd" "d:xac" - yes, ditto

-- Observations:
-- when the result of // is empty - that means the first fits in the second and we have to find
-- the difference, namely all the elements in the right that have not been used.
-- the use Just to return result
-- in all other cases, namely if the function returns anything, we ouput Nothing

subtest xs (y:ys)
    | y `elem` xs = subtest (delete y xs) ys
    | otherwise = [y] ++ (subtest xs ys)
subtest xs [] = []
subtest [] ys = ys

-}


-- Exercise
-- Given a word, a list of letters on your rack, and the
-- intersection point letter c, determine whether you can form the word on the
-- board on that intersection point by adding letters from your rack. 
-- If so, return the letters that are left over. They do not need to be in order.
--
-- formable "exercise" "seeqcixez" 'r' = Just "qz"
-- formable "exercise" "seeqcixez" 'x' = Nothing

-- WARNING- changed type signature of Rack to String. REPLACE in original solution
formable :: String -> String -> Char -> Maybe String
formable xs rs y = submultiset (xs \\ [y]) rs

-- Utility code given to you:
letterValue :: Char -> Score
letterValue c | c `elem` "aeioulnstr" = 1
letterValue c | c `elem` "dg" = 2
letterValue c | c `elem` "bcmp" = 3
letterValue c | c `elem` "fhvwy" = 4
letterValue c | c `elem` "k" = 5
letterValue c | c `elem` "jx" = 8
letterValue c | c `elem` "qz" = 10
letterValue c | otherwise = error "not a valid letter"

-- Exercise, basic. Make a function to compute the value of a word.
wordValue :: String -> Score
wordValue xs = sum [letterValue x| x <- xs]
-- Testing code
wordValueTests =  [wordValue "abacus" == 1+3+1+3+1+1,
                    wordValue "xylophone" == 8+4+1+1+3+4+1+1+1]

-- Exercise, basic.
-- Given a board, rotate it 180 degrees. The resulting board has the same
-- number of rows and columns as the input.
invertBoard :: Board -> Board
invertBoard = undefined


-- Letter Dict testing
-- only returns the length until the first instance
lengthBeforeLetter :: String -> Char -> Int
lengthBeforeLetter (x:xs) c 
    | x==c = 0
    | otherwise = 1 + (lengthBeforeLetter xs c)

lengthAfterLetter :: String -> Char -> Int
lengthAfterLetter (x:xs) c
    | x==c = length xs
    | otherwise = (lengthAfterLetter xs c) 

-- Better Attempt: BEGIN
-- Which words fit?
allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 dict c x y = 

-- first get all words with that letter : [String]
initialWords = [initialWord | initialWord <- allWords1 dict c, length(InitialWord)<=(x+y+1)]

-- for every element in the list get where the CHAR is at
elementPositions = map (elemIndices c) initialWords

-- zip the resulting lists together:
wordsWithIntersectionIndices = zip initialWords elementPositions

-- for every pair in the list of the type below:
-- [ ("tetkaest",[1,5]) , ("test",[1]) ]

if length $ snd $ (choose from list) = 1 then
    splitAt (head $ snd $ choose from list)+1 choose from list 
    -- we obtain the tuple of the split in the form ("te","st")
    if length (fst of the above tuple)-1 <= x AND 
    length (snd of the above tuple) <=y
    -- then it fits the gap basically
    -- return the word and the anchor position
    zip [fst (choose from list) ] (snd (choose from list))

else 
    -- case ("tetkaest", [1,5])
    -- go one by one:
    splitAt (head $ snd $ choose from list) + 1 choose from list
    if length (fst of above)-1 <=x AND length(snd of above) <=y
    zip [fst (from list)] [ head $ snd (choose from list)] :
    apply same function to rest of list

-- implementation of the above
tupConverter :: ([Char], [Int]) -> [([Char], Int)]
tupConverter ent 
    | (length $ snd $ ent ) == 1 =
        splitIntersect = splitAt ((head $ snd $ ent) + 1) (fst $ ent)
        if (length (fst $ splitIntersect) - 1) <= x &&
            length (snd splitIntersect) <=y
        then zip (fst $ ent) [(head $ snd $ ent) + 1]


