-- Scrabble testing
import Data.List

boardFromWord:: String -> [[Maybe Char]]
boardFromWord (x:[]) = [[Just x]]
boardFromWord (x:xs) = [Just x] : boardFromWord xs

numOcc :: Char -> String -> Int
numOcc x ys = length [y | y <- ys, y == x]

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


{--
-- "aecdb" "b:edxac" , check b in fist string -> yes -> remove from first string
-- "aecd" "e:dxac" - yes, ditto
--"acd" "d:xac" - yes, ditto
--}
subtest xs (y:ys)
    | y `elem` xs = subtest (delete y xs) ys
    | otherwise = [y] ++ (subtest xs ys)
subtest xs [] = []
subtest [] ys = ys

-- when the result of // is empty - that means the first fits in the second and we have to find
-- the difference, namely all the elements in the right that have not been used.
-- the use Just to return result
-- in all other cases, namely if the function returns anything, we ouput Nothing

-- Given a word, a list of letters on your rack, and the
-- intersection point letter c, determine whether you can form the word on the
-- board on that intersection point by adding letters from your rack. 
-- If so, return the letters that are left over. They do not need to be in order.
--
-- formable "exercise" "seeqcixez" 'r' = Just "qz"
-- formable "exercise" "seeqcixez" 'x' = Nothing

-- WARNING- changed type signature of Rack.
formable :: String -> String -> Char -> Maybe String
formable xs rs y = submultiset (xs \\ [y]) rs

