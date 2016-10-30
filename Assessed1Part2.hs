{-# LANGUAGE Safe #-} 
module Assessed1Part2 where

-- We begin with a sample solution of part 1, produced by Cory.

import Data.Maybe
import Data.Char
import Data.Function
import Data.List

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

readBit :: Char -> Bit
readBit '0' = Z
readBit '1' = I

readBits :: [Char] -> [Bit]
readBits = map readBit

decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (tree, bits) = decodeAux tree tree bits

decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux fullTree (Leaf c _) [] = [c]
decodeAux fullTree (Leaf c _) bs = c:(decodeAux fullTree fullTree bs) 
decodeAux fullTree (Branch left right _) (Z:bs) = decodeAux fullTree left bs
decodeAux fullTree (Branch left right _) (I:bs) = decodeAux fullTree right bs

{- The input String has the following format:

   * An integer n coded as a sequence of digits.

   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

decompress :: String -> String
decompress str = decode (t,bits)
    where
        (n',str') = span isDigit str
        n         = read n'
        t'        = take n str'
        t         = read t'
        bits      = readBits $ drop n str'

{- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding. -}

charlength :: Int
charlength = 8

-- gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*':s)   = s
decompress' s = decompress s

-- Generate the frequency table
-- An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

-- Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate = foldr update []

-- Removes the existing entry for c (if it exists), updates it, and
-- then reinserts it if no entry exists, we start over at 0, and then
-- "update"
update :: Eq c => c -> [Freq c] -> [Freq c]
update c keys = newFreq : rest
    where
        (old,rest) = (is c) `outOf` keys
        key = fromMaybe (c,0) old
        newFreq = mapSnd (+1) key

is :: Eq c => c -> Freq c -> Bool
is c (d,_) = c == d

outOf :: (a -> Bool) -> [a] -> (Maybe a,[a])
outOf p []     = (Nothing,[])
outOf p (x:xs) = if (p x) then (Just x,xs) else (mapSnd (x:) $ outOf p xs)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (c,a) = (c, f a)

{- End of part 1. Your tasks for part 2 begin here. -}

-- Produce a Huffman tree from a list of Huffman trees.
-- https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-- Question:
makeTree :: [Tree c] -> Tree c
-- Collects a list of trees into an optimal prefix tree.
makeTree (t1:[]) = t1
makeTree (t1:t2:[]) = merge t1 t2
makeTree (t1:t2:ts) = makeTree tsordered
    where
        tsordered = insertCorrect (merge t1 t2) (tail(tail(t1:t2:ts)))

--CHR
-- Convert the frequency pairs to leaves
leafify :: [Freq c] -> [Tree c]
leafify xs = map ((\(k,v) -> Leaf k v)) xs


--CHR
-- Sorting the frequency list in ascending order
-- !! Only works on list of frequencies not with trees.
sortByFrequency :: [Freq c] -> [Freq c]
sortByFrequency xs = sortBy (compare `on` snd) xs

insertCorrect :: Tree c  -> [Tree c] -> [Tree c]
insertCorrect t xss = xs ++ [t] ++ ys
    where
        xs = [x | x <- xss, getVal x <= getVal t]
        ys = [y | y <- xss, getVal y > getVal t]

getVal :: Tree c -> Int
getVal (Branch _ _ v) = v
getVal (Leaf _ x) = x

-- You may wish to use a helper function such as this:
merge :: Tree c -> Tree c -> Tree c
merge (Branch t1 t2 p) (Leaf a q) = Branch (Leaf a q) (Branch t1 t2 p) (p+q)
merge (Leaf a q)  (Branch t1 t2 p) = Branch (Leaf a q) (Branch t1 t2 p) (p+q)
merge (Leaf a p) (Leaf b q) = Branch (Leaf a p) (Leaf b q) (p+q)
merge (Branch t1 t2 p) (Branch t3 t4 q) = Branch (Branch t1 t2 p) (Branch t3 t4 q) (p+q)

--CHR
-- TESTING leafify, merge and makeTree
-- Initial setup: 
testleaf1 = Leaf 'k' 5
testleaf2 = Leaf 'x' 8
testbranch1 = Branch (Leaf 'z' 13) (Leaf 'q' 2) 15
testbranch2 = Branch (Branch (Leaf 'z' 1) (Leaf 'e' 3) 4) (Branch (Leaf 'y' 2) (Leaf 'w' 4) 6) 10
-- merge Testing:
mergeinput1 = merge testleaf1 testleaf2
mergeinput2 = merge testleaf1 testbranch1
mergeinput3 = merge testbranch2 testbranch1
expectedmerge1 = Branch (Leaf 'k' 5) (Leaf 'x' 8) 13
expectedmerge2 = Branch (Leaf 'k' 5) (Branch (Leaf 'z' 13) (Leaf 'q' 2) 15) 20
expectedmerge3 = Branch t1 t2 25
    where
        t1 = (Branch (Branch (Leaf 'z' 1) (Leaf 'e' 3) 4) (Branch (Leaf 'y' 2) (Leaf 'w' 4) 6) 10)
        t2 = (Branch (Leaf 'z' 13) (Leaf 'q' 2) 15)

mergeTest = [mergeinput1 ==expectedmerge1, mergeinput2 == expectedmerge2, mergeinput3 == expectedmerge3]
-- leafify Testing:
leafifyTest = leafify (sortByFrequency (tabulate "abacbcbababaaaddddwwwrttt")) == leafifyexpected
leafifyexpected = [Leaf 'r' 1, Leaf 'c' 2, Leaf 'w' 3, Leaf 't' 3, Leaf 'd' 4, Leaf 'b' 5, Leaf 'a' 7]
-- makeTree Testing:
-- NB: Remember to sort list beforehand when testing.
-- treelist2 = [testleaf2, testbranch2, mergeinput1, testbranch1, mergeinput3]
-- makeTreeTest2 = makeTree treelist2 == merge mergeinput3 (merge (merge testleaf2 testbranch2) (merge mergeinput1 testbranch1))

treelist1 = [Leaf 'a' 5, Leaf 'b' 7, Leaf 'c' 10, Leaf 'd' 15, Leaf 'e' 20, Leaf 'f' 45]
makeTreeTest = Branch (Leaf 'f' 45) branchR 102 == makeTree treelist1
    where
        branchR = Branch (Branch (Leaf 'c' 10) (Branch (Leaf 'a' 5) (Leaf 'b' 7) 12) 22) (Branch (Leaf 'd' 15) (Leaf 'e' 20) 35) 57

generateTreeTest = generateTree (tabulate "abacbcbababaaaddddwwwrttt") == generateexpected
generateexpected = Branch (Branch (Leaf 'b' 5) (Branch (Leaf 'w' 3) (Leaf 't' 3) 6) 11) (Branch (Leaf 'a' 7) (Branch (Leaf 'd' 4) (Branch (Leaf 'r' 1) (Leaf 'c' 2) 3) 7) 14) 25

-- Question:
-- Generate a tree from list of Freqs (using makeTree above):
generateTree :: [Freq c] -> Tree c
generateTree xs = makeTree (leafify (sortByFrequency xs))

-- Encoding table.
-- A key is a key-value pair (an entry in a map/table).
type Key c = (c,[Bit])

-- The whole coding table
type CodingTable c = [Key c]

-- Question:
-- Given a tree, generates a coding table
makeTable :: Eq c => Tree c -> CodingTable c
makeTable tree = bitToDict [] tree

--CHR
bitToDict :: Eq c => [Bit] -> Tree c -> CodingTable c
bitToDict s (Leaf a _ ) = [(a,s)]
bitToDict s (Branch left right _) = bitToDict (s++[Z]) left ++ bitToDict (s++[I]) right

-- Testing makeTable
-- Output is ordered from left to right DFS
testtree1 = generateexpected
makeTableTest = makeTable testtree1 == tableexpected
tableexpected = [('b', readBits "00"),('w', readBits "010"),('t', readBits "011"),('a',readBits"10"),('d',readBits"110"),('r',readBits"1110"),('c',readBits"1111")]
makeTableTest2 = makeTable (makeTree (treelist1)) == tableexpected2
tableexpected2 = zip ['f','c','a','b','d','e'] [readBits x | x <- ["0","100","1010","1011","110","111"]]

-- Question:
-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable = undefined

-- Question:
-- Encodes directly from the tree (more efficient).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing = undefined

-- Question:
-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode = undefined

-- Encoding trees

-- Question:
-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress = undefined

-- Question:
-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' = undefined
