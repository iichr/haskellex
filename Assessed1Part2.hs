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


-- Convert the frequency pairs to leaves
leafify :: [Freq c] -> [Tree c]
leafify xs = map ((\(k,v) -> Leaf k v)) xs

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

bitToDict :: Eq c => [Bit] -> Tree c -> CodingTable c
bitToDict s (Leaf a _ ) = [(a,s)]
bitToDict s (Branch left right _) = bitToDict (s++[Z]) left ++ bitToDict (s++[I]) right

-- Question:
-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable ctbl (x:xs) = lookupTable x ctbl ++ encodeUsingTable ctbl xs 
encodeUsingTable ctbl [] = []

lookupTable :: Eq c =>  c -> [(c,v)] -> v
lookupTable a ((k,v):xs) = if a == k
    then v
    else lookupTable a xs

-- Question:
-- Encodes directly from the tree (more efficient).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing tree [] = []
encodeUsing tree (x:xs) = charToBitFromTree x tree ++ encodeUsing tree xs

charToBitFromTree :: Eq c => c -> Tree c -> [Bit]
charToBitFromTree x tree = reverse (tail (charToBitHelper x tree))

charToBitHelper :: Eq c => c -> Tree c -> [Bit]
charToBitHelper x (Leaf a _) = if a == x then [I] else [Z]
charToBitHelper x (Branch left right _) 
    | head (charToBitHelper x left) == I = charToBitHelper x left ++ [Z]
    | otherwise = (charToBitHelper x right) ++ [I]

-- Question:
-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode xs = (tree, bitseq)
    where
        tree = makeTree (leafify (sortByFrequency (tabulate xs)))
        bitseq = encodeUsing tree xs

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
compress xs = show (treeLength) ++ encodedTree ++ encodedBits
    where
        encodedTree = show (fst (encode xs))
        encodedBits = show (snd (encode xs))
        treeLength = length (encodedTree)

-- Question:
-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' xs
    | lengthOfCompressed > memSize xs = '*':xs
    | otherwise = compress xs
        where
            lengthOfCompressed = length (compress xs)


-- OWN TESTS
--
-- Run all tests available
-- Result ought to be True on all
allTests =  [mergeTest, [leafifyTest], [makeTreeTest], [generateTreeTest], makeTableTest, encodeUsingTbtest, encodeUsingTest]

-- Following code order
-- Initial setup: 
testleaf1 = Leaf 'k' 5
testleaf2 = Leaf 'x' 8
testbranch1 = Branch (Leaf 'z' 13) (Leaf 'q' 2) 15
testbranch2 = Branch (Branch (Leaf 'z' 1) (Leaf 'e' 3) 4) (Branch (Leaf 'y' 2) (Leaf 'w' 4) 6) 10

-- Test merge:
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

-- Test leafify:
leafifyTest = leafify (sortByFrequency (tabulate "abacbcbababaaaddddwwwrttt")) == leafifyexpected
leafifyexpected = [Leaf 'r' 1, Leaf 'c' 2, Leaf 'w' 3, Leaf 't' 3, Leaf 'd' 4, Leaf 'b' 5, Leaf 'a' 7]

-- Test makeTree:
-- NB: Sort list beforehand when testing.
treelist1 = [Leaf 'a' 5, Leaf 'b' 7, Leaf 'c' 10, Leaf 'd' 15, Leaf 'e' 20, Leaf 'f' 45]
makeTreeTest = Branch (Leaf 'f' 45) branchR 102 == makeTree treelist1
    where
        branchR = Branch (Branch (Leaf 'c' 10) (Branch (Leaf 'a' 5) (Leaf 'b' 7) 12) 22) (Branch (Leaf 'd' 15) (Leaf 'e' 20) 35) 57

generateTreeTest = generateTree (tabulate "abacbcbababaaaddddwwwrttt") == generateexpected
generateexpected = Branch (Branch (Leaf 'b' 5) (Branch (Leaf 'w' 3) (Leaf 't' 3) 6) 11) (Branch (Leaf 'a' 7) (Branch (Leaf 'd' 4) (Branch (Leaf 'r' 1) (Leaf 'c' 2) 3) 7) 14) 25

-- Test makeTable:
-- Output is ordered from left to right DFS
testtree1 = generateexpected
makeTableTest1 = makeTable testtree1 == tableexpected
tableexpected = [('b', readBits "00"),('w', readBits "010"),('t', readBits "011"),('a',readBits"10"),('d',readBits"110"),('r',readBits"1110"),('c',readBits"1111")]
makeTableTest2 = makeTable (makeTree (treelist1)) == tableexpected2
tableexpected2 = zip ['f','c','a','b','d','e'] [readBits x | x <- ["0","100","1010","1011","110","111"]]

makeTableTest = [makeTableTest1, makeTableTest2]

-- Test encodeUsingTable:
encodeUsingTbtest1 = encodeUsingTable tableexpected "batradwac" == encodeUsingTbexpected
encodeUsingTbexpected = readBits "0010011111010110010101111"
encodeUsingTbtest2 = encodeUsingTable tableexpected2 "bedcabdeafdabbec" == encodeUsingTbexpected2 
encodeUsingTbexpected2 = readBits "10111111101001010101111011110100110101010111011111100"

encodeUsingTbtest = [encodeUsingTbtest1, encodeUsingTbtest2]

-- Test charToBitFromTree:
ctbtest1 = charToBitFromTree 'a' (makeTree treelist1)
ctbtest2 = charToBitFromTree 'b' (makeTree treelist1)
ctbtest3 = charToBitFromTree 'c' (makeTree treelist1)
ctbtest4 = charToBitFromTree 'd' (makeTree treelist1)
ctbtest5 = charToBitFromTree 'e' (makeTree treelist1)
ctbtest6 = charToBitFromTree 'f' (makeTree treelist1)

ctbtests= [ctbtest1,ctbtest2,ctbtest3,ctbtest4,ctbtest5,ctbtest6]

encodeUsingTest1 = encodeUsing testtree1 "batradwac" == encodeUsingexpected
encodeUsingexpected = readBits "0010011111010110010101111"
encodeUsingTest2 = encodeUsing (makeTree (treelist1)) "bedcabdeafdabbec" == encodeUsingexpected2
encodeUsingexpected2 = readBits "10111111101001010101111011110100110101010111011111100"

encodeUsingTest = [encodeUsingTest1, encodeUsingTest2]

-- Provided tests from compress and decompress
-- Include this in your submission to test it:

compressTest :: Bool
compressTest = uncompressedString == decompress compressedString

compressTest' :: Bool
compressTest' = compress uncompressedString == compressedString

-- We will also test other examples for markink.

uncompressedString = "US officials are investigating multiple attacks that caused widespread online disruption on both sides of the Atlantic on Friday.\n\nThe Department of Homeland Security has begun an investigation into the DDoS (distributed denial-of-service) attack, the Guardian confirmed.\n\nThe incident took offline some of the most popular sites on the web, including Netflix, Twitter, Spotify, Reddit, CNN, PayPal, Pinterest and Fox News - as well as newspapers including the Guardian, the New York Times and the Wall Street Journal.\n\nThe attacks seemed to have been focused on Dyn, the company that runs the internet's domain name system (DNS).\n\nAmazon's web services division, one of the world's biggest cloud computing companies, also reported an outage that lasted several hours on Friday morning.\nSign up to the new-look Media Briefing: bigger, better, brighter\nRead more\n\nDoug Madory, director of internet analysis at Dyn, said he was not sure if the outages at Dyn and Amazon were connected.\n\n'We provide service to Amazon, but theirs is a complex network so it is hard to be definitive about causality,' he said.\n\nAmazon was not available for comment.\n\nDyn said it first became aware of the attack shortly after 7am ET on Friday. 'We began monitoring and mitigating a DDoS [distributed denial-of-service] attack against our Dyn Managed DNS infrastructure,' the company said on its website.\n\nThe company sent out updates throughout the day, confirming a second attack at about noon and a third just after 4pm.\n\nDDoS attacks are also becoming more common. Brian Krebs, an independentsecurity researcher, observed earlier this month that the 'source code' to the Mirai botnet had been released by a hacker group, 'virtually guaranteeing that the internet will soon be flooded with attacks from many new botnets powered by insecure routers, IP cameras, digital video recorders and other easily hackable devices'"

compressedString = "1473Branch (Branch (Branch (Leaf 'e' 179) (Branch (Branch (Branch (Branch (Leaf 'S' 10) (Branch (Leaf 'A' 5) (Branch (Leaf 'x' 3) (Leaf 'W' 3) 6) 11) 21) (Leaf '\\n' 22) 43) (Branch (Branch (Leaf '\\'' 11) (Leaf '.' 12) 23) (Leaf 'p' 23) 46) 89) (Leaf 'r' 91) 180) 359) (Branch (Branch (Branch (Leaf 'c' 49) (Leaf 'h' 49) 98) (Branch (Branch (Leaf ',' 25) (Leaf 'y' 26) 51) (Branch (Branch (Branch (Leaf '-' 6) (Leaf 'T' 7) 13) (Leaf 'k' 14) 27) (Leaf 'f' 27) 54) 105) 203) (Branch (Leaf 'n' 111) (Leaf 'o' 112) 223) 426) 785) (Branch (Branch (Branch (Leaf 'i' 113) (Branch (Branch (Leaf 'b' 29) (Branch (Branch (Leaf 'N' 7) (Branch (Leaf 'z' 4) (Leaf 'M' 4) 8) 15) (Leaf 'D' 15) 30) 59) (Branch (Leaf 'g' 31) (Branch (Branch (Branch (Branch (Branch (Leaf 'U' 1) (Leaf 'H' 1) 2) (Leaf '(' 2) 4) (Branch (Branch (Leaf 'J' 1) (Leaf ':' 1) 2) (Branch (Leaf 'C' 1) (Leaf 'Y' 1) 2) 4) 8) (Branch (Branch (Leaf 'R' 2) (Leaf 'B' 2) 4) (Branch (Leaf ')' 2) (Leaf 'G' 2) 4) 8) 16) (Branch (Branch (Leaf 'F' 4) (Leaf 'P' 4) 8) (Branch (Branch (Branch (Leaf '[' 1) (Leaf ']' 1) 2) (Branch (Leaf '7' 1) (Leaf 'E' 1) 2) 4) (Branch (Branch (Leaf 'K' 1) (Leaf 'I' 1) 2) (Branch (Leaf 'j' 1) (Leaf '4' 1) 2) 4) 8) 16) 32) 63) 122) 235) (Branch (Leaf 'a' 134) (Branch (Leaf 'd' 67) (Branch (Branch (Leaf 'v' 16) (Leaf 'w' 20) 36) (Leaf 'm' 39) 75) 142) 276) 511) (Branch (Leaf ' ' 287) (Branch (Leaf 't' 146) (Branch (Branch (Leaf 'l' 42) (Leaf 'u' 42) 84) (Leaf 's' 87) 171) 317) 604) 1115) 190010011100000001000011001110101110101111000010001000101011110011111110101000110001101000011010111000001111111101000100110101011101000011010011011010111111110111110011101000001011111100000110101011101110101001000010110111111110111001001101011101100100010101111011111100010110110101110110001011000011111001011001100010101011011001110110111100100001100001101011010001111100111111010010111110100001110110110011101101101001000111111001001110111111000101100001111111001110101111101110010010001100010001011101111001010011011101000010001100111011011010011110000111000101101010010101001010100100100100101011001010010001101001011000001011101000111110101111000011011101100111010111110100111000010111101111000111100101001101011011000100000000100011110100111000111001010111001001101011111110100100000100110111101011011010100110110100001101011100000111111110100010011010101110100001110110110100001101110011111011100100100011010010111001011011100100001101001110001101101000111111110001110001001001111011110000101101101011000001101000101011110001011000011101011101011000111110000011101110010000100000010011101101101010111011101010010000101101010100110111001001000110100111011111110110100011101101000101001101100100001110110010111100000111011110001011000101010010010010010101100101001000110100001100100010001011000001101110110111001110111010110111001110101110101111111001000011000011011111011110111100011001110101111101110010010001101011110111111111110110001011011100101111110111110010100011110111111000111000011111110011101101101110010010001101011101000100100010100110100001100100011110011110110110100001101001101101001010000011100101111111001000001000110010100110010110011011101100011101110000001101010011000100000010110111111010000101110101010101001101001110100000101101011010001110010100110100111001101001010010010100010100110100111101101001010110011110110101111000101001101001111011000011011100000011000111111110110101001101011011010011110001110010001101101001010000010111011111111001011000110101011111110101110100011110011110011010101111111001100001011101111110010111010001011000001111111110100001100100011110011110110110100001101001101101110010010001101001110111111101101000111011010001010011001010011011100100100011010010100000101110111010011100111011100110101101110010110011000101111000111111101010011010110110111001001000110001000111101011110011110011000100001110001100000011101101001110010001111111010011011010101111000010101001001001001010110010100100011010101110111010100100001011011111111011111000000101111000101101101110011111001001101010111000001101001000000000110110010111011101000111101111110001011011001110110110100101101010101100101001101110010010001100100001111011110010111010011001010111011100100110101110110001111110101101111111011100100100011010000110111000000110110000111000101001111111010110011110111110101000011011001101010101111000110111110101011111111100001011111101001110001100101110010100001000010011101100010101001001001001001000101011111010100101010011101100010100111111101011101000100100110111110000011101110010000100000011111110101101000101110010001111110000111011001010011001110110000110011101011111011100100100011010111010111001111110010110001010011111110100100100010011010011000011111111011001000111100011111110110110110010000111101111001011111101111010000110100110110010000111101111001011101001101000000111110101001101010111100111110111110001100000101101110011111000010110110101001101100111111101111010101001100001101110010011010111011011110010101111111100001011011011111000101110000000111010111100110010010111111101001111111110011101101101001111000011100010110101001010111010111101110011011010000110100110001010100100100100001000100110011011011110100101111011100111110111001001000110011000010111010101100011110001110111010110111010010101100010110100010101101001110101001110000000101111000011010011010011100101110100100100010011010011000000110101001101001000001110111000000110101001101001000011100010011001001111000000110010011001110100000101010110110101111011100110000010010010011001011011111110110011011010010101110101011001110011010101010100110101101000001100001000111001110011110011101011111010000110111000000110110000111011010100110101011110001010111111100011111110101011101101001011010101011001010011011111101010001011011001001000110101110110101111111001100111111011011111111101001100011010000101111101110010010001100111111101111010101001100001111111010101110110100101101010101101101010011010110110001000101011111010100101010011101101101011101000001100011001000011101100110000010001110000101100010101001001001001001010000100011100011000101100110111101110010001011000011011111000001110111001000010000001101110011111000100010101111101010010101001110110010100110100100111101111011011100100100010000011111111101000111111101010110010000111101111001011111100000001000110110011000011101011101011100110101101110111110111110100011101101000111111100100110100011101101101110011111010010000011010110000010111100001101000111010001011100000110101010010001111111011110110010001010111101111111010111100100011100101010101000010100110010010001101111110101000101100010101001001001001001000101011111010100101010011101101101011101101011111110011001111110110101010111001010100011110010101001001111000001100101110111001111001000011110111110111100001101110001010100100100100110010110101010110110111111010100010110110100011101100101111000001111111111011010010000001000101010111100011010101011101101000110001100111010111110111001001000110101011101110101001000010110111011111010010111001111101111000101011101010010111111000000111101001111101010101011111101001111101101011001110011101101101001111000011100010110101001010100101011100010100001000111000110100100000100110101001101101011110111011010001110011100111000011010011011010100110101101101011111000111010001001101010111010000110100110110101011010010111001011011100100001101001111100010110100011111111000111000100100111101111000010110110101100000110100010101111000101100001110101110101100011111000001110111001000010000001001111100111010101110111010100100001011011101010100110101010000110111111110110011111110100111101001011010101011011010010101110100110101010011000010110110100101110010100001000011010000110010111001110101111111100011111101010001110111101001100001010000101001101110010010001100100001111011110010111010011001010111011111101010001011011001110110110100011101111111010111010001001001111110001110000001010100100100100101011001010010001100100001111011110010111010011001010111011111000011011101100111111101111011011110100101110110101011100001111111011100100100110111111101100110010010111111101111011011100100100011010110101001010101010011001000011101100101111000001110111110000110100110110101011011111000010000111011010110110101011101110101001000010110111010101110110101010010001111111011110110011001110111011011010100110101101101010110111001001100000111011011010011111110111101111111110110101001011111100000011110100111111110010111011110010101001001001001100101110010110111001000011010101110111010100100001011011111111010100011000110101011110011111011111010010000001000011110111110000110100110110101111011100110001100100001111011111011110111011000101011101001110101001110001010011011010011111100001100010010011111010100110101001101101000011010110000001011000011010110000011011101111100001000111101001110001110010101110001100011111000101000110100001001000001101010011001111001001111100000111011100000101101100001010001111110010000000011110111001001100011111110101111011101101110010011101110010011010111011011100100100011000101001111101111111010011010000001100100001111011000000101001101110011111011100100100011010010101110000011101010001101001000111111001100001110110010011010101101101001000000000110110001100011110000010101111100010110110100100010101110101011001001101001000010110100000111101001100011011111110100101101010011000101001011100100000111110111101101011110011110001010111010011011110110100011101001101110000000100001101001101101110010011010111011011100100100011010000110111000000110110000111011010111011000111100111100110111110111011101101101001000001100101111111000111011110110000101101101011101100011100100111010101110111010100100001011011111111001011100110111101111110101111101001100101011100110000101110111010010001111110011000011101111111000101101111011101000001100010110110100100010101110100001101111100001000111101001100011000110111111101111000000111111101010011010011111101100111101110010001010101111000001110101111101010011010110100010011010001110101011110011010111001000101100000111110001100001000011100111011000000111111111010100110101101100111111001001000001111000010101111110001111000101011100100110100100001011011010100100111100000110101100001011100100001000000111110010100"


