import Data.List
import Data.Function
{-
Huffman encoding. Week 4 Assessed exercise in Functional Programming.
Part 1 consist of a couple of general exericises.
-}

--Exercise 1: Given a string, create a frequency table of the occurrences
--of each letter in the string (count the number of instances basically)

count x xs = length [x' | x' <- xs, x==x']

freq xs = removeduplicates [(x,count x ys)| x <- ys]
    where
        ys = sort xs

removeduplicates :: Eq a => [a] -> [a]
removeduplicates [] = []
removeduplicates (x:xs)
    |x `elem` xs = removeduplicates xs
    | otherwise = x : removeduplicates xs

-- Exercise 2: sort by frequency instead, not by lexicographical order
{-
sortByFreq :: [(Char, Int)] -> [(Char, Int)]
sortByFreq ((a,n):xs) = ys ++ [(a,n)] ++ zs
    where
        ys = [(b,c)| (b,c) <- xs, c >= n]
        zs = [(d,e)| (d,e) <- xs, e < n]
-}

sortDef (a,b) (c,d)
    | b<d = GT
    | b>d = LT
    | b==d = compare a c

sortByFreq :: [Char] -> [(Char, Int)]
--sortByFreq xs = sortBy (sortDef) (freq xs)
sortByFreq xs = sortBy (compare `on` snd) (freq xs)

--Exercise 3:
-- All leaves contain a char, and all nodes 
-- contain the number of frequencies from the sorted String.

data HuffTree = Leaf (Char,Int)
    | Node Int HuffTree HuffTree
    | Empty
    deriving Show

mergeTwo :: HuffTree -> HuffTree -> HuffTree
mergeTwo (Leaf (a,p)) Empty = (Leaf (a,p))
mergeTwo Empty (Leaf (a,p)) = (Leaf (a,p))

mergeTwo (Node x t1 t2) Empty = (Node x t1 t2)
mergeTwo Empty (Node x t1 t2) = (Node x t1 t2)

mergeTwo Empty Empty = Empty

mergeTwo (Leaf (a,p)) (Node x t1 t2) = (Node (p+x) (Leaf (a,p)) (Node x t1 t2))
mergeTwo (Node x t1 t2) (Leaf (a,p)) = (Node (p+x) (Leaf (a,p)) (Node x t1 t2))

mergeTwo (Leaf (a,p)) (Leaf(b,q)) = (Node (p+q) (Leaf (a,p)) (Leaf (b,q)))
mergeTwo (Node x t1 t2) (Node y t3 t4) 
    | x<y = (Node (x+y) (Node x t1 t2) (Node y t3 t4))
    | otherwise = (Node (x+y) (Node y t3 t4) (Node x t1 t2))

insertT :: HuffTree -> HuffTree -> HuffTree
insertT Empty (Node x t1 t2) = (Node x t1 t2)
insertT (Node x t1 t2) Empty = (Node x t1 t2)
insertT Empty (Leaf (a,p)) = (Leaf (a,p))
insertT (Leaf (a,p)) Empty = (Leaf (a,p))
insertT Empty Empty = Empty
insertT (Leaf (a,p)) (Leaf (b,q)) = (Node (p+q) (Leaf (a,p)) (Leaf (b,q)))
insertT (Leaf (a,p)) (Node x t1 t2) = (Node (x+p) (Leaf (a,p)) (Node x t1 t2))
insertT (Node x t1 t2) (Leaf (a,p)) = (Node (x+p) (Leaf (a,p)) (Node x t1 t2))
insertT (Node x t1 t2) (Node y t3 t4) 
    | x<y = (Node (x+y) (Node x t1 t2) (Node y t3 t4))
    | otherwise = (Node (x+y) (Node y t3 t4) (Node x t1 t2))

--merger :: [(Char, Int)] -> HuffTree
--merger [(a,p)] = Leaf (a,p)
--merger (t1:t2:leaves) =  insertT (mergeTwo t1 t2) (merger leaves)

merger :: [HuffTree] -> HuffTree
merger [] = Empty
merger [Empty] = Empty
merger [Leaf (a,p)] = Leaf (a,p)
merger (t1:t2:leaves) =  insertT (mergeTwo t1 t2) (merger leaves)

leafify :: [(Char, Int)] -> [HuffTree]
leafify xs = [Leaf (a,p) | (a,p) <- xs]
--map (\ ((k,v)) -> Leaf (k,v)) xs

makeTree :: [Char] -> HuffTree
makeTree xs = merger (leafify (reverse(sortByFreq xs)))

--Char to bit, stored in a table for easier referencing
cTb :: HuffTree -> [(Char, String)]
cTb = helper_cTb ""

helper_cTb :: String -> HuffTree -> [(Char, String)]
helper_cTb s (Leaf (a,_)) = [(a,s)]
helper_cTb s (Node _ left right) = helper_cTb (s ++ "0") left ++ helper_cTb (s ++ "1") right

--Encode
encode :: String -> String
encode xs = helper_encode xs charToBit
    where
        charToBit = cTb (makeTree xs)

helper_encode :: String -> [(Char, String)] -> String
helper_encode (x:xs) charToBit = lookupMy x charToBit ++ helper_encode xs charToBit
helper_encode [] charToBit = []
--[lookup s charToBit | s <- xs]

lookupMy :: Eq a => a -> [(a,b)] -> b
lookupMy a ((k,v):xs) = if a == k
    then v
    else lookupMy a xs  

decode :: HuffTree -> [Char] -> String
decode tree bits = helper_decode tree bits
    where
        helper_decode (Node _ left right) (x:xs)
            | x == '0' = helper_decode left xs
            | x == '1' = helper_decode right xs
            | otherwise = []
        helper_decode (Leaf (k,v)) xs = k : helper_decode tree xs
        helper_decode _ [] = []
{-
helper_decode :: HuffTree -> [Char] -> String
helper_decode (Node _ left right) (x:xs)
    | x == '0' = helper_decode left xs
    | x == '1' = helper_decode right xs
    | otherwise = []
helper_decode (Leaf (k,v)) xs = k : helper_decode tree xs
helper_decode _ [] = []
-}
{-
charToBit :: HuffTree -> [String]
strings Empty = []
strings (Leaf (k,v)) = ['']
strings (Node x t1 t2) = ['0':xs | xs <- strings t1] ++ ['1':xs | xs <- strings t2]

stringToBit :: [Char] -> [Char]
stringToBit xs = [charToBit x| x <- xs']
    where
        xs' = makeTree xs
-}

{- data HuffTree = Empty
    | Leaf Char
    | Branch HuffTree HuffTree

strings::HuffTree -> [String]
strings Empty = []
strings Leaf x = [x]
strings Branch l r = ['0':xs | xs <- strings l] ++
['1':xs | xs <- strings r]


hftree::[Char] -> HuffTree
hftree [] = Empty
hftree xss = 
    Branch (tree yss) (tree zss)
    where
        yss = [ys | ('0':ys) <- xss]
        zss = [zs | ('1':zs) <- xss]
-}


{-
From frequency table we get an ordered list by frequencies
for every element of that list -> create a tree with Node =  freq1 + freq2
and Leaf Char1 and Char2 (so basically a Branch with 2 leaf nodes)

then Merge every two into a tree
repeat until no more left!

At this point should have a tree with an empty start node, branching down 
with numbers in the Nodes and Leaf chars
then we traverse tree with 1=L, 0=R to get the code for a specific char
mapping it in a tuple of the form [(hufman code, 'char'), ... ]

data HuffTree = Empty
    | Leaf Char
    | Branch HuffTree HuffTree
-}
