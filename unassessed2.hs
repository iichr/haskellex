-- The exercises not present here don't lend themselves to 
-- being written in Haskell code, but rather on paper
-- Week 2 October 3-9

-- CHAPTER 2
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

--last function definition
lastmodified :: [a] -> a
lastmodified [x] = x
lastmodified (_:xs) = lastmodified xs

--init function definiton
initmodified :: [a] -> [a]
initmodified [x] = []
initmodified (x:xs) = x : initmodified xs

--init function defined in a second way
initmodified2 :: [a] -> [a]
initmodified2 xs = take (length xs -1)  xs

--init function defined in a thrid way
initmodified3 :: [a] -> [a]
initmodified3 xs = reverse (drop 1 (reverse xs))

--CHAPTER 3

bools3 :: [Bool]
bools3 = [True, False, True, False]

nums3 :: [[Int]]
nums3 = [[1,2,3],[4,5,6]]

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

copy3 :: a -> (a,a)
copy3 a = (a,a)

apply3 :: (a -> b) -> a -> b
apply3 f x = f x


--CHAPTER 4

--splitting a list with an even number of elements into two halves
halve :: [a] -> ([a],[a])
splitlist_helper :: [a] -> Int

splitlist_helper xs = (length xs) `div` 2

halve xs = (x,y) 
    where
        x = take (splitlist_helper xs) xs
        y = drop (splitlist_helper xs) xs

--function that returns the third element of a list defined in 3 ways

third1 :: [a] -> a
third2 :: [a] -> a
third3 :: [a] -> a

third1 xs = head (tail (tail xs))
third2 xs = xs !! 2
third3 (_:_:x:_) = x

--FOLLOWING 3 EXAMPLES:
    --safe tail that returns an empty list if no list exists
    --safetail :: [a] -> a

--using conditional statements
safetail xs =
    if null xs then [] else tail xs

--using guarded equations
safetailguard xs 
    | null xs = []
    | otherwise = tail xs

--using pattern matching
safetailPM [] = []
safetailPM [x] = []
safetailPM (_:x) = x

--OR definition with pattern matching
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

--Curried function to lambda expression
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z  -> x*y*z

-- Luhn Algorithm for 4-digit numbers
luhnDouble x
    | x*2 < 9 = x*2
    | otherwise = x*2 - 9

luhnDouble2 x = 
    if x*2 < 9
        then x*2
        else x*2 - 9

luhn w x y z = (luhnDouble y + luhnDouble w + x + z) `mod` 10 == 0  

--CHAPTER 5

--Ex.2 Grid generator 
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--Ex.3 Sqaure grid generator
square n = [(x,y) | (x,y) <- grid n n, x/=y]

--Ex.4 Replicator
replicate5 n x = [x | _ <- [1..n]]

--Ex.5 Pythagorean triplet up to a number n
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--Ex.6 Perfects function

removeHelper x ys = [y | y <- ys, x/=y]

factors n = [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], (sum (removeHelper x (factors x))) == x ]
