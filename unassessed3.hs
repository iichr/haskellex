data MorseUnit = Beep | Silence
  deriving (Eq, Show)

dit, dah, shortGap, mediumGap :: [MorseUnit]
dit       = [Beep, Silence]
dah       = [Beep, Beep, Beep, Silence]
shortGap  = replicate (3-1) Silence
mediumGap = replicate (7-3) Silence

codeSymbol :: Char -> [MorseUnit]
codeSymbol 'A' = dit ++ dah
codeSymbol 'B' = dah ++ dit ++ dit ++ dit
codeSymbol 'C' = dah ++ dit ++ dah ++ dit
codeSymbol 'D' = dah ++ dit ++ dit
codeSymbol 'E' = dit
codeSymbol 'F' = dit ++ dit ++ dah ++ dit
codeSymbol 'G' = dah ++ dah ++ dit
codeSymbol 'H' = dit ++ dit ++ dit ++ dit
codeSymbol 'I' = dit ++ dit
codeSymbol 'J' = dit ++ dah ++ dah ++ dah
codeSymbol 'K' = dah ++ dit ++ dah
codeSymbol 'L' = dit ++ dah ++ dit ++ dit
codeSymbol 'M' = dah ++ dah
codeSymbol 'N' = dah ++ dit
codeSymbol 'O' = dah ++ dah ++ dah
codeSymbol 'P' = dit ++ dah ++ dah ++ dit
codeSymbol 'Q' = dah ++ dah ++ dit ++ dah
codeSymbol 'R' = dit ++ dah ++ dit
codeSymbol 'S' = dit ++ dit ++ dit
codeSymbol 'T' = dah
codeSymbol 'U' = dit ++ dit ++ dah
codeSymbol 'V' = dit ++ dit ++ dit ++ dah
codeSymbol 'W' = dit ++ dah ++ dah
codeSymbol 'X' = dah ++ dit ++ dit ++ dah
codeSymbol 'Y' = dah ++ dit ++ dah ++ dah
codeSymbol 'Z' = dah ++ dah ++ dit ++ dit
codeSymbol '1' = dit ++ dah ++ dah ++ dah ++ dah
codeSymbol '2' = dit ++ dit ++ dah ++ dah ++ dah
codeSymbol '3' = dit ++ dit ++ dit ++ dah ++ dah
codeSymbol '4' = dit ++ dit ++ dit ++ dit ++ dah
codeSymbol '5' = dit ++ dit ++ dit ++ dit ++ dit
codeSymbol '6' = dah ++ dit ++ dit ++ dit ++ dit
codeSymbol '7' = dah ++ dah ++ dit ++ dit ++ dit
codeSymbol '8' = dah ++ dah ++ dah ++ dit ++ dit
codeSymbol '9' = dah ++ dah ++ dah ++ dah ++ dit
codeSymbol '0' = dah ++ dah ++ dah ++ dah ++ dah


-- Q 1.1
codeWord :: String -> [MorseUnit]
codeWord xs = concat [codeSymbol x ++ shortGap | x <- xs]

{-
codeWord2 :: String -> [MorseUnit]
codeWord2 xs = [y | z <- ys, y <- z] 
    where
        ys = [codeSymbol x ++ shortGap | x <- xs]
-}


-- FIX ending with mediumGap instead of shortGap

codeText :: [String] -> [MorseUnit]
codeText yss = concat [codeWord y ++ mediumGap | ys <- yss, y <- words ys]

--TEST with "TE ET"
outputTest = [Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence]
inputTest = codeText ["TE ET"]

--Q 2
decodeWord::[MorseUnit] -> [MorseUnit] -> String
decodeWord (Silence:Silence:Silence:xs) sth = (lookup (word ++ [Silence]) table) : decodeWord xs []
decodeWord (x:xs) sth = decodeWord [] _ = []
--decodeWord xs (word ++ [x])

decodeHelper [] (x:xs) = decodeWord (x:xs) []
decodeHelper [] _ = []
decodeHelper (Silence:Silence:Silence:Silence:xs) word = 
    (decodeWord (word ++ [Silence]) []) ++ " " ++ decodeHelper xs []
decodeHelper (x:xs) word = decodeHelper xs (word ++ [x])

decode xs = decodeHelper xs []


--Q 3

type MorseTable = [([MorseUnit], Char)]

table :: MorseTable
table = [(dit ++ dah, 'A'),
         (dah ++ dit ++ dit ++ dit, 'B'),
         (dah ++ dit ++ dah ++ dit, 'C'),
         (dah ++ dit ++ dit, 'D'),
         (dit, 'E'),
         (dit ++ dit ++ dah ++ dit, 'F'),
         (dah ++ dah ++ dit, 'G'),
         (dit ++ dit ++ dit ++ dit, 'H'),
         (dit ++ dit, 'I'),
         (dit ++ dah ++ dah ++ dah, 'J'),
         (dah ++ dit ++ dah, 'K'),
         (dit ++ dah ++ dit ++ dit, 'L'),
         (dah ++ dah, 'M'),
         (dah ++ dit, 'N'),
         (dah ++ dah ++ dah, 'O'),
         (dit ++ dah ++ dah ++ dit, 'P'),
         (dah ++ dah ++ dit ++ dah, 'Q'),
         (dit ++ dah ++ dit, 'R'),
         (dit ++ dit ++ dit, 'S'),
         (dah, 'T'),
         (dit ++ dit ++ dah, 'U'),
         (dit ++ dit ++ dit ++ dah, 'V'),
         (dit ++ dah ++ dah, 'W'),
         (dah ++ dit ++ dit ++ dah, 'X'),
         (dah ++ dit ++ dah ++ dah, 'Y'),
         (dah ++ dah ++ dit ++ dit, 'Z'),
         (dit ++ dah ++ dah ++ dah ++ dah, '1'),
         (dit ++ dit ++ dah ++ dah ++ dah, '2'),
         (dit ++ dit ++ dit ++ dah ++ dah, '3'),
         (dit ++ dit ++ dit ++ dit ++ dah, '4'),
         (dit ++ dit ++ dit ++ dit ++ dit, '5'),
         (dah ++ dit ++ dit ++ dit ++ dit, '6'),
         (dah ++ dah ++ dit ++ dit ++ dit, '7'),
         (dah ++ dah ++ dah ++ dit ++ dit, '8'),
         (dah ++ dah ++ dah ++ dah ++ dit, '9'),
         (dah ++ dah ++ dah ++ dah ++ dah, '0')]


data MorseTree = Nil 
               | Leaf Char 
               | Branch1 Char MorseTree MorseTree 
               | Branch0 MorseTree MorseTree 
  deriving (Eq, Show)



toTree :: MorseTable -> MorseTree
toTree :: [([MorseUnit], Char)] -> MorseTree


{-


if table [] -> Nil  
if table [(_,x)] -> Leaf x
table xss = Branch0 tree1 tree2
    where
        tree1 = Branch1 Char 
        yss = [ys |(dit:ys, _) <- xss]

        tree2 = Branch1
        zss = [zs |(dah:zs, _) <- xss]


tree xss | "" `elem` xss = EndOr t
         | otherwise     = t
  where
    yss = [ys | ('0':ys) <- xss]
    zss = [zs | ('1':zs) <- xss] 
    t = Branch (tree yss) (tree zss)


-}

