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

-- first get all words with that letter : [String]
-- initialWords = [w | w <- allWords1 dict c, (length w <= (x+y+1))]

-- for every element in the list get where the CHAR is at
-- elementPositions = map (elemIndices c) initialWords

-- zip the resulting lists together:
-- wordsWithIntersectionIndices = zip initialWords elementPositions

{-
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
-}

-- implementation of the above
tupConverter :: ([Char], [Int]) -> Int -> Int -> [([Char], Int)]
tupConverter ent x y
    | (length $ snd $ ent ) == 1 && ((length (fst $ splitIntersect) - 1) <= x) && (length (snd splitIntersect) <=y) = zip [(fst $ ent)] [(head $ snd $ ent)]
    | otherwise = []
    where  splitIntersect = splitAt ((head $ snd $ ent) + 1) (fst $ ent)

--        if (length (fst $ splitIntersect) - 1) <= x &&
--            length (snd splitIntersect) <=y
--        then zip (fst $ ent) [(head $ snd $ ent) + 1]
--        else []

-- implementation with pattern matching, tuple is (t, us)
-- seems to work the same way
tupConverter' :: (String, [Int]) -> Int -> Int -> [(String, Int)]
tupConverter' (t,us) x y
    | length us == 1 && ((length (fst $ splitIntersect) - 1) <= x) && (length (snd splitIntersect) <=y) = zip [t] us
    | length us > 1 = tupConverter' (t, [head us]) x y ++ (tupConverter' (t,tail us) x y)
    | otherwise = [] 
    where splitIntersect = splitAt (head us + 1) t

-- get all words with given letter (passed as argument to allWords2 as well)
allWords1 :: Dict -> Char -> [String]
allWords1 dict x = [w | w <- dict, x `elem` w]

wordsWith c dict =  allWords1 dict c;

-- for every element in the list get positions of the CHAR (ditto as above)
elemPos c dict = map (elemIndices c) (wordsWith c dict)

-- get the resulting tuple lsit of element,possible index of intersection char
wordCharPosList :: Char -> Dict -> [(String,[Int])]
wordCharPosList c dict = zip (wordsWith c dict) (elemPos c dict)

-- finally all into one fucntion

allWords2' :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2' dict c x y = tupConverterList v x y
    where v = wordCharPosList c dict

-- working attempt, relying on the first implementation tupConverter'
allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 dict c x y = ws
    where
        ws = concat  [z | z<-[tupConverter' w x y| w <- [ k | k <- wordCharPosList c dict ]], z/=[]]

--Attempt to make tupConverter' works straight away with wordCharPosList as input 
tupConverterList :: [(String, [Int])] -> Int -> Int -> [(String, Int)]
tupConverterList ((t,us) : v) x y  
    | v==[] && length us == 1 && ((length (fst $ splitIntersect) - 1) <= x) && (length (snd splitIntersect) <=y) = zip [t] us
    | v==[] && length us > 1 = tupConverterList [(t, [head us])] x y ++ (tupConverterList [(t,tail us)] x y) 
    | length us == 1 && ((length (fst $ splitIntersect) - 1) <= x) && (length (snd splitIntersect) <=y) = (zip [t] us) ++ (tupConverterList v x y)
    | length us > 1 = tupConverterList [(t, [head us])] x y ++ (tupConverterList [(t,tail us)] x y) ++ (tupConverterList v x y)
    | otherwise = []
    where splitIntersect = splitAt (head us + 1) t

{-
tupConverterList' :: [(String, [Int])] -> Int -> Int -> [(String, Int)]
tupConverterList' ((t,us) : []) x y 
    |length us == 1 && ((length (fst $ splitIntersect) - 1) <= x) && (length (snd splitIntersect) <=y) = zip [t] us
    |length us > 1 = tupConverterList' [(t, [head us])] x y ++ (tupConverterList' [(t,tail us)] x y) 
    where splitIntersect = splitAt (head us + 1) t

tupConverterList' ((t,us) : v) x y
    |length us == 1 && ((length (fst $ splitIntersect) - 1) <= x) && (length (snd splitIntersect) <=y) = zip [t] us ++ tupConverterList' v x y
    |length us > 1 = tupConverterList' [(t, [head us])] x y ++ (tupConverterList' [(t,tail us)] x y) ++ tupConverterList' v x y
    where splitIntersect = splitAt (head us + 1) t
-}



sampleDict :: Dict
sampleDict = ["abacus", "aardvark", "lion", "mesmerise", "egg", "elsewhere", "somewhere", "tetkaest", "discrepancy"]

sowpods500 :: Dict
sowpods500 = ["aa","aah","aahed","aahing","aahs","aal","aalii","aaliis","aals","aardvark","aardvarks","aardwolf","aardwolves","aargh","aarrgh","aarrghh","aarti","aartis","aas","aasvogel","aasvogels","ab","aba","abac","abaca","abacas","abaci","aback","abacs","abacterial","abactinal","abactinally","abactor","abactors","abacus","abacuses","abaft","abaka","abakas","abalone","abalones","abamp","abampere","abamperes","abamps","aband","abanded","abanding","abandon","abandoned","abandonedly","abandonee","abandonees","abandoner","abandoners","abandoning","abandonment","abandonments","abandons","abandonware","abandonwares","abands","abapical","abas","abase","abased","abasedly","abasement","abasements","abaser","abasers","abases","abash","abashed","abashedly","abashes","abashing","abashless","abashment","abashments","abasia","abasias","abasing","abask","abatable","abate","abated","abatement","abatements","abater","abaters","abates","abating","abatis","abatises","abator","abators","abattis","abattises","abattoir","abattoirs","abattu","abature","abatures","abaxial","abaxile","abaya","abayas","abb","abba","abbacies","abbacy","abbas","abbatial","abbe","abbed","abbes","abbess","abbesses","abbey","abbeys","abbot","abbotcies","abbotcy","abbots","abbotship","abbotships","abbreviate","abbreviated","abbreviates","abbreviating","abbreviation","abbreviations","abbreviator","abbreviators","abbreviatory","abbreviature","abbreviatures","abbs","abcee","abcees","abcoulomb","abcoulombs","abdabs","abdicable","abdicant","abdicate","abdicated","abdicates","abdicating","abdication","abdications","abdicative","abdicator","abdicators","abdomen","abdomens","abdomina","abdominal","abdominally","abdominals","abdominoplasty","abdominous","abduce","abduced","abducens","abducent","abducentes","abduces","abducing","abduct","abducted","abductee","abductees","abducting","abduction","abductions","abductor","abductores","abductors","abducts","abeam","abear","abearing","abears","abecedarian","abecedarians","abed","abegging","abeigh","abele","abeles","abelia","abelian","abelias","abelmosk","abelmosks","aberdevine","aberdevines","abernethies","abernethy","aberrance","aberrances","aberrancies","aberrancy","aberrant","aberrantly","aberrants","aberrate","aberrated","aberrates","aberrating","aberration","aberrational","aberrations","abessive","abessives","abet","abetment","abetments","abets","abettal","abettals","abetted","abetter","abetters","abetting","abettor","abettors","abeyance","abeyances","abeyancies","abeyancy","abeyant","abfarad","abfarads","abhenries","abhenry","abhenrys","abhominable","abhor","abhorred","abhorrence","abhorrences","abhorrencies","abhorrency","abhorrent","abhorrently","abhorrer","abhorrers","abhorring","abhorrings","abhors","abid","abidance","abidances","abidden","abide","abided","abider","abiders","abides","abiding","abidingly","abidings","abies","abietic","abigail","abigails","abilities","ability","abiogeneses","abiogenesis","abiogenetic","abiogenetically","abiogenic","abiogenically","abiogenist","abiogenists","abiological","abioses","abiosis","abiotic","abiotically","abiotrophic","abiotrophies","abiotrophy","abirritant","abirritants","abirritate","abirritated","abirritates","abirritating","abiturient","abiturients","abject","abjected","abjecting","abjection","abjections","abjectly","abjectness","abjectnesses","abjects","abjoint","abjointed","abjointing","abjoints","abjunction","abjunctions","abjuration","abjurations","abjure","abjured","abjurer","abjurers","abjures","abjuring","ablactation","ablactations","ablate","ablated","ablates","ablating","ablation","ablations","ablatitious","ablatival","ablative","ablatively","ablatives","ablator","ablators","ablaut","ablauts","ablaze","able","abled","ablegate","ablegates","ableism","ableisms","ableist","ableists","abler","ables","ablest","ablet","ablets","abling","ablings","ablins","abloom","ablow","abluent","abluents","ablush","abluted","ablution","ablutionary","ablutions","ablutomane","ablutomanes","ably","abmho","abmhos","abnegate","abnegated","abnegates","abnegating","abnegation","abnegations","abnegator","abnegators","abnormal","abnormalism","abnormalisms","abnormalities","abnormality","abnormally","abnormals","abnormities","abnormity","abnormous","abo","aboard","abode","aboded","abodement","abodements","abodes","aboding","abohm","abohms","aboideau","aboideaus","aboideaux","aboil","aboiteau","aboiteaus","aboiteaux","abolish","abolishable","abolished","abolisher","abolishers","abolishes","abolishing","abolishment","abolishments","abolition","abolitional","abolitionary","abolitionism","abolitionisms","abolitionist","abolitionists","abolitions","abolla","abollae","abollas","aboma","abomas","abomasa","abomasal","abomasi","abomasum","abomasus","abomasuses","abominable","abominableness","abominably","abominate","abominated","abominates","abominating","abomination","abominations","abominator","abominators","abondance","abondances","abonnement","abonnements","aboon","aboral","aborally","abord","aborded","abording","abords","abore","aborigen","aborigens","aborigin","aboriginal","aboriginalism","aboriginalisms","aboriginalities","aboriginality","aboriginally","aboriginals","aborigine","aborigines","aborigins","aborne","aborning","abort","aborted","abortee","abortees","aborter","aborters","aborticide","aborticides","abortifacient","abortifacients","aborting","abortion","abortional","abortionist","abortionists","abortions","abortive","abortively","abortiveness","abortivenesses","aborts","abortuaries","abortuary","abortus","abortuses","abos","abought","aboulia","aboulias","aboulic","abound","abounded","abounding"]
