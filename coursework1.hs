---- TASK 1 ----

--checks check digit and input length
--returns true if valid
isValidISBN13 :: [Int] -> Bool
isValidISBN13 a = (10-((sumISBN a) `mod` 10)) `mod` 10 == last a 
                  && length a == 13

--adds all numbers together, multiplying evens by 3
--subtracts the last number (x13 if correct length)
sumISBN x = let e = getEvens x
                o = getOdds x
                in (sum (map (3*) e) + sum o) - last x

--gets number and then recurses into getEvens
getOdds :: [Int] -> [Int]
getOdds (x:xs) = x:(getEvens xs)
getOdds [] = []

--gets number and then recurses into getOdds
getEvens :: [Int] -> [Int]
getEvens (x:xs) = getOdds xs
getEvens [] = []

---- END TASK 1 ----

----TASK 2----

--stores ordered numerals to be appended after their 
--value has been subtracted from the running total
numeralSet = ["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"]

--determines value of numeral
charValue :: Char -> Int
charValue 'M' = 1000
charValue 'D' = 500
charValue 'C' = 100
charValue 'L' = 50
charValue 'X' = 10
charValue 'V' = 5
charValue 'I' = 1

--returns string repeated n times
duplicate :: String -> Int -> String
duplicate x n = concat (replicate n x)

--assumes syntactically correct roman numerals
--converts by determining if value of a char is negative
--and then summing them together
romanToInt :: [Char] -> Int
romanToInt (x:xs) = 
  if xs == []
    then charValue x
    else if charValue x < charValue (head xs)
      then romanToInt xs - charValue x
      else charValue x + romanToInt xs
      
--converts to numeral by subtracting from a running total based on
--max value represented with one element from numeral set
subtractAppend :: Int -> [String] -> [Char]
subtractAppend 0 numSet= ""
subtractAppend x numSet= 
  let repeats = x `div` romanToInt (head numSet) --gets times the numeral should repeat
    in duplicate (head numSet) (repeats) --gets numeral string
    ++ subtractAppend (x - repeats * romanToInt (head numSet)) (tail numSet) --subtracts value of above, recurses

--Converts int to roman using subtract append 
intToRoman :: Int -> [Char]
intToRoman x = subtractAppend x numeralSet

---- END TASK 2 ----

---- TASK 3 ----

--defines a tree, either empty node or node with value and 2 subtrees
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

--returns an end node (node with no children)
endNode :: a -> Tree a
endNode x = Node x Empty Empty

--builds a tree by using foldr to recursively call insert on each new tree
buildTree :: [Int] -> Tree Int
buildTree x = foldr insertIntoTree Empty (reverse x)

--left prioritised depth first traversal
flattenTree :: Tree Int -> [Int]
flattenTree Empty = [] --base case, if empty no value stored so return no value
flattenTree (Node x left right) = flattenTree left ++ [x] ++ flattenTree right --get values of subtrees recursively and append

--returns modified tree with item added to it
insertIntoTree :: Ord a => a -> Tree a -> Tree a
insertIntoTree x Empty = endNode x --base case, if current node is empty, place x there
insertIntoTree x (Node currentVal left right) = --non-empty node
  if x < currentVal
    then Node currentVal (insertIntoTree x left) right --if lower than item in current node, insert on left child
    else Node currentVal left (insertIntoTree x right) --if higher than item in current node, insert on right child

--sorts list of nums with tree functions, depth first tree
sortWithTree :: [Int] -> [Int]
sortWithTree x = flattenTree (buildTree x)

---- END TASK 3 ----

---- TASK 4 ----

--stores the grid as list of strings
wordGrid = ["IUPGRADEEPEQ",
            "YTDZMTZVNRXS",
            "YVCECTIWALZR",
            "PCPGERSWGCRE",
            "PGLUDVDUCFNS",
            "ONTDJRRWDFOY",
            "LVRGAXAIYFKZ",
            "FAUHBGSXTELI",
            "HEGSPKHWYPOC",
            "TESZEBABIDKY",
            "NZWTUROHOIPK",
            "MXTGEADGAVLU",
            "TESSRMEMORYO",
            "DIQDTROMTKSL",
            "IRCTLAPTOPOX"]

--stores the words for convenience of testing
wordList =["LAPTOP", "KEYBOARD", "BUGS", "DISKETTE", "UPGRADE",
           "MEMORY", "HARDWARE", "FLOPPY", "HARDDRIVE", "SOFTWARE"]

----HELPER FUNCS----

--checks if true occurs in a list of bools at least once
trueOccurs :: [Bool] -> Bool
trueOccurs = foldr (||) False

--checks if a word occurs as a substring in a list of strings
wordOccurs :: String -> [String] -> Bool
wordOccurs word x = trueOccurs (map (isIn word) x)

--reverses all strings in a list of strings
revls :: [String] -> [String]
revls x = map reverse x

--checks if a substring appears in a list
isIn :: String -> String -> Bool
isIn toFind x = 
  if length toFind <= length x
    then toFind == take (length toFind) x || isIn toFind (tail x)
    else False

----END HELPER FUNCS----

--takes grid x and list of words w, finds them and returns method of finding         
wordSearch:: [String] -> [String] -> [String]
wordSearch x (w:ws) = 
  if wordOccurs w x then (w++" right"):wordSearch x ws
  else if wordOccurs w (revls x) then (w++" left"):wordSearch x ws
  else if wordOccurs w (getDowns x) then (w++" down"):wordSearch x ws
  else if wordOccurs w (revls(getDowns x)) then (w++" up"):wordSearch x ws
  else if wordOccurs w (getAllUR x) then (w++" upright"):wordSearch x ws
  else if wordOccurs w (revls(getAllUR x)) then (w++" downleft"):wordSearch x ws
  else if wordOccurs w (getAllDR x) then (w++" downright"):wordSearch x ws
  else if wordOccurs w (revls(getAllDR x)) then (w++" upleft"):wordSearch x ws
    else (w++" notfound"):wordSearch x ws --if not found say so and carry on
wordSearch x [] = []


-- gets downs, reverse is ups
-- takes a grid and returns the transpose
getDowns :: [String] -> [String]
getDowns ([]:xs) = []
getDowns xs = map head xs : getDowns (map tail xs)

--DownRight diagonals, reverse is upleft
--get coordinates of a char and then adds chars to all below and to the right
getDiagonalDR :: [String] -> (Int,Int) -> [Char]
getDiagonalDR x (col,row) = 
  if row <= length (x)-1 && col <= length (x !!0 )-1
    then ([(x !! row) !! col])  ++ getDiagonalDR x (col+1,row+1)
    else ""


--UpRight diagonals, reverse is downleft
--get coordinates of a char and then adds chars to all above and to the right
getDiagonalUR :: [String] -> (Int,Int) -> [Char]
getDiagonalUR x (col,row) = 
  if row >= 0 && col <= length (x !!0 )-1
    then ([(x !! row) !! col])  ++ getDiagonalUR x (col+1,row-1)
    else ""
    
--applies getDiagonalUR to bottom side and left side
getAllUR :: [String] -> [String]
getAllUR x = let width = (length (x !! 0)) -1
                 height = (length x) -1
             in map (getDiagonalUR x) (zip [0,0..] [0..height]) --zip is coords of all leftmost elements
             ++ map (getDiagonalUR x) (zip [1..width] [height,height..]) --zip is coords of all bottom elements

--applies getDiagonalDR to top side and left side          
getAllDR :: [String] -> [String]
getAllDR x = let width = (length (x !! 0)) -1
                 height = (length x) -1
             in map (getDiagonalDR x) (zip [0,0..] [0..height]) --zip is coords of all leftmost elements
             ++ map (getDiagonalDR x) (zip [1..width] [0,0..]) --zip is coords of all top elements

---- END TASK 4 ----