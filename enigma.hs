module EnigmaMachine where

--for indexing
import Data.List
--for ord, isUpper, and isLower
import Data.Char

--DATA DECLARATIONS-----------------------------------

type Rotor = ([Int], Int)
-- Rotor = (key, count)
--the key is a list of integers that determines how to translate one integer into another.  For example, a key of [3,4,5] means that 0 goes to 3, 1 goes to 4, and 2 goes to 5.  When I say "assignment pair," I am referring to one integer mapping to another, like 0 going to 3.  I thought that using a list of integers together with their index was potentially more elegant than a list of coordinate pairs to represent assignment pairs, although the "reverseRotor" function is a little messy as a result.  The count represents how many times the rotor has been stepped up (i.e. how many times the assignment pairs have been shifted up by one).  It was called the "ring setting."  



--STEPPING THE ROTORS----------------------------------

--one way of dealing with the Maybe in elemIndex. If the code works properly, "Nothing" will never be returned.  
fix :: Maybe Int -> Int
fix Nothing = error "Whoops"
fix (Just i) = i

--takes a rotor and steps one assignment pair.  If 'A' went to 'S', then 'B' will now go to 'T'.
stepOne :: Rotor -> Int -> Int
stepOne (rs,count) i = mod stepped (length rs)
    where index = fix $ elemIndex i rs
          stepped = (rs !! (index+1)) - 1

--steps an entire rotor, and increases the "count" by 1. We use stepOne for all the assignments except the first one (i.e. the head of our key). The first assignment needs to reference the last element in the key, which is why stepFirst is defined separately.  
stepRot :: Rotor -> Rotor
stepRot (rs,count) = ( newOrder, newCount) 
    where indexLast = length rs - 1
          stepLast = mod (head rs + indexLast) (length rs) 
          newOrder = map (stepOne (rs,count)) (init rs) ++ [stepLast] 
          newCount = mod (count+1) 26


--steps a whole rotor set.  If the first rotor makes one full revolution (i.e. the rotor count reaches 26), then that rotor resets the count, and the next rotor  to the right is stepped.
stepSet :: [Rotor] -> [Rotor]
stepSet [] = []
stepSet ((order,25):rs) = stepRot (order,25) : stepSet rs
stepSet (r:rs) = (stepRot r) : rs 



--ENCRYPTING CHARACTERS AS INTEGERS-------------------------

--takes an integer, runs it forward through the rotor set, including the special last rotor.  The output of one rotor becomes the index for the key of the next rotor.  
runThru :: [Rotor] -> Int -> Int
runThru rotors i = foldr (!!) i orders
    where orders = map fst (reverse rotors)
--foldr here looks like rotor3 !! rotor2 !! (rotor1 !! i)

--reverses the rotors so that if 'A' went to 'S', now 'S' goes to 'A'.  This is the first step in creating the returning path for an integer being encrypted.  
reverseRotor :: Rotor -> Rotor 
reverseRotor (rs, count) = (map snd sorted, count) 
    where k = length rs -1
          zips = zip rs [0..k]
          sorted = sort zips

--creates the set of rotors that resemble the returning path of an integer being encrypted.  It doesn't include the last rotor (the "reflector"), which explains "init".  It reverses the assignment pairs using "reverseRotor".  It reverses the order of the rotor set to simulate traveling backward through the rotor set.  
backward :: [Rotor] -> [Rotor]
backward rotors = reverse $ init $ (map reverseRotor rotors)

--takes one integer, runs it forward through the rotor set, then runs it backward through the set.  Again, the integer runs forward through the last rotor, but doesn't run backward through the last rotor.  
enigmaOne :: [Rotor] -> Int -> Int
enigmaOne rotors int = runThru backwardRotors (runThru rotors int) 
    where backwardRotors = backward rotors

--encrypts a whole list of integers, and steps the rotor set after each integer.  
enigmaNum :: [Rotor] -> [Int] -> [Int]
enigmaNum _ [] = []
enigmaNum rotors (i:is) = (enigmaOne rotors i) : (enigmaNum (stepSet rotors) is )



--INTEGER CHARACTER CONVERSIONS------------

charToInt :: Char -> Int
charToInt a
    | isUpper a = ord a - 65
    | isLower a = ord a - 97
    | otherwise = error "That's not a letter. Remember, no spaces either."

intToChar :: Int -> Char
intToChar i = ['A'..'Z'] !! i

intsToChars :: [Int] -> [Char]
intsToChars = map intToChar

charsToInts :: [Char] -> [Int]
charsToInts = map charToInt



-- !~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!
---GRAND MASTER FUNCTION--~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!

--takes a list of letters, converts them into integers, encrypts them, and then turns them back into a list of letters.
enigma :: [Rotor] -> String -> String 
enigma rotor string = intsToChars $ enigmaNum rotor integers
    where integers = charsToInts string

-- !~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!
-- !~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!~!

 
--SAMPLE ROTOR SETS--------------------------
--http://en.wikipedia.org/wiki/Enigma_rotor_details

i_GermanRailway_Rocket   = ['J','G','D','Q','O','X','U','S','C','A','M','I','F','R','V','T','P','N','E','W','K','B','L','Z','Y','H']
ii_GermanRailway_Rocket  = ['N','T','Z','P','S','F','B','O','K','M','W','R','C','J','D','I','V','L','A','E','Y','U','X','H','G','Q']
iii_GermanRailway_Rocket = ['J','V','I','U','B','H','T','C','D','Y','A','K','E','Q','Z','P','O','S','G','X','N','R','M','W','F','L']
ukw_GermanRailway_Rocket = ['Q','Y','H','O','G','N','E','C','V','P','U','Z','T','F','D','J','A','X','W','M','K','I','S','R','B','L']

--ring setting (or count) is determined by wikipedia table "turnover positions"
i = (charsToInts i_GermanRailway_Rocket, 9)
ii = (charsToInts ii_GermanRailway_Rocket, 21)
iii = (charsToInts iii_GermanRailway_Rocket, 4)
ukw = (charsToInts ukw_GermanRailway_Rocket, 0)

grr1941 = [i,ii,iii,ukw]

test = enigma grr1941


r1 :: Rotor
r1 = ([3,6,9,12,15,18,21,24,1,4,7,10,0,13,16,19,22,25,2,5,8,11,14,17,20,23],0)
r2 :: Rotor
r2 = ([9,13,15,18,20,21,25,0,3,4,6,7,10,12,14,17,19,22,24,1,2,5,8,11,16,23],0)
r3 :: Rotor
r3 = ([1,3,6,10,15,21,25,24,23,22,20,19,18,17,16,14,13,11,12,9,7,8,2,5,4,0],0)

rs :: [Rotor]
rs = [r1,r2,r3,ukw]

test2 :: String -> String 
test2 = enigma rs

-----------------------------------------




----SCRATCH WORK----
{-
halfenigma :: [Rotor] -> [Int] -> [Int]
halfenigma _ [] = []
halfenigma rotors (i:is) = runThru rotors i : halfenigma stepR is
    where stepR = stepSet rotors 
-}

{-
input :: Rotor -> Int -> Int 
input rotors i = rotors !! i
-}

{-
--takes a rotor and steps one assignment pair.  If 'A' went to 'S', then 'B' will now go to 'T'.
stepOne :: Rotor -> Int -> Int
stepOne (rs,count) i = mod stepped (length rs)
    where index = fix $ elemIndex i rs
          stepped = (rs !! (index-1) ) - (index-1) + index

--steps an entire rotor, and increases the "count" by 1. We use stepOne for all the assignments except the first one (i.e. the head of our key). The first assignment needs to reference the last element in the key, which is why stepFirst is defined separately.  
stepRot :: Rotor -> Rotor
stepRot (rs,count) = ( newOrder, newCount) 
    where indexLast = length rs - 1
          stepFirst = mod ((rs !! indexLast) - indexLast) (length rs)
          newOrder = stepFirst : map (stepOne (rs,count)) (tail rs)
          newCount = mod (count+1) 26
-}
