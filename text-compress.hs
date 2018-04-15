---Fatima Tayeb
--024485

------------------------------------HASKELL COURSEWORK #2-------------------------------------------
{- | This program is part of the G51PGP Haskell coursework given at the University of Nottingham Malaysia Campus
 The purpose of this program is to apply a simple compression scheme called the run-length encoding, where each run of a string is replaced by a single 
 instance of the letter followed with number of its repetition.
The respective decompression algorithm is also written to reverse the process and decode the string -}


---import necessary libraries for char and int implantations.
import Data.Char



----The program is divided into 9 functions , each in charge of a specific task to smoothen the process of compression and decompression, and the code can look elegant.
-----------------EX1---------------------
-- this function, chomp(), extracts the first run in a string , i.e the rest of repeated instances of the head.

--chomp takes in a string and returns a string 
chomp :: String -> String 

--chomp can be defined elegantly as displayed below, essentially the string is added to the output string until the char is equal to the head of the string.
chomp message = takeWhile (== head message) message


-----------------EX2---------------------
-- munch does a similar task like chomp except that it sets a limit to the length of a run string ;9.

munch :: String -> String 
-- a simple 'take 9' built in function can be used to define the function munch using the pre-defined function chomp.

munch message = take 9 (chomp message) 

-----------------EX3---------------------


-- the function runs essentially utilizes the munched function to split the inputted string in to a list of run strings .

runs :: String -> [String]

--pattern matching is used to define the recursive function 'runs'

--base step : if list or string is empty
runs []  = []

--recursive step: munch the string then apply runs to rest of the "un-munched" string and keep doing this untill string is empty

runs xs = munched : runs (drop (length munched) xs)
  where munched = munch xs
  
-----------------EX4---------------------

-- the encode function basically  zips each character with its number of occurrences ,with the limit of its reption being 9 times.
encode :: String -> [(Char,Int)]

--base step.
encode [] = []
--recursive step: extract the first letter in the list and the length of the string and place themp in a tuple , then chop off the list and repeat process untill list is empty.

encode message = ((letter !! 0),(length letter)):(encode (drop (length letter) (message)))
    where (letter:_) = runs message

-----------------EX5---------------------

-- this function reformats the output of the encode function to change it into a string and easen the process of compression.

flatten :: [(Char,Int)] -> String
flatten [] = []

flatten (x:xs) = (fst x):(show (snd x) ++ flatten xs)

-----------------EX6---------------------


-- Finally, the compress function is the main functionality of this program where by by the assitance of the previous utility function a specific string is compressed using the run length encoding scheme.
compress :: String -> String
compress [] = []

--the string is first encoded then flattened.
compress message = flatten (encode message)

-----------------EX7---------------------


-- the decode function preform the exact opposite action of the encode function , the first element in the tuple is repeated n times ( n being number of occurrences ; i.e 2nd element in tuple) the function is applied recursively until list is empty.
decode :: [(Char,Int)] -> String
decode [] = []
decode (x:xs) = ((take (snd x) (repeat (fst x)))) ++ decode xs

-----------------EX8---------------------

-- expand does the opposite of flatten by converting a compressed string  into a list of tuples with the 1st element in the tuple being the letter and the 2nd element being the number of occurrences

expand :: String -> [(Char,Int )]
expand []=[]
expand (letter:occurrence:rest) = (letter, digitToInt(occurrence)):(expand rest)

-----------------EX9---------------------

--by utilizing the expand and decode function a compressed message can be decompressed as defined below.
decompress :: String -> String
decompress [] = []
decompress comp_message = decode (expand comp_message)
---------------------------------------------------------