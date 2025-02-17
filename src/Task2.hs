{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (reverse, map, sum, doubleEveryOtherLast, toDigits, calc)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN m f list = calc m (sum (map (normalizeN m) (doubleEveryOtherLast (map f list))))

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt ch
    | ch >= '0' && ch <= '9' = fromEnum ch - fromEnum '0'
    | ch >= 'a' && ch <= 'f' = fromEnum ch - fromEnum 'a' + 10
    | ch >= 'A' && ch <= 'F' = fromEnum ch - fromEnum 'A' + 10
    | otherwise = error "Invalid hexadecimal digit"

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec n = luhnDec (init (toDigits n)) == fromIntegral (last (toDigits n))

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex list = luhnHex (init list) == digitToInt (last list)


-----------------------------------
--
-- Generic function for getting the last elem in the list 
--
-- Usage example:
--
-- >>> last  "abc"
-- 'c'
-- >>> last [1, 2, 3]
-- 3

last :: [a] -> a
last []     = error "List is empty"
last [x]    = x 
last (_:xs) = last xs 


-----------------------------------
--
-- Generic function for dropping the last elem in the list 
--
-- Usage example:
--
-- >>> init  "abc"
-- "ab"
-- >>> init [1, 2, 3]
-- [1, 2]

init :: [a] -> [a]
init []     = error "List is empty"
init [_]    = [] 
init (x:xs) = x : init xs 


-----------------------------------
--
-- Normalizes given number to single digit by subtracting N-1
-- if it is greater than or equal to N
--
--
-- Usage example:
--
-- >>> normalizeN 10 12
-- 3
-- >>> normalizeN 16 24
-- 9

normalizeN :: Int -> Int -> Int
normalizeN m n
    | n >= m     = n - (m - 1)
    | otherwise  = n

