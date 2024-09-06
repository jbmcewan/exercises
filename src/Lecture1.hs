{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use max" #-}

-- |
-- Module                  : Lecture1
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 1 of the Haskell Beginners course.
--
-- To complete exercises, you need to complete implementation and add
-- missing top-level type signatures. You can implement any additional
-- helper functions. But you can't change the names of the given
-- functions.
--
-- Comments before each function contain explanations and example of
-- arguments and expected returned values.
--
-- It's absolutely okay if you feel that your implementations are not
-- perfect. You can return to these exercises after future lectures and
-- improve your solutions if you see any possible improvements.
module Lecture1
  ( makeSnippet,
    sumOfSquares,
    lastDigit,
    minmax,
    subString,
    strSum,
    lowerAndGreater,
  )
where
import Data.Ord (clamp)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


-- VVV If you need to import libraries, do it after this line ... VVV

-- ^ ^^ and before this line. Otherwise the test suite might fail  ^^^

-- | Specify the type signature of the following function. Think about
-- its behaviour, possible types for the function arguments and write the
-- type signature explicitly.
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

-- | Implement a function that takes two numbers and finds sum of
-- their squares.
--
-- >>> sumOfSquares 3 4
-- 25
--
-- >>> sumOfSquares (-2) 7
-- 53
--
-- Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
-- is 25.

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Num a => a -> a -> a
sumOfSquares x y =
  let e = 2 :: Integer
   in x ^ e + y ^ e

-- | Implement a function that returns the last digit of a given number.
--
-- >>> lastDigit 42
-- 2
-- >>> lastDigit (-17)
-- 7
--
-- 🕯 HINT: use the @mod@ functionn

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE

lastDigit :: Integral a => a -> a
lastDigit n = abs n `mod` 10

-- | Write a function that takes three numbers and returns the
-- difference between the biggest number and the smallest one.
--
-- >>> minmax 7 1 4
-- 6
--
-- Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
-- and 1 is the smallest, and 7 - 1 = 6.
--
-- Try to use local variables (either let-in or where) to implement this
-- function.
minmax :: (Num a, Ord a) => a -> a -> a -> a
minmax x y z =
  let m
        | x < y = if x < z then x else z
        | y < z = y
        | otherwise = z

      n
        | x > y = if x > z then x else z
        | y > z = y
        | otherwise = z
   in n - m


-- | Implement a function that takes a string, start and end positions
-- and returns a substring of a given string from the start position to
-- the end (including).
--
-- >>> subString 3 7 "Hello, world!"
-- "lo, w"
--
-- >>> subString 10 5 "Some very long String"
-- ""
--
-- This function can accept negative start and end position. Negative
-- start position can be considered as zero (e.g. substring from the
-- first character) and negative end position should result in an empty
-- string.
subString :: Int -> Int -> String -> String
subString start end str = 
  -- if end is negative return empty string
  if end < 0 then "" 
    else let
        -- clamping function to keep index in range
        k = clamp(0, length str - 1)
        -- start index 
        s = k start
        -- end index
        e = k end
        -- transfer count
        x = e - s + 1 
      in -- no optimization doe with regard to boundary cases as assume the take/drop is optimized for them
        take x (drop s str)
  

-- | Write a function that takes a String — space separated numbers,
-- and finds a sum of the numbers inside this string.
--
-- >>> strSum "100    -42  15"
-- 73
--
-- The string contains only spaces and/or numbers.strSum :: String -> Int
strSum :: String -> Int
strSum s = sum (map stringToIntDefault (words s))
  where
     -- Function to convert a string to an integer with a default value
    stringToIntDefault :: String -> Int
    stringToIntDefault str = fromMaybe 0 (readMaybe str)

-- | Write a function that takes a number and a list of numbers and
-- returns a string, saying how many elements of the list are strictly
-- greater than the given number and strictly lower.
--
-- >>> lowerAndGreater 3 [1 .. 9]
-- "3 is greater than 2 elements and lower than 6 elements"
--
-- Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- The given number 3 is greater than 2 elements (1 and 2)
-- and lower than 6 elements (4, 5, 6, 7, 8 and 9).
--
-- 🕯 HINT: Use recursion to implement this function.
lowerAndGreater :: Int -> [Int] -> String
lowerAndGreater spec list =
  let go :: Int -> Int -> [Int] -> String
      -- Terminal case, end of string, report results
      go lt gt [] = show spec ++ " is greater than " ++ show gt ++ " elements and lower than " ++ show lt ++ " elements"
      -- A character and list case 
      go lt gt (x : xs) =
        let -- Calculate new less-than value based on if the spec < x  
            lt'
              | spec < x = lt + 1
              | otherwise = lt
            -- Calculate the nee greater-than value based on the spec > x
            gt'
              | spec > x = gt + 1
              | otherwise = gt
         in -- Recurse with remainder of list
         go lt' gt' xs
   in go 0 0 list
