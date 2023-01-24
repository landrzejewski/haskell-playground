{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Lib
  ( anagrams,
  )
where

import Data.Char (isAlpha)
import Data.List (sort)

-- if-else (only boolean) vs. case

min :: Int -> Int -> Int
min x y = if x < y then x else y

min' :: Int -> Int -> Int
min' x y = case x < y of
  True -> x
  _ -> y

-- Anagrams

isWord :: String -> Maybe String
isWord word
  | null word = Nothing
  | all isAlpha word = Just word
  | otherwise = Nothing

isAnagram :: String -> String -> Bool
isAnagram first second = sort first == sort second

checkAnagram :: String -> String -> String
checkAnagram first second = case isWord first of
  Nothing -> "The first word is invalid"
  Just _ -> case isWord second of
    Nothing -> "The second word is invalid"
    Just _ -> if isAnagram first second then "These words are anagrams" else "These words are not anagrams"

anagrams :: IO ()
anagrams = do
  putStr "Please enter a word:\n> "
  first <- getLine
  putStr "Please enter a second word:\n> "
  second <- getLine
  print (checkAnagram first second)
