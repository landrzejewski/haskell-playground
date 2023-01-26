{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Lib
  ( anagrams,
    users,
  )
where

import Data.Char (isAlpha, isAlphaNum, isSpace)
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

-- Users

trimLeading :: String -> Maybe String
trimLeading "" = Nothing
trimLeading (x : xs) =
  if isSpace x
    then trimLeading xs
    else Just (x : xs)

requireLength :: Int -> Int -> String -> Maybe String
requireLength min max xs =
  if len >= min && len <= max
    then Just xs
    else Nothing
  where
    len = length xs

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  if all isAlphaNum xs
    then Just xs
    else Nothing

checkPassword :: String -> Maybe String
{-
checkPassword text = case trimLeading text of
  Nothing -> Nothing
  Just password -> case requireLength 2 8 password of
    Nothing -> Nothing
    Just _ -> case requireAlphaNum password of
      Nothing -> Nothing
      Just _ -> Just password
-}
checkPassword password = trimLeading password >>= requireLength 2 8 >>= requireAlphaNum

users :: IO ()
users = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (checkPassword password)
