{-# LANGUAGE OverloadedStrings #-}

module Util
( dropFirstAndLastXPercent
, eqListElems
, hasLetters
, lowerString
, trim
, unquote
) where

import Data.Char (isLetter, isSpace, toLower)
import qualified Data.Set as S

-- |Transform a String to a lower case version.
lowerString :: String -> String
lowerString = map toLower

-- |True if String contains at least one letter.
hasLetters :: String -> Bool
hasLetters = any isLetter

-- |Delete leading and ending quaotes.
unquote :: String -> String
unquote s =
    let
      s'  = if head s == '"' then tail s else s
      s'' = if last s' == '"' then init s' else s'
    in
      s''

-- |Trim leading and trailing whitespace.
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- |True, if both lists contain the same elements.
eqListElems :: (Ord a) => [a] -> [a] -> Bool
eqListElems x y = S.fromList x == S.fromList y

-- |Drop the first and last p percent of the list elements.
dropFirstAndLastXPercent :: Int -> [a] -> [a]
dropFirstAndLastXPercent p xs =
    dropLastXPercent p (dropFirstXPercent p xs)

-- |Drop the first p percent of the list elements.
dropFirstXPercent :: Int -> [a] -> [a]
dropFirstXPercent p xs =
    let n = round (fromIntegral (length xs) * toPercent p)
    in  drop n xs

-- |Drop the last p percent of the list elements.
dropLastXPercent :: Int -> [a] -> [a]
dropLastXPercent p xs = reverse $ dropFirstXPercent p (reverse xs)

-- |Convert p to p / 100.
toPercent :: Int -> Double
toPercent p = fromIntegral p / 100

