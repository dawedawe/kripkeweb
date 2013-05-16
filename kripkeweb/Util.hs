{-# LANGUAGE OverloadedStrings #-}

module Util
( dropRelsWithElemIn
, flattenTuples
, hasLetters
, lowerString
, trim
, unquote
) where

import Data.Char (isLetter, isSpace, toLower)
import qualified Data.List as L

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

-- |Drop relations containing one or two elements of the given list.
dropRelsWithElemIn :: (Eq a) => [a] -> [(a, a)] -> [(a, a)]
dropRelsWithElemIn xs = filter (\(x, y) -> x `notElem` xs && y `notElem` xs)

-- |Take the elements out of every tuple and remove duplicates.
flattenTuples :: (Eq a) => [(a, a)] -> [a]
flattenTuples xs = L.nub $ concatMap (\(x, y) -> [x, y]) xs
