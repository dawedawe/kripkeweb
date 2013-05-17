{-# LANGUAGE OverloadedStrings #-}

module Util
( dropRelsWithElemIn
, dropRelsWithElemInS
, flattenTuples
, flattenTupleSet
, hasLetters
, lowerString
, relsStartingWith'
, targetsOf'
, trim
, unquote
) where

import Data.Char (isLetter, isSpace, toLower)
import qualified Data.List as L
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

-- |Drop relations containing one or two elements of the given list.
dropRelsWithElemIn :: (Eq a) => [a] -> [(a, a)] -> [(a, a)]
dropRelsWithElemIn xs = filter (\(x, y) -> x `notElem` xs && y `notElem` xs)

-- |Drop relations containing one or two elements of the given list.
dropRelsWithElemInS :: (Eq a, Ord a) => S.Set a -> S.Set (a, a) -> S.Set (a, a)
dropRelsWithElemInS xs =
    S.filter (\(x, y) -> x `S.notMember` xs && y `S.notMember` xs)

-- |Take the elements out of every tuple and remove duplicates.
flattenTuples :: (Eq a) => [(a, a)] -> [a]
flattenTuples xs = L.nub $ concatMap (\(x, y) -> [x, y]) xs

-- |Take the elements out of every tuple and remove duplicates.
flattenTupleSet :: (Eq a, Ord a) => S.Set (a, a) -> S.Set a
flattenTupleSet xs = S.unions (map (\(x, y) -> S.fromList [x, y]) (S.toList xs))

-- |Pure version of relsStartingWith.
relsStartingWith' :: (Eq a) => [(a, a)] -> a -> [(a, a)]
relsStartingWith' rels w = filter ((== w) . fst) rels

-- |Pure version of targetsOf
targetsOf' :: (Eq a) => [(a, a)] -> a -> [a]
targetsOf' rels w = map snd (relsStartingWith' rels w)


