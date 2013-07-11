{-# LANGUAGE OverloadedStrings #-}

module Util
( appendUrlStrings
, dropFirstAndLastXPercent
, eqListElems
, hasLetters
, isAbsoluteHttpUrl
, isSoundExHash
, isStringAbsoluteHttpUrl
, keepFirstXPercent
, lowerString
, trim
, unquote
) where

import Data.Char (isDigit, isLetter, isSpace, toLower)
import qualified Data.Set as S
import Network.URL

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

-- |Keep the first p percent of the list elements.
keepFirstXPercent :: Int -> [a] -> [a]
keepFirstXPercent p = dropLastXPercent (100 - p)

-- |Convert p to p / 100.
toPercent :: Int -> Double
toPercent p = fromIntegral p / 100

-- |True if String is a valid SoundEx Hash.
isSoundExHash :: String -> Bool
isSoundExHash (c:x:y:z:[]) =
    c `elem` ['A' .. 'Z'] && isDigit x && isDigit y && isDigit z
isSoundExHash _            = False

-- |True, if URL is Absolute and the protocol is HTTP, otherwise False
isAbsoluteHttpUrl :: URL -> Bool
isAbsoluteHttpUrl (URL (Absolute (Host (HTTP _) _ _)) _ _) = True
isAbsoluteHttpUrl _                                        = False

-- |True, if String is an absolute HTTP URL, otherwise False
isStringAbsoluteHttpUrl :: String -> Bool
isStringAbsoluteHttpUrl str =
      case importURL str of
        Nothing -> False
        Just u  -> isAbsoluteHttpUrl u

-- |Deal with trailing, leading slashes.
appendUrlStrings :: String -> String -> String
appendUrlStrings u1 u2
    | last u1 /= '/' && head u2 /= '/' = u1 ++ "/" ++ u2
    | last u1 == '/' && head u2 == '/' = u1 ++ tail u2
    | otherwise                        = u1 ++ u2
