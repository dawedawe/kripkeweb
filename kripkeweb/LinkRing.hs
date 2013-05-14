{-# LANGUAGE OverloadedStrings #-}

module LinkRing
( formatRingsAsCols
, inLinkRings
, outLinkRings
) where

import Control.Monad (liftM)
import Data.List (nub, union)
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB

-- |Construct list of link rings.
-- The given world is the target of the worlds in ring 1, these worlds are the
-- target of the worlds in ring 2...
inLinkRings :: Connection -> T.Text -> IO [[T.Text]]
inLinkRings c world =
    liftM ([world] :) (linkRings' c sourcesOf [world] [world])

-- |Construct list of link rings.
-- The given world is the target of the worlds in ring 1, these worlds are the
-- target of the worlds in ring 2...
outLinkRings :: Connection -> T.Text -> IO [[T.Text]]
outLinkRings c world =
    liftM ([world] :) (linkRings' c targetsOf [world] [world])

-- |Helper for linkRings
linkRings' :: Connection -> (Connection -> T.Text -> IO [T.Text]) ->
              [T.Text] -> [T.Text] -> IO [[T.Text]]
linkRings' c linkFunc r0 visited = do
    r1 <- mapM (linkFunc c) r0
    let r1' = filter (`notElem` visited ) (nub (concat r1))
    if null r1'
      then return []
      else liftM (r1' :) (linkRings' c linkFunc r1' (visited `union` r1'))

-- |Transpose a list of lists of Text.
formatRingsAsCols :: [[T.Text]] -> [T.Text]
formatRingsAsCols = foldr zipTexts []

-- |Zip two Text lists with some space betwenn the elements and the shorter one
-- filled up to match the longer one.
zipTexts :: [T.Text] -> [T.Text] -> [T.Text]
zipTexts t1 t2 =
    let
      (ft1, ft2) = fillup t1 t2
      jt1        = justLeftToLongest ft1
      jt2        = justLeftToLongest ft2
      h s1 s2    = s1 `T.append` "    " `T.append` s2
    in
      zipWith h jt1 jt2

-- |Fill up the shorter of the two lists with words containing spaces.
fillup :: [T.Text] -> [T.Text] -> ([T.Text], [T.Text])
fillup xs ys =
    let
      lxs = length xs
      lys = length ys
      mx = max lxs lys
      mi = min lxs lys
      fil = replicate (mx - mi) ""
    in
      if length xs == mi
        then (xs ++ fil, ys)
        else (xs, ys ++ fil)

-- |Left justify all elements to the length of the longest element.
justLeftToLongest :: [T.Text] -> [T.Text]
justLeftToLongest xs =
    let maxL = maximum (map T.length xs)
    in  map (T.justifyLeft maxL ' ') xs

