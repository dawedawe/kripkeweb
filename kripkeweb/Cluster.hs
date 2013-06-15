module Cluster
( boolsDist
, fmlSpacePos
, fmlSpacePoses
) where

import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import System.Random

import DB
import KripkeTypes
import LogicSearch
import Model
import Tfidf

data SpacePnt = SpacePnt { name     :: T.Text
                         , position :: [Bool]
                         }

instance Show SpacePnt where
    show (SpacePnt n p) = show (n, show p)

-- |fmlSpacePos vectors of all given worlds.
fmlSpacePoses :: (AsLambdaType f, PTrueIn f) => Model -> [f] -> [T.Text] ->
                 [SpacePnt]
fmlSpacePoses mdl@(Model (Frame w r) _) fmls ws =
    map (fmlSpacePos mdl fmls) (S.toList w)

-- |Position of a world with regard to a list of formulas spanning a space.
-- Expects the formulas to be already in the right LambdaType.
fmlSpacePos :: (PTrueIn f) => Model -> [f] -> T.Text -> SpacePnt
fmlSpacePos mdl fmls w = SpacePnt w (map (isPTrueInWorld mdl w) fmls)


-- |Distance of two Bool Lists.
boolsDist :: [Bool] -> [Bool] -> Double
boolsDist x y
    | length x == length y = sqrt (fromIntegral (sum (zipWith boolDist x y)))
    | otherwise            = error "boolsDist: uneven lists"

-- |Distance of two Bools.
boolDist :: Bool -> Bool -> Int
boolDist x y
    | x == y    = 0
    | otherwise = 1

-- |True, if more or equal Trues than Falses are in the list.
majorityAdd :: [Bool] -> Bool
majorityAdd xs =
    let trueLen  = length (filter (== True) xs)
    in  trueLen >= (length xs - trueLen)

-- |Sum Lists of Bool with the help of majorityAdd.
sumBoolLists :: [[Bool]] -> [Bool]
sumBoolLists [] = []
sumBoolLists xs =
    let cols = [map (!! i) xs | i <- [0..(length (head xs) -1)]]
    in  map majorityAdd cols

-- |Generate n random Bools.
randomBools :: Int -> IO [Bool]
randomBools n = do
    g <- newStdGen
    return (take n (randoms g))

-- |Minimum distance between two boolean space positions.
minCentsDist :: [Bool] -> [Bool] -> Bool
minCentsDist xs ys = boolsDist xs ys >= sqrt (fromIntegral (length xs `div` 2))

-- |Minimum randomness of a boolean space position.
minCentRandomness :: [Bool] -> Bool
minCentRandomness centroid =
    let
      ts = length (filter (== True) centroid)
      fs = length (filter (== False) centroid)
      p  = fromIntegral (min ts fs) / fromIntegral (max ts fs)
    in
      p >= 0.3

-- |True, if given list of boolean space positions reach the min quality.
minCentQuality :: [[Bool]] -> Bool
minCentQuality cents =
    let
      rs = map minCentRandomness cents
      dq = [and q | x <- cents, let q = map (minCentsDist x) (L.delete x cents)]
    in
      and rs && and dq

-- |Generate k random centroid positions in a d dimensional boolean space.
-- Stop reaching min quality after i tries.
randomCentroids :: Int -> Int -> Int -> IO [[Bool]]
randomCentroids d k i
    | i >= 0    = do
        centroids <- mapM (\_ -> randomBools d) [0..(k - 1)]
        if minCentQuality centroids || i == 0
          then return centroids
          else randomCentroids d k (pred i)
    | otherwise = error "randomCentroids: argument i < 0"

-- |kMeans implementation for a boolean n dimensional space.
kMeans :: (AsLambdaType f, PTrueIn f) => Model -> [f] -> Int ->
          IO [([Bool], [SpacePnt])]
kMeans mdl@(Model (Frame w r) l) fmls k = do
    let dim    = length fmls
    -- centroids  <- mapM (\_ -> randomBools dims) [0..(k - 1)]
    centroids  <- randomCentroids dim k 20
    let wPoses = fmlSpacePoses mdl fmls (S.toList w)
    let hOfx   = map (closestCentroidIdx' centroids) wPoses
    let wk     = zip wPoses hOfx
    let clusters =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    return (kMeansLoop clusters centroids k 5)

-- |Looping function for kMeans, ends when centroids stop moving or after i
-- loops.
kMeansLoop :: [[SpacePnt]] -> [[Bool]] -> Int -> Int -> [([Bool], [SpacePnt])]
kMeansLoop clusters centroids k 0 = zip centroids clusters
kMeansLoop clusters centroids k i = do
    let hOfx       = map (closestCentroidIdx' centroids) (concat clusters)
    let wk         = zip (concat clusters) hOfx
    let clusters'  =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    -- new centroids
    let centroids' = map (sumBoolLists . map position) clusters'
    -- deal with possible empty clusters and the resulting null-like centroids
    let centroids'' = keepEmptyClustCents centroids centroids'
    if centroids'' /= centroids
      then kMeansLoop clusters' centroids'' k (pred i)
      else zip centroids'' clusters'

-- |Determine the index number of the closes centroid.
closestCentroidIdx :: [[Bool]] -> [Bool] -> Int
closestCentroidIdx cs w =
    let
      dists = map (boolsDist w) cs
      minD  = minimum dists
    in
      fromJust (L.elemIndex minD dists)

-- |Determine the index number of the closes centroid.
closestCentroidIdx' :: [[Bool]] -> SpacePnt -> Int
closestCentroidIdx' cs (SpacePnt _ pos) =
    let
      dists = map (boolsDist pos) cs
      minD  = minimum dists
    in
      fromJust (L.elemIndex minD dists)

-- |Take the corresponding element out of the first list if the element in the
-- second list is null.
keepEmptyClustCents :: [[a]] -> [[a]] -> [[a]]
keepEmptyClustCents os ns
    | length os /= length ns = error "keepEmptyClustCents: uneven lists"
    | otherwise              =
        map (\(o, n) -> if null n then o else n) (zip os ns)
