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
                 [[Bool]]
fmlSpacePoses mdl@(Model (Frame w r) _) fmls ws =
    map (fmlSpacePos mdl fmls) (S.toList w)

-- |fmlSpacePos vectors of all given worlds.
fmlSpacePoses' :: (AsLambdaType f, PTrueIn f) => Model -> [f] -> [T.Text] ->
                 [SpacePnt]
fmlSpacePoses' mdl@(Model (Frame w r) _) fmls ws =
    map (fmlSpacePos' mdl fmls) (S.toList w)

-- |Position of a world with regard to a list of formulas spanning a space.
-- Expects the formulas to be already in the right LambdaType.
fmlSpacePos :: (PTrueIn f) => Model -> [f] -> T.Text -> [Bool]
fmlSpacePos mdl fmls w = map (isPTrueInWorld mdl w) fmls

-- |Position of a world with regard to a list of formulas spanning a space.
-- Expects the formulas to be already in the right LambdaType.
fmlSpacePos' :: (PTrueIn f) => Model -> [f] -> T.Text -> SpacePnt
fmlSpacePos' mdl fmls w = SpacePnt w (map (isPTrueInWorld mdl w) fmls)


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

-- |kMeans implementation for a boolean n dimensional space.
kMeans :: (AsLambdaType f, PTrueIn f) => Model -> [f] -> Int ->
          IO [([Bool], [[Bool]])]
kMeans mdl@(Model (Frame w r) l) fmls k = do
    let dims   = length fmls
    centroids  <- mapM (\_ -> randomBools dims) [0..(k - 1)]
    let wPoses = fmlSpacePoses mdl fmls (S.toList w)
    let hOfx   = map (closestCentroidIdx centroids) wPoses
    let wk     = zip wPoses hOfx
    let clusters =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    return (kMeansLoop clusters centroids k)

-- |Looping function for kMeans, ends when centroids stop moving.
kMeansLoop :: [[[Bool]]] -> [[Bool]] -> Int -> [([Bool], [[Bool]])]
kMeansLoop clusters centroids k = do
    let hOfx       = map (closestCentroidIdx centroids) (concat clusters)
    let wk         = zip (concat clusters) hOfx
    let clusters'  =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    let centroids' = map sumBoolLists clusters' -- new centroids
    -- deal with possible empty clusters and the resulting null-like centroids
    let centroids'' = keepEmptyClustCents centroids centroids'
    if centroids'' /= centroids
      then kMeansLoop clusters' centroids'' k
      else zip centroids'' clusters'

-- |kMeans implementation for a boolean n dimensional space.
kMeans' :: (AsLambdaType f, PTrueIn f) => Model -> [f] -> Int ->
          IO [([Bool], [SpacePnt])]
kMeans' mdl@(Model (Frame w r) l) fmls k = do
    let dims   = length fmls
    centroids  <- mapM (\_ -> randomBools dims) [0..(k - 1)]
    let wPoses = fmlSpacePoses' mdl fmls (S.toList w)
    let hOfx   = map (closestCentroidIdx' centroids) wPoses
    let wk     = zip wPoses hOfx
    let clusters =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    return (kMeansLoop' clusters centroids k)

-- |Looping function for kMeans, ends when centroids stop moving.
kMeansLoop' :: [[SpacePnt]] -> [[Bool]] -> Int -> [([Bool], [SpacePnt])]
kMeansLoop' clusters centroids k = do
    let hOfx       = map (closestCentroidIdx' centroids) (concat clusters)
    let wk         = zip (concat clusters) hOfx
    let clusters'  =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    -- new centroids
    let centroids' = map (sumBoolLists . map position) clusters'
    -- deal with possible empty clusters and the resulting null-like centroids
    let centroids'' = keepEmptyClustCents centroids centroids'
    if centroids'' /= centroids
      then kMeansLoop' clusters' centroids'' k
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
