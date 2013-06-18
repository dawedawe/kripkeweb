module Cluster
( boolsDist
, fmlSpacePos
, fmlSpacePoses
) where

import Control.Monad (liftM)
import qualified Data.List as L
import Data.Maybe (catMaybes, fromJust)
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
                         } deriving (Eq)

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
    let hOfx   = map (closestCentroidIdx centroids) wPoses
    let wk     = zip wPoses hOfx
    let clusters =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    return (kMeansLoop clusters centroids k 5)

-- |Looping function for kMeans, ends when centroids stop moving or after i
-- loops.
kMeansLoop :: [[SpacePnt]] -> [[Bool]] -> Int -> Int -> [([Bool], [SpacePnt])]
kMeansLoop clusters centroids k 0 = zip centroids clusters
kMeansLoop clusters centroids k i = do
    let hOfx       = map (closestCentroidIdx centroids) (concat clusters)
    let wk         = zip (concat clusters) hOfx
    let clusters'  =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    -- new centroids
    let centroids' = map (sumBoolLists . map position) clusters'
    -- deal with possible empty clusters and the resulting null-like centroids
    let centroids'' = keepCentroidsOfEmptyClusters centroids centroids'
    if centroids'' /= centroids
      then kMeansLoop clusters' centroids'' k (pred i)
      else zip centroids'' clusters'

-- |Determine the index number of the closes centroid.
closestCentroidIdx :: [[Bool]] -> SpacePnt -> Int
closestCentroidIdx cs (SpacePnt _ pos) =
    let
      dists = map (boolsDist pos) cs
      minD  = minimum dists
    in
      fromJust (L.elemIndex minD dists)

-- |Take the corresponding element out of the first list if the element in the
-- second list is null.
keepCentroidsOfEmptyClusters :: [[a]] -> [[a]] -> [[a]]
keepCentroidsOfEmptyClusters os ns
    | length os /= length ns = error "keepEmptyClustCents: uneven lists"
    | otherwise              =
        map (\(o, n) -> if null n then o else n) (zip os ns)

-- |Simple similarity measure: |x intersect y| / |x union y|
similarity :: (Eq a, Ord a) => S.Set a -> S.Set a -> Double
similarity x y
    | S.null x && S.null y = error "similarity: empty sets"
    | otherwise            =
        let
          interSize = S.size (x `S.intersection` y)
          unionSize = S.size (x `S.union` y)
        in
          fromIntegral interSize / fromIntegral unionSize

-- |Simple disimilarity measure: 1 - similarity x y
disimilarity :: (Eq a, Ord a) => S.Set a -> S.Set a -> Double
disimilarity x y
    | S.null x && S.null y = error "disimilarity: empty sets"
    | otherwise            = 1 - similarity x y

-- |Average similarity of pairs in a cluster.
clusterSim :: Connection -> LambdaType -> [SpacePnt] -> IO (Maybe Double)
clusterSim c lamType []      = return Nothing
clusterSim c lamType (x:[])  = return Nothing
clusterSim c lamType cluster = do
    let ws    = map name cluster
    ls        <- mapM (worldFormulas c lamType) ws
    let ls'   = map S.fromList ls
    let wsims = [map (similarity l) (L.delete l ls') | l <- ls']
    let sims  = concat wsims
    return (Just (sum sims / fromIntegral (length sims)))

-- |Disimilarity between the formula sets of two clusters.
clusterDisim :: Connection -> LambdaType -> [SpacePnt] -> [SpacePnt] ->
                IO (Maybe Double)
clusterDisim c lamType c1 c2
    | null c1 || null c2 = return Nothing
    | otherwise          = do
        let c1ws = map name c1
        let c2ws = map name c2
        c1ls <- liftM concat (mapM (worldFormulas c lamType) c1ws)
        c2ls <- liftM concat (mapM (worldFormulas c lamType) c2ws)
        return (Just (disimilarity (S.fromList c1ls) (S.fromList c2ls)))

-- |Disimilarity scores of one cluster to a list of other clusters.
clusterDisims :: Connection -> LambdaType -> [[SpacePnt]] -> [SpacePnt] ->
                 IO [Maybe Double]
clusterDisims c lamType clusters cl =
    mapM (clusterDisim c lamType cl) (L.delete cl clusters)

-- |Average similarity of a list of clusters.
avgClusterSim :: Connection -> LambdaType -> [[SpacePnt]] -> IO Double
avgClusterSim c lamType clusters
    | length clusters < 1 = error "avgClusterSim: < 1 clusters given"
    | otherwise           = do
      ss <- mapM (clusterSim c lamType) clusters
      let ss' = catMaybes ss
      return (sum ss' / fromIntegral (length ss'))

-- |Average disimilarity among a list of clusters.
avgClusterDisim :: Connection -> LambdaType -> [[SpacePnt]] -> IO Double
avgClusterDisim c lamType clusters
    | length clusters < 2 = error "avgClusterDisim: < 2 clusters given"
    | otherwise           = do
      ds <- mapM (clusterDisims c lamType clusters) clusters
      let ds' = catMaybes (concat ds)
      return (sum ds' / fromIntegral (length ds'))

-- |Edge count in a total directed graph.
edgesInDigraphClique :: Int -> Int
edgesInDigraphClique n = n * (n - 1)

-- |Links in a cluster / edge count of a digraph clique.
clusterToDigraphCliqueMeasure :: Connection -> [SpacePnt] -> IO Double
clusterToDigraphCliqueMeasure c []      =
    error "clusterToTotalDGraphMeasure: empty cluster given"
clusterToDigraphCliqueMeasure c cluster = do
    let ws = map name cluster
    let n  = length ws
    let t  = edgesInDigraphClique n
    lc     <- linkCountAmongWorlds c ws
    return (fromIntegral lc / fromIntegral t)

