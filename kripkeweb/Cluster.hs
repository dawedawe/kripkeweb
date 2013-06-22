module Cluster
( boolsDist
, fmlSpacePos
, fmlSpacePoses
) where

import qualified Data.List as L
import qualified Data.List.Utils as LU (replace)
import Data.Maybe (catMaybes, isJust, fromJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import System.Random

import DB
import KripkeTypes
import LogicSearch
import Util

data SpacePnt = SpacePnt { name     :: T.Text
                         , position :: [Bool]
                         } deriving (Eq)

instance Show SpacePnt where
    show (SpacePnt n p) = show (n, show p)

-- |fmlSpacePos vectors of all given worlds.
fmlSpacePoses :: (AsLambdaType f, PTrueIn f) => Model -> [f] -> [T.Text] ->
                 [SpacePnt]
fmlSpacePoses mdl fmls ws =
    map (fmlSpacePos mdl fmls) ws

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
      p  = fromIntegral (min ts fs) / fromIntegral (max ts fs) :: Double
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
kMeans mdl@(Model (Frame w _) _) fmls k = do
    let dim    = length fmls
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
kMeansLoop clusters centroids _ 0 = zip centroids clusters
kMeansLoop clusters centroids k j = do
    let hOfx       = map (closestCentroidIdx centroids) (concat clusters)
    let wk         = zip (concat clusters) hOfx
    let clusters'  =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    -- new centroids
    let centroids' = map (sumBoolLists . map position) clusters'
    -- deal with possible empty clusters and the resulting null-like centroids
    let centroids'' = keepCentroidsOfEmptyClusters centroids centroids'
    if centroids'' /= centroids
      then kMeansLoop clusters' centroids'' k (pred j)
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
clusterSim :: Model -> [SpacePnt] -> Maybe Double
clusterSim (Model _ lam) cluster
    | length cluster < 2 = Nothing
    | otherwise          =
        let
          wlams = map (S.fromList . lam . name) cluster
          sims  = concat [map (similarity l) (L.delete l wlams) | l <- wlams]
        in
          Just (sum sims / fromIntegral (length sims))

-- |Disimilarity between the formula sets of two clusters.
clusterDisim :: Model -> [SpacePnt] -> [SpacePnt] -> Maybe Double
clusterDisim (Model _ lam) c1 c2
    | null c1 || null c2 = Nothing
    | otherwise          =
        let
          c1ls = concatMap (lam . name) c1
          c2ls = concatMap (lam . name) c2
        in
          Just (disimilarity (S.fromList c1ls) (S.fromList c2ls))


-- |Disimilarity scores of one cluster to a list of other clusters.
clusterDisims :: Model -> [[SpacePnt]] -> [SpacePnt] -> [Maybe Double]
clusterDisims mdl clusters cl = map (clusterDisim mdl cl) (L.delete cl clusters)

-- |Disimilarity scores of one cluster to a list of clusters, maybe including it
-- self.
clusterDisims' :: Model -> [[SpacePnt]] -> [SpacePnt] -> [Maybe Double]
clusterDisims' mdl clusters cl = map (clusterDisim mdl cl) clusters

-- |Average similarity of a list of clusters.
avgClusterSim :: Model -> [[SpacePnt]] -> Double
avgClusterSim mdl clusters
    | null clusters = error "avgClusterSim: no clusters given"
    | otherwise     =
        let ss  = mapMaybe (clusterSim mdl) clusters
        in  sum ss / fromIntegral (length ss)

-- |Average disimilarity among a list of clusters.
avgClusterDisim :: Model -> [[SpacePnt]] -> Double
avgClusterDisim mdl clusters
    | length (filter (not . null) clusters) < 2 =
        error "avgClusterDisim: < 2 unempty clusters given"
    | otherwise                                 =
        let
          ds  = map (clusterDisims mdl clusters) clusters
          ds' = catMaybes (concat ds)
        in
          sum ds' / fromIntegral (length ds')

-- |Merge clusters with too small a disimilarity, fill up empty spots with empty
-- lists.
mergeClusters :: Model -> [[SpacePnt]] -> [[SpacePnt]]
mergeClusters mdl clusters =
    let
      mergeList = dropOverlappingPairs (mergeCandidates mdl clusters)
    in
      workOffMergeList clusters mergeList

-- |Merge clusters in given list, don't touch any overlapping pairs, replace
-- second cluster of a merge pair with an empty cluster.
workOffMergeList :: [[SpacePnt]] -> [(Int, Int)] -> [[SpacePnt]]
workOffMergeList clusters []     = clusters
workOffMergeList clusters (x:xs) =
    let
        c1  = clusters !! fst x
        c2  = clusters !! snd x
        cm  = c1 ++ c2
        c'  = LU.replace [c1] [cm] clusters
        c'' = LU.replace [c2] [[]] c'
    in
      workOffMergeList c'' xs

-- |Index pairs of clusters with a too small disimilarity.
mergeCandidates :: Model -> [[SpacePnt]] -> [(Int, Int)]
mergeCandidates mdl clusters =
    let
      ds = map (clusterDisims' mdl clusters) clusters
      mcs = map mergeCandidate ds
      pairs = zip [0..] mcs
      pairs' = filter (isJust . snd) pairs
    in
      map (\(x, Just y) -> (x, y)) pairs'

-- |Index of first cluster with 0.0 < disimilarity < 0.9.
mergeCandidate :: [Maybe Double] -> Maybe Int
mergeCandidate =
    L.findIndex (\d -> isJust d && fromJust d > 0.0 && fromJust d < 0.9)

-- |Indexes of Clusters with too small a similarity.
splitCandidates :: Model -> [[SpacePnt]] -> [Int]
splitCandidates mdl clusters =
    let
      sims = map (clusterSim mdl) clusters
    in
      [i | i <- [0..(length sims - 1)], let s = sims !! i, isJust s,
        fromJust s < 0.01]

-- |Edge count in a total directed graph.
edgesInDigraphClique :: Int -> Int
edgesInDigraphClique n = n * (n - 1)

-- |Links in a cluster / edge count of a digraph clique.
clusterToDigraphCliqueMeasure :: Connection -> [SpacePnt] -> IO Double
clusterToDigraphCliqueMeasure _ []      =
    error "clusterToTotalDGraphMeasure: empty cluster given"
clusterToDigraphCliqueMeasure c cluster = do
    let ws = map name cluster
    let n  = length ws
    let t  = edgesInDigraphClique n
    lc     <- linkCountAmongWorlds c ws
    return (fromIntegral lc / fromIntegral t)

