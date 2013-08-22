module ClusterPart
( kMeans
, dynkMeans
) where

import qualified Data.List as L
import qualified Data.List.Utils as LU (replace)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S
import System.Random

import Cluster
import KripkeTypes
import Logic
import Relation

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

-- |Entropy of a boolean space position.
entropy :: [Bool] -> Double
entropy bvec =
    let
      len  = length bvec
      tcnt = length $ filter (== True) bvec
      fcnt = len - tcnt
      p_t  = fromIntegral tcnt / fromIntegral len
      p_f  = fromIntegral fcnt / fromIntegral len
      e_t  = if p_t == 0
               then 0
               else (-p_t) * logBase 2 p_t
      e_f  = if p_f == 0
               then 0
               else (-p_f) * logBase 2 p_f
    in
      e_t + e_f

-- |Minimum entropy of a boolean space position.
minCentEntropy :: [Bool] -> Bool
minCentEntropy centroid = entropy centroid > 0.3

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
      et = map minCentEntropy cents
    in
      and rs && and dq && and et

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
kMeans :: (AsLambdaType f, MTrueIn f) => Model -> [f] -> Int ->
          IO [([Bool], Cluster)]
kMeans mdl@(Model (Frame w _) _) fmls k = do
    let dim    = length fmls
    centroids  <- randomCentroids dim k 20
    let wPoses = fmlSpacePoses mdl fmls (S.toList w)
    let hOfx   = map (closestCentroidIdx centroids) wPoses
    let wk     = zip wPoses hOfx
    let clusters =
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    return (kMeansLoop clusters centroids k 4)

-- |Looping function for kMeans, ends when centroids stop moving or after i
-- loops.
kMeansLoop :: [Cluster] -> [[Bool]] -> Int -> Int -> [([Bool], Cluster)]
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

-- |kMeans implementation for a boolean n dimensional space.
dynkMeans :: (AsLambdaType f, MTrueIn f) => Model -> [f] -> Int ->
          IO [([Bool], Cluster)]
dynkMeans mdl@(Model (Frame w _) _) fmls k = do
    let dim    = length fmls
    let wPoses = fmlSpacePoses mdl fmls (S.toList w)
    centroids  <- randomCentroids dim k 20
    let hOfx   = map (closestCentroidIdx centroids) wPoses
    let wk     = zip wPoses hOfx
    let clusters =
          filter (not . null)
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    return (dynkMeansLoop mdl clusters centroids 20)

-- |Looping function for kMeans, ends when centroids stop moving or after i
-- loops.
dynkMeansLoop :: Model -> [Cluster] -> [[Bool]] -> Int -> [([Bool], Cluster)]
dynkMeansLoop _   clusters centroids 0 = zip centroids clusters
dynkMeansLoop mdl clusters centroids j = do
    let k          = length clusters
    let hOfx       = map (closestCentroidIdx centroids) (concat clusters)
    let wk         = zip (concat clusters) hOfx     -- [(world, k_index)]
    let clusters'  =
          filter (not . null)
          [map fst c | i <- [0..(k - 1)], let c = filter ((== i) . snd) wk]
    -- split if needed
    let clustersSp = splitClusters mdl clusters'
    -- merge if needed
    let clustersMg = mergeClusters mdl clustersSp
    -- new centroids
    let centroids' = map (sumBoolLists . map position) clustersMg
    if centroids' /= centroids
      then dynkMeansLoop mdl clustersMg centroids' (pred j)
      else zip centroids' clustersMg

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
    | length os /= length ns =
        error "keepCentroidsOfEmptyClusters: uneven lists"
    | otherwise              =
        map (\(o, n) -> if null n then o else n) (zip os ns)

--------------------------------------------------------------------------------
-- functions for cluster merging/splitting

-- |Lowest acceptable cluster similarity in decision to split or not to split
minClusterSimilarity :: Double
minClusterSimilarity = 0.2

-- |Lowest acceptable cluster disimilarity in decision to merge or not to merge
minClusterDisimilarity :: Double
minClusterDisimilarity = 0.8

-- |Merge clusters with too small a disimilarity, fill up empty spots with empty
-- lists to maintain the index information, then delete the empty lists.
mergeClusters :: Model -> [Cluster] -> [Cluster]
mergeClusters mdl clusters =
    let mergeList = dropOverlappingPairs (mergeCandidates mdl clusters)
    in  workOffMergeList clusters mergeList

-- |Merge clusters in given list, don't touch any overlapping pairs.
-- Use index of first half for the merger, delete second half after merge.
workOffMergeList :: [Cluster] -> [(Int, Int)] -> [Cluster]
workOffMergeList clusters []     = filter (not . null) clusters
workOffMergeList clusters (x:xs) =
    let
        c1  = clusters !! fst x
        c2  = clusters !! snd x
        c12 = c1 ++ c2
        c'  = LU.replace [c1] [c12] clusters
        c'' = LU.replace [c2] [[]] c'
    in
      workOffMergeList c'' xs

-- |Index pairs of clusters with a too small disimilarity.
mergeCandidates :: Model -> [Cluster] -> [(Int, Int)]
mergeCandidates mdl clusters =
    let
      ds     = map (clusterDisims' mdl clusters) clusters
      mcs    = map mergeCandidate ds
      pairs  = zip [0..] mcs
      pairs' = filter (isJust . snd) pairs
    in
      map (\(x, Just y) -> (x, y)) pairs'

-- |Index of first cluster with 0.0 < disimilarity < minClusterDisimilarity.
mergeCandidate :: [Maybe Double] -> Maybe Int
mergeCandidate =
    L.findIndex (\d -> isJust d &&
      fromJust d > 0.0 && fromJust d < minClusterDisimilarity)

-- |Indexes of Clusters with too small a similarity.
splitCandidates :: Model -> [Cluster] -> [Int]
splitCandidates mdl clusters =
    let sims = map (clusterSim mdl) clusters
    in  [i | i <- [0..(length sims - 1)], let s = sims !! i, isJust s,
          fromJust s < minClusterSimilarity]

-- |Split clusters with too small a similarity into two halves.
splitClusters :: Model -> [Cluster] -> [Cluster]
splitClusters mdl clusters =
    let candidates = splitCandidates mdl clusters
    in  concat [if i `elem` candidates
                  then splitCluster' mdl (clusters !! i)
                  else [clusters !! i]
                  | i <- [0..(length clusters - 1)]]

-- |Split a cluster into two halves.
splitCluster :: [a] -> [[a]]
splitCluster cluster =
    let
      c0 = take (length cluster `div` 2) cluster
      c1 = drop (length c0) cluster
    in
      [c0, c1]

-- |Alternative version of splitCluster that splits of worlds till min.
-- similarity is reached.
splitCluster' :: Model -> Cluster -> [Cluster]
splitCluster' mdl cluster = splitClusterHelper mdl cluster 1

splitClusterHelper :: Model -> Cluster -> Int -> [Cluster]
splitClusterHelper _ [ ] _       = []
splitClusterHelper _ [x] _       = [[x]]
splitClusterHelper mdl cluster n =
    let
      cs = clusterSim mdl (drop n cluster)
    in
      case cs of
        Nothing -> [cluster]
        Just s  -> if s >= minClusterSimilarity  
                     then [take n cluster, drop n cluster]
                     else splitClusterHelper mdl cluster (succ n)

