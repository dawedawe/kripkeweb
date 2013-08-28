module Cluster
( Cluster
, SpacePnt (..)
, boolsDist
, clusterDisims'
, clusterSim
, disimilarity
, fmlSpacePos
, fmlSpacePoses
, printClusterStats
, printClusters
, similarity
) where

import qualified Data.List as L
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T

import KripkeTypes
import Logic
import Measure
import Relation

-- |Type for a world in the representation space.
data SpacePnt = SpacePnt { name     :: T.Text
                         , position :: [Bool]
                         } deriving (Eq)

instance Show SpacePnt where
    show (SpacePnt n p) = show (n, show p)

type Cluster = [SpacePnt]

-- |fmlSpacePos vectors of all given worlds.
fmlSpacePoses :: (AsLambdaType f, MTrueIn f) => Model -> [f] -> [T.Text] ->
                 [SpacePnt]
fmlSpacePoses mdl fmls = map (fmlSpacePos mdl fmls)

-- |Position of a world with regard to a list of formulas spanning a space.
-- Expects the formulas to be already in the right LambdaType.
fmlSpacePos :: (MTrueIn f) => Model -> [f] -> T.Text -> SpacePnt
fmlSpacePos mdl fmls w = SpacePnt w (map (isMTrueInWorld mdl w) fmls)

-- |Distance of two Bool Lists.
boolsDist :: [Bool] -> [Bool] -> Double
boolsDist x y
    | length x == length y = sqrt (fromIntegral (sum (zipWith boolDist x y)))
    | otherwise            =
        error ("boolsDist: uneven lists: " ++ show x ++ " " ++ show y)

-- |Distance of two Bools.
boolDist :: Bool -> Bool -> Int
boolDist x y
    | x == y    = 0
    | otherwise = 1

--------------------------------------------------------------------------------
-- functions for similarity/disimilarity measures

-- |Average similarity of pairs in a cluster.
clusterSim :: Model -> Cluster -> Maybe Double
clusterSim (Model _ lam) cluster
    | length cluster < 2 = Nothing
    | otherwise          =
        let
          wlams = map (S.fromList . lam . name) cluster
          sims  = concat [map (similarity l) (L.delete l wlams) | l <- wlams]
        in
          Just (sum sims / fromIntegral (length sims))

-- |Disimilarity between the formula sets of two clusters.
clusterDisim :: Model -> Cluster -> Cluster -> Maybe Double
clusterDisim (Model _ lam) c1 c2
    | null c1 || null c2 = Nothing
    | otherwise          =
        let
          c1ls = concatMap (lam . name) c1
          c2ls = concatMap (lam . name) c2
        in
          Just (disimilarity (S.fromList c1ls) (S.fromList c2ls))


-- |Disimilarity scores of one cluster to a list of other clusters.
clusterDisims :: Model -> [Cluster] -> Cluster -> [Maybe Double]
clusterDisims mdl clusters cl = map (clusterDisim mdl cl) (L.delete cl clusters)

-- |Disimilarity scores of one cluster to a list of clusters, maybe including it
-- self.
clusterDisims' :: Model -> [Cluster] -> Cluster -> [Maybe Double]
clusterDisims' mdl clusters cl = map (clusterDisim mdl cl) clusters

-- |Average similarity of a list of clusters.
avgClusterSim :: Model -> [Cluster] -> Double
avgClusterSim mdl clusters
    | null clusters = error "avgClusterSim: no clusters given"
    | otherwise     =
        let ss  = mapMaybe (clusterSim mdl) clusters
        in  sum ss / fromIntegral (length ss)

-- |Average disimilarity among a list of clusters.
avgClusterDisim :: Model -> [Cluster] -> Double
avgClusterDisim mdl clusters
    | length (filter (not . null) clusters) < 2 =
        error "avgClusterDisim: < 2 unempty clusters given"
    | otherwise                                 =
        let ds  = concatMap (clusterDisims mdl clusters) clusters
        in  sum (catMaybes ds) / fromIntegral (length ds)

--------------------------------------------------------------------------------
-- functions for displaying cluster information

-- |Data type to hold some stats about a cluster.
data ClusterStats = ClusterStats
                  { clusterSize    :: Int
                  , clusterStatSim :: Maybe Double
                  , cliquenessStat :: Maybe Double
                  }

instance Show ClusterStats where
    show cs =
      "size = " ++ show (clusterSize cs) ++
      " similarity = " ++ show (clusterStatSim cs) ++
      " cliqueness = " ++ show (cliquenessStat cs)

-- |Print the stats of all given clusers and the avgClusterSim, avgClusterDisim
printClusterStats :: Model -> [Cluster] -> IO ()
printClusterStats mdl clusters = do
    let ss = map (genClusterStats mdl) clusters
    mapM_ print ss
    putStrLn ("avgClusterSim = " ++ show (avgClusterSim mdl clusters))
    putStrLn ("avgClusterDisim = " ++ show (avgClusterDisim mdl clusters))
    putStrLn ("avgClusterCliqueness = " ++
      show (avgClusterCliqueness (frame mdl) clusters))

-- |Generate a ClusterStats for the given Cluster.
genClusterStats :: Model -> Cluster -> ClusterStats
genClusterStats mdl@(Model frm _) cluster =
    let
      sze = length cluster
      sim = clusterSim mdl cluster
      clq = clusterCliqueness frm cluster
    in
      ClusterStats sze sim clq

-- |Print the given clusers, seperated by an empty newline.
printClusters :: [Cluster] -> IO ()
printClusters = mapM_ (\cls -> printCluster cls >> putStrLn "")

-- |Print the names of a cluser, one per line.
printCluster :: Cluster -> IO ()
printCluster cluster = do
    let ns = map name cluster
    mapM_ print ns

--------------------------------------------------------------------------------
-- functions for graph related measures

-- |Links in a cluster / edge count of a digraph clique.
clusterCliqueness :: Frame -> Cluster -> Maybe Double
clusterCliqueness _           []      = Nothing
clusterCliqueness _           [_]     = Just 1.0
clusterCliqueness (Frame _ r) cluster =
    let
      ws = map name cluster
      es = edgesInDigraphClique (length ws)
      lc = unreflRelCountAmongWorlds (S.toList r) ws
    in
      Just (fromIntegral lc / fromIntegral es)

-- |Average cliqueness in a list of clusters.
avgClusterCliqueness :: Frame -> [Cluster] -> Double
avgClusterCliqueness frm clusters =
        let cls = mapMaybe (clusterCliqueness frm) clusters
        in  sum cls / fromIntegral (length cls)

