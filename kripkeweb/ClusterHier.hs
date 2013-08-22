module ClusterHier
( singleLink
, singleLinkHier
) where

import Control.Arrow ((&&&))
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T

import Cluster
import KripkeTypes
import Logic

-- |Hierarchie of clusters constructed by the single link algorithm.
singleLinkHier :: (AsLambdaType f, MTrueIn f) => Model -> [f] ->
                  [[S.Set T.Text]]
singleLinkHier mdl fmls =
    let edges = singleLink mdl fmls
    in  L.nub (map (edgeListToClusters . concat) (tail (L.inits edges)))

-- |Implementation of the single link algorithm, returns a list of lists of
-- edges, that are drawn with rising distances.
singleLink :: (AsLambdaType f, MTrueIn f) => Model -> [f] ->
              [[(T.Text, T.Text)]]
singleLink mdl fmls =
    let
      sortedDs = distancesMap mdl fmls
      ds       = L.nub (map snd sortedDs)
    in
      singleLinkLoop sortedDs ds

-- |Recursive part of the single link algorithm.
singleLinkLoop :: [(S.Set T.Text, Double)] -> [Double] -> [[(T.Text, T.Text)]]
singleLinkLoop _        []     = []
singleLinkLoop sortedDs (d:ds) =
    let
      edges = map (S.toList . fst) (takeWhile (\x -> snd x <= d) sortedDs)
      graph = map (head &&& last) edges
    in
      graph : singleLinkLoop (drop (length graph) sortedDs) ds

-- |Distances between all worlds in the model.
distancesMap :: (AsLambdaType f, MTrueIn f) => Model -> [f] ->
                 [(S.Set T.Text, Double)]
distancesMap mdl@(Model (Frame ws _) _) fmls =
    let
      ps = fmlSpacePoses mdl fmls (S.toList ws)
      ds = L.nub (concatMap (worldsDistances ps) ps)
    in
      L.sortBy (compare `on` snd) ds
      
-- |Distances of a world to all other given worlds, except to its self.
worldsDistances :: [SpacePnt] -> SpacePnt -> [(S.Set T.Text, Double)]
worldsDistances poses (SpacePnt wn wp) =
    let poses' = filter (\p -> name p /= wn) poses
    in  [(S.fromList [wn, name x], boolsDist wp (position x)) | x <- poses']

-- |Construct clusters out of the output of singleLink.
edgeListToClusters :: [(T.Text, T.Text)] -> [S.Set T.Text]
edgeListToClusters es =
    let eSets = [S.fromList [x, y] | (x, y) <- es]
    in  L.nub [connComp s eSets | s <- eSets]


-- |Vertices of the connected component containing startEdge.
connComp :: (Ord a) => S.Set a -> [S.Set a] -> S.Set a
connComp startEdge [] = startEdge
connComp startEdge es =
    let
      dirConn    = filter (\e -> startEdge `S.intersection` e /= S.empty) es
      rest       = es L.\\ dirConn
      startEdge' = startEdge `S.union` S.unions dirConn
    in
      if length dirConn > 0
        then connComp startEdge' rest
        else startEdge

