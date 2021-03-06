module Partition
( approxHnA
, divideHubsAndAuthorities
, hubsAndAuthorities
, initAHpages
) where

import Control.Monad (liftM)
import Data.Function (on)
import qualified Data.List as L ((\\), nub, sortBy)
import qualified Data.Set  as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import qualified Cluster as C
import DB
import FormulaSchemes
import KripkeTypes
import Logic
import Measure
import Relation

--------------------------------------------------------------------------------
-- Functions for hubs and authorities

-- Type to help with the divideHubsAndAuthorities algorithm.
data AHpage = AHpage { name :: T.Text
                     , auth :: Double
                     , hub  :: Double
                     } deriving (Eq, Show)

-- Approximate the hubs and authorities partitioning with
-- tfidfsTopXOredDiamonded.
approxHnA :: Connection -> LambdaType -> Int -> IO ([T.Text], [T.Text])
approxHnA c lamType prcnt = do
    mdl  <- dbModel c lamType
    fmls <- tfidfsTopXOredDiamonded c lamType prcnt
    let poss  = C.fmlSpacePoses mdl fmls (S.toList (wSet (frame mdl)))
    let auths = 
          filter (\(C.SpacePnt _ p) ->
            length (filter (== False) p) > (length p `div` 2)) poss
    let hubs = poss L.\\ auths
    return (map C.name auths, map C.name hubs)

-- Implementation of the kleinberg algorithm for hubs and authorities.
divideHubsAndAuthorities :: Frame -> ([AHpage], [AHpage])
divideHubsAndAuthorities frm@(Frame w _) =
    let
      hubsnAuths = hubsAndAuthorities frm 20 (initAHpages (S.toList w))
      auths      = filter (\x -> auth x > hub x) hubsnAuths
      hubs       = hubsnAuths L.\\ auths
      auths'     = L.sortBy (compare `on` auth) auths
      hubs'      = L.sortBy (compare `on` hub) hubs
    in
      (auths', hubs')

-- |Convert to a list of AHpage and initialize scores with 1.0.
initAHpages :: [T.Text] -> [AHpage]
initAHpages names = [AHpage n 1.0 1.0 | n <- names]

-- |Iterating code of the kleinberg algorithm, gives every AHpage a hub- and an
-- authoritiy-score.
hubsAndAuthorities :: Frame -> Int -> [AHpage] -> [AHpage]
hubsAndAuthorities frm k pages
    | k <  0    = error "hubsAndAuthorities: negative k"
    | k == 0    = pages
    | otherwise =
        let
          pages1 = map (updateAuthValue frm pages) pages
          norm1  = updateNorm pages1 auth
          pages2 = [p { auth = auth p / norm1 } | p <- pages1]
          pages3 = map (updateHubValue frm pages2) pages2
          norm2  = updateNorm pages3 hub
          pages4 = [p { hub = hub p / norm2 } | p <- pages3]
        in
          hubsAndAuthorities frm (pred k) pages4

-- |Update the normalization factor.
updateNorm :: [AHpage] -> (AHpage -> Double) -> Double
updateNorm pages f = sqrt $ sum [sqrt (f p) | p <- pages]

-- |Update the auth score of the given AHpage with the sum of the hub scores of
-- the inlinking AHpages.
updateAuthValue :: Frame -> [AHpage] -> AHpage -> AHpage
updateAuthValue (Frame _ rels) bigG p =
    let
      sourceWs = unreflSourcesOf (S.toList rels) (name p)
      hubSum   = sum $ map hub (filter (\x -> name x `elem` sourceWs) bigG)
    in
      p { auth = hubSum }
      
-- |Update the hub score of the given AHpage with the sum of the auth scores of
-- the targeted AHpages.
updateHubValue :: Frame -> [AHpage] -> AHpage -> AHpage
updateHubValue (Frame _ rels) bigG p =
    let
      outLnkWs = unreflTargetsOf (S.toList rels) (name p)
      authSum  = sum $ map auth (filter (\x -> name x `elem` outLnkWs) bigG)
    in
      p { hub = authSum }

--------------------------------------------------------------------------------

-- |Partitioning between worlds belonging to some community and loners.
communityAndLoners :: Connection -> LambdaType -> Int -> IO ([T.Text], [T.Text])
communityAndLoners c lamType prcnt = do
    fmls      <- atLeastOneSimilarAccWorld c lamType prcnt
    mdl       <- dbModel c lamType
    let comWs =  (L.nub . concat) (map (satMWorlds (toUnreflModel mdl)) fmls)
    let ws    =  S.toList (wSet (frame mdl))
    return (comWs, ws L.\\ comWs)

--------------------------------------------------------------------------------

-- |Partitioning between dead ends and non dead ends.
deadEndsAndNonDeadEnds :: Connection -> LambdaType -> IO ([T.Text], [T.Text])
deadEndsAndNonDeadEnds c lamType = do
    mdl       <- dbModel c lamType
    let mdl'  = toUnreflModel mdl
    let dEnds = satMWorlds mdl' deadEndScheme
    let nEnds = S.toList (wSet (frame mdl')) L.\\ dEnds
    return (dEnds, nEnds)

--------------------------------------------------------------------------------
-- functions for partitioning stats

-- |1 - interrelation, the higher the less the two partitions stick together.
selectivity :: Connection -> [T.Text] -> [T.Text] -> IO Double
selectivity c aPart bPart = do
    z <- interrelation c aPart bPart
    return (1 - z)

-- |Link count among partitions in proportion to the maximum link count to
-- measure how much two partitions stick together.
interrelation :: Connection -> [T.Text] -> [T.Text] -> IO Double
interrelation c aPart bPart = do
    lnkcnt <- linkCountBetweenWorldSets c aPart bPart
    let me =  maxEdgesBetweenBinPartition aPart bPart
    return (fromIntegral lnkcnt / fromIntegral me)

-- |Count of max possible edges between two distinct sets.
maxEdgesBetweenBinPartition :: [a] -> [a] -> Int
maxEdgesBetweenBinPartition aPart bPart = 2 * length aPart * length bPart

-- |Average link count per world in a partition, not counting reflexive ones.
avgLinksPerWorldInPartition :: Connection -> [T.Text] -> IO Double
avgLinksPerWorldInPartition c part = do
    plinks <- unreflLinkCountAmongWorlds c part
    return (fromIntegral plinks / fromIntegral (length part))

-- |Links in a partition / edge count of a digraph clique.
partCliqueness :: Connection -> [T.Text] -> IO (Maybe Double)
partCliqueness _ []  = return Nothing
partCliqueness _ [_] = return (Just 1.0)
partCliqueness c ws  = do
    let es =  edgesInDigraphClique (length ws)
    lc     <- unreflLinkCountAmongWorlds c ws
    return (Just (fromIntegral lc / fromIntegral es))

