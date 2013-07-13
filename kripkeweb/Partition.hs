module Partition
( hubsAndAuthorities
, initAHpages
) where

import Data.Function (on)
import qualified Data.List as L ((\\), sortBy)
import qualified Data.Set  as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import qualified Cluster as C
import DB
import FormulaSchemes
import KripkeTypes
import Relation

data AHpage = AHpage { name :: T.Text
                     , auth :: Double
                     , hub  :: Double
                     } deriving (Eq, Show)

approxHnA :: Connection -> LambdaType -> IO ([T.Text], [T.Text])
approxHnA c lamType = do
    mdl  <- dbModel c lamType
    fmls <- tfidfsTopXOredDiamonded c lamType 9
    let poss  = C.fmlSpacePoses mdl fmls (S.toList (wSet (frame mdl)))
    let auths = 
          filter (\(C.SpacePnt _ p) ->
            length (filter (== False) p) > (length p `div` 2)) poss
    let hubs = poss L.\\ auths
    return (map C.name auths, map C.name hubs)

divideHubsAndAuthorities :: Frame -> ([AHpage], [AHpage])
divideHubsAndAuthorities frm@(Frame w _) =
    let
      hubsnAuths = hubsAndAuthorities frm 5 (initAHpages (S.toList w))
      auths      = filter (\x -> auth x > hub x) hubsnAuths
      hubs       = hubsnAuths L.\\ auths
      auths'     = L.sortBy (compare `on` auth) auths
      hubs'      = L.sortBy (compare `on` hub) hubs
    in
      (auths', hubs')

initAHpages :: [T.Text] -> [AHpage]
initAHpages names = [AHpage n 1.0 1.0 | n <- names]

hubsAndAuthorities :: Frame -> Int -> [AHpage] -> [AHpage]
hubsAndAuthorities frm k pages
    | k <  0    = error "hubsAndAuthorities: negative k"
    | k == 0    = pages
    | otherwise =
        let
          pages1 = map (updateAuthValue frm pages) pages
          norm1  = updateNorm pages1 auth
          pages2 = [p { auth = auth p / norm1 } | p <- pages1]  -- 14, 15
          pages3 = map (updateHubValue frm pages2) pages2       -- 17 - 20
          norm2  = updateNorm pages3 hub
          pages4 = [p { hub = hub p / norm2 } | p <- pages3]    -- 23, 24
        in
          hubsAndAuthorities frm (pred k) pages4

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
