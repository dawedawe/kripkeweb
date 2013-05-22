{-# LANGUAGE OverloadedStrings #-}

module DB
( dbFrame
, documentFrequency
, formulasInLambda
, initPageRankTable
, insertLambdaRelation
, insertAccessRel
, insertStemLang
, lambdaAccum
, myConn
, negateLambaWorlds
, outLinkCount
, outLinkCountAndPageRankofSources
, pathsTo
, pathsToViaLambda
, reflSubFrame
, sourcesInLinks
, sourcesOf
, stemLang
, symSubFrame
, targetsOf
, termFrequency
, transSubFrames
, updatePageRank
, worldCountInLambda
, worldFormulas
, worldsInLambda
, worldsInLinks
, worldsWithFormula
, worldsWithFmlSetIntersectWith
, worldsWithFmlSetSubsetOf
, worldsWithIntersectingLambda
, worldsWithILinksIntersectWith
, worldsWithOLinksIntersectWith
, worldsWithILinksSubsetOf
, worldsWithOLinksSubsetOf
) where

import Control.Monad (liftM, when)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import GHC.Int
import NLP.Snowball

import KripkeTypes
import Util

myConn :: ConnectInfo
myConn = ConnectInfo "localhost" 5432 "saul" "13_kripke_13" "kripkeweb"

-- |Query all distinct sources in the link table.
sourcesInLinks :: Connection -> IO [T.Text]
sourcesInLinks c =
    liftM (map fromOnly) (query_ c "SELECT DISTINCT source FROM links")

-- |Query all distinct worlds (source and target) in the link table.
worldsInLinks :: Connection -> IO [T.Text]
worldsInLinks c =
    let q = "SELECT DISTINCT source FROM links \
            \UNION \
            \SELECT DISTINCT target FROM links"
    in  liftM (map fromOnly) (query_ c q)

-- |Insert OneToN into the link table.
insertAccessRel :: Connection -> OneToN -> IO () 
insertAccessRel c otn = do
    let otolst = oToN2OneToOnes otn
    let otolln = fromIntegral (Prelude.length otolst)
    rs <- executeMany c "INSERT INTO links (source, target) VALUES (?,?)" otolst
    putStrLn ("insertOneToN " ++ show rs)
    when (rs /= otolln) $
      error ("insertAccessRel: inserted only " ++ show rs ++ " out of " ++
        show otolln)

-- |List of targets the given source links to.
targetsOf :: Connection -> T.Text -> IO [T.Text]
targetsOf c s =
    liftM (map fromOnly) (query c "SELECT target FROM links WHERE source = ?"
      (Only s))

-- |List of sources that link to the given target.
sourcesOf :: Connection -> T.Text -> IO [T.Text]
sourcesOf c t =
    liftM (map fromOnly) (query c "SELECT source FROM links WHERE target = ?"
      (Only t))

-- |Relations with the given world as the source.
relsStartingWith :: Connection -> T.Text -> IO [(T.Text, T.Text)]
relsStartingWith c w =
    let q = "SELECT source, target FROM links WHERE source = ?"
    in  query c q (Only w)

-- |Relations staring in one world of the given list.
relsStartingIn :: Connection -> [T.Text] -> IO [(T.Text, T.Text)]
relsStartingIn c ws =
    let q = "SELECT source, target FROM links WHERE source IN ?"
    in  query c q (Only (In ws))

-- |Count of targets of a world.
outLinkCount :: Connection -> T.Text -> IO Int
outLinkCount c s =
    liftM (fromOnly . head) $
      query c "SELECT count(*) FROM links WHERE source = ?" (Only s)

-- |Construct all possible paths leading up to the target, no shorter sub paths.
pathsTo :: Connection -> T.Text -> IO [[T.Text]]
pathsTo c target = liftM (map reverse) (pathsTo' c [target])

-- |Helper for pathsTo.
pathsTo' :: Connection -> [T.Text] -> IO [[T.Text]]
pathsTo' c path = do
    sOf <- sourcesOf c (last path)
    let sOf' = filter (`notElem` path) sOf     -- no recursive links
    if null sOf'
      then return [path]
      else let ps = [path ++ [s] | s <- sOf']
           in  liftM concat (mapM (pathsTo' c) ps)

-- |Construct all possible paths leading up to the target, a link is modelled as
-- a non-empty intersection of the lambda of two worlds. 
pathsToViaLambda :: Connection -> LambdaType -> T.Text -> IO [[T.Text]]
pathsToViaLambda c lamType target =
    liftM (map reverse) (pathsToViaLambda' c lamType [target] [target])

-- |Helper for pathsToViaLambda.
pathsToViaLambda' :: Connection -> LambdaType -> [T.Text] -> [T.Text] ->
                     IO [[T.Text]]
pathsToViaLambda' c lamType visited path = do
    sOf <- worldsWithIntersectingLambda c lamType visited (last path)
    if null sOf
      then return [path]
      else do
             let v' = visited ++ sOf
             let ps = [path ++ [s] | s <- sOf]
             liftM concat (mapM (pathsToViaLambda' c lamType v') ps)

-- |Insert OneToN into on of the lambda tables.
insertLambdaRelation :: Connection -> LambdaType -> OneToNtuples -> IO () 
insertLambdaRelation c lamType otnt = do
    let
      ottlst = oToNtuples2LambdaEntry otnt
      ottlln = fromIntegral (Prelude.length ottlst)
      rawq   = "INSERT INTO lambda         (world, formula, frmcount) \
               \VALUES (?,?,?)"
      stemq  = "INSERT INTO lambda_stems   (world, formula, frmcount) \
               \VALUES (?,?,?)"
      sndexq = "INSERT INTO lambda_soundex (world, formula, frmcount) \
               \VALUES (?,?,?)"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    rs <- executeMany c q ottlst
    putStrLn ("insertLambdaRelation " ++ show lamType ++ " " ++ show rs)
    when (rs /= ottlln) $
      error ("insertLambdaRelation: inserted only " ++ show rs ++
        " out of " ++ show ottlln)

-- |Formulas of the given world.
worldFormulas :: Connection -> LambdaType -> T.Text -> IO [T.Text]
worldFormulas c lamType w =
    let
      rawq   = "SELECT formula FROM lambda         WHERE world = ?"
      stemq  = "SELECT formula FROM lambda_stems   WHERE world = ?"
      sndexq = "SELECT formula FROM lambda_soundex WHERE world = ?"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (map fromOnly) (query c q (Only w))

-- |Count of the given formula in the given world.
termFrequency :: Connection -> LambdaType -> T.Text -> T.Text -> IO Int
termFrequency c lamType w f = do
    let
      rawq   = "SELECT frmcount FROM lambda \
                \WHERE world = ? AND formula = ?"
      stemq  = "SELECT frmcount FROM lambda_stems \
               \WHERE world = ? AND formula = ?"
      sndexq = "SELECT frmcount FROM lambda_soundex \
               \WHERE world = ? AND formula = ?"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    rs <- query c q [w,f]
    if null rs then return 0 else return (fromOnly (head rs))

-- |Count of worlds containing the given formula.
documentFrequency :: Connection -> LambdaType -> T.Text -> IO Int
documentFrequency c lamType f =
    let
      rawq   = "SELECT count(*) FROM lambda         WHERE formula = ?"
      stemq  = "SELECT count(*) FROM lambda_stems   WHERE formula = ?"
      sndexq = "SELECT count(*) FROM lambda_soundex WHERE formula = ?"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (fromOnly . head) $ query c q (Only f)

-- |Count of worlds in the lambda table.
worldCountInLambda :: Connection -> LambdaType -> IO Int
worldCountInLambda c lamType =
    let
      rawq   = "SELECT count(DISTINCT world) FROM lambda"
      stemq  = "SELECT count(DISTINCT world) FROM lambda_stems"
      sndexq = "SELECT count(DISTINCT world) FROM lambda_soundex"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (fromOnly . head) $ query_ c q

-- |Insert the stemming language of a world.
insertStemLang :: Connection -> T.Text -> Maybe Algorithm -> IO ()
insertStemLang _ _ Nothing   = return ()
insertStemLang c w (Just alg) = do
    _ <- execute c "INSERT INTO world_stem_lang (world, language) \
                   \VALUES (?,?)" [w, s]
    return ()
    where s = (T.pack . show . MyStemAlgo) alg

-- |language entry of a world in world_stem_lang.
stemLang :: Connection -> T.Text -> IO T.Text
stemLang c w = do
    rs <- query c "SELECT language FROM world_stem_lang \
                  \WHERE world = ?" (Only w)
    if null rs then return T.empty else return (fromOnly (head rs))

-- |Worlds containing the given formula.
worldsWithFormula :: Connection -> LambdaType -> T.Text -> IO [T.Text]
worldsWithFormula c lamType f =
    let
      rawq   = "SELECT world FROM lambda         WHERE formula = ?"
      stemq  = "SELECT world FROM lambda_stems   WHERE formula = ?"
      sndexq = "SELECT world FROM lambda_soundex WHERE formula = ?"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (map fromOnly) (query c q (Only f))

-- |Worlds whose lambda intersected with the given world's lambda isn't empty.
worldsWithIntersectingLambda :: Connection -> LambdaType -> [T.Text] ->
                                T.Text -> IO [T.Text]
worldsWithIntersectingLambda c lamType v w =
    let
      rawq   = "SELECT DISTINCT world FROM lambda \
               \WHERE world NOT IN ? AND formula IN \
                 \(SELECT formula FROM lambda WHERE world = ?)"
      stemq  = "SELECT DISTINCT world FROM lambda_stems \
               \WHERE world NOT IN ? AND formula IN \
                 \(SELECT formula FROM lambda_stems WHERE world = ?)"
      sndexq = "SELECT DISTINCT world FROM lambda_soundex \
               \WHERE world NOT IN ? AND formula IN \
                 \(SELECT formula FROM lambda_soundex WHERE world = ?)"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (map fromOnly) (query c q (In v, w))

-- |Query all worlds in the lambda or lambda_stems table.
worldsInLambda :: Connection -> LambdaType -> IO [T.Text]
worldsInLambda c lamType =
    let
      rawq   = "SELECT DISTINCT world FROM lambda"
      stemq  = "SELECT DISTINCT world FROM lambda_stems"
      sndexq = "SELECT DISTINCT world FROM lambda_soundex"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in  liftM (map fromOnly) (query_ c q)

-- |Query all formulas in the lambda table.
formulasInLambda :: Connection -> LambdaType -> IO [T.Text]
formulasInLambda c lamType =
    let
      rawq   = "SELECT DISTINCT formula FROM lambda"
      stemq  = "SELECT DISTINCT formula FROM lambda_stems"
      sndexq = "SELECT DISTINCT formula FROM lambda_soundex"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (map fromOnly) (query_ c q)

-- |outlink count and pagerank score of source worlds for a world.
outLinkCountAndPageRankofSources :: Connection -> T.Text ->
                                    IO [(T.Text, Int, Double)]
outLinkCountAndPageRankofSources c w =
    let q = "SELECT source, count(source), score \
            \FROM links LEFT JOIN pagerank ON links.source = pagerank.world \
            \WHERE source <> ? AND source IN \
              \(SELECT source FROM links WHERE target = ?) \
            \GROUP by source, score;"
    in  query c q (w, w)

-- |Delete all entries in the pagerank table and insert all sources from the
-- link table.
initPageRankTable :: Connection -> IO Int64
initPageRankTable c = do
    _ <- execute_ c "DELETE FROM pagerank"
    execute_ c "INSERT INTO pagerank (world) \
               \(SELECT DISTINCT source from links)"

-- |Update score value of a world in the pagerank table.
updatePageRank :: Connection -> T.Text -> Double -> IO Int64
updatePageRank c w s =
    execute c "UPDATE pagerank SET score = ? WHERE world = ?" (s, w)

-- |Worlds whose formula set is a subset of the given formula set.
worldsWithFmlSetSubsetOf :: Connection -> LambdaType -> [T.Text] -> IO [T.Text]
worldsWithFmlSetSubsetOf c lamType l =
    let
      rawq   = "SELECT world FROM lambda         WHERE formula IN ? \
               \EXCEPT \
               \SELECT world FROM lambda         WHERE formula NOT IN ?"
      stemq  = "SELECT world FROM lambda_stems   WHERE formula IN ? \
               \EXCEPT \
               \SELECT world FROM lambda_stems   WHERE formula NOT IN ?"
      sndexq = "SELECT world FROM lambda_soundex WHERE formula IN ? \
               \EXCEPT \
               \SELECT world FROM lambda_soundex WHERE formula NOT IN ?"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (map fromOnly) (query c q [In l, In l])

-- |Worlds whose formula set has a non-empty intersect with the given formula
-- set.
worldsWithFmlSetIntersectWith :: Connection -> LambdaType -> [T.Text] ->
                                 IO [T.Text]
worldsWithFmlSetIntersectWith c lamType l =
    let
      rawq   = "SELECT DISTINCT world FROM lambda         WHERE formula IN ?"
      stemq  = "SELECT DISTINCT world FROM lambda_stems   WHERE formula IN ?"
      sndexq = "SELECT DISTINCT world FROM lambda_soundex WHERE formula IN ?"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (map fromOnly) (query c q (Only (In l)))

-- |Worlds whose target set is a subset of the given target set.
worldsWithOLinksSubsetOf :: Connection -> [T.Text] -> IO [T.Text]
worldsWithOLinksSubsetOf c l =
    let q = "SELECT source FROM links WHERE target IN ? \
            \EXCEPT \
            \SELECT source FROM links WHERE target NOT IN ?"
    in  liftM (map fromOnly) (query c q [In l, In l])

-- |Worlds whose source set is a subset of the given source set.
worldsWithILinksSubsetOf :: Connection -> [T.Text] -> IO [T.Text]
worldsWithILinksSubsetOf c l =
    let q = "SELECT target FROM links WHERE source IN ? \
            \EXCEPT \
            \SELECT target FROM links WHERE source NOT IN ?"
    in  liftM (map fromOnly) (query c q [In l, In l])

-- |Worlds whose target set has a non-empty intersect with the given target
-- set.
worldsWithOLinksIntersectWith :: Connection -> [T.Text] -> IO [T.Text]
worldsWithOLinksIntersectWith c l =
    let q = "SELECT DISTINCT source FROM links WHERE target IN ?"
    in  liftM (map fromOnly) (query c q (Only (In l)))

-- |Worlds whose target set has a non-empty intersect with the given target
-- set.
worldsWithILinksIntersectWith :: Connection -> [T.Text] -> IO [T.Text]
worldsWithILinksIntersectWith c l =
    let q = "SELECT DISTINCT target FROM links WHERE source IN ?"
    in  liftM (map fromOnly) (query c q (Only (In l)))

-- |Negated list of given worlds in lambda.
negateLambaWorlds :: Connection -> LambdaType -> [T.Text] -> IO [T.Text]
negateLambaWorlds c lamType [] = worldsInLambda c lamType
negateLambaWorlds c lamType ws =
    let
      rawq   = "SELECT DISTINCT world FROM lambda         WHERE world NOT IN ?"
      stemq  = "SELECT DISTINCT world FROM lambda_stems   WHERE world NOT IN ?"
      sndexq = "SELECT DISTINCT world FROM lambda_soundex WHERE world NOT IN ?"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      liftM (map fromOnly) (query c q (Only (In ws)))

-- |Accumulations of Lambda formulas with fraction of formula total.
lambdaAccum :: Connection -> LambdaType -> IO [(T.Text, Int, Double)]
lambdaAccum c lamType =
    let
      rawq   = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
               \FROM \
               \(SELECT formula, sum(frmcount) AS fc FROM lambda \
               \GROUP BY formula) AS t1, \
               \(SELECT sum(frmcount) AS total FROM lambda) AS t2 \
               \ORDER BY t1.fc DESC"
      stemq  = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
               \FROM \
               \(SELECT formula, sum(frmcount) AS fc FROM lambda_stems \
               \GROUP BY formula) AS t1, \
               \(SELECT sum(frmcount) AS total FROM lambda_stems) AS t2 \
               \ORDER BY t1.fc DESC"
      sndexq = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
               \FROM \
               \(SELECT formula, sum(frmcount) AS fc FROM lambda_soundex \
               \GROUP BY formula) AS t1, \
               \(SELECT sum(frmcount) AS total FROM lambda_soundex) AS t2 \
               \ORDER BY t1.fc DESC"
      q      = case lamType of
                 Raw     -> rawq
                 Stem    -> stemq
                 Soundex -> sndexq
    in
      query_ c q
 
--------------------------------------------------------------------------------
-- functions for subsets with certain relation properties

dbFrame :: Connection -> IO Frame
dbFrame c = do
    let q = "SELECT source, target FROM links"
    rels <- liftM S.fromList (query_ c q)
    return (Frame (flattenTupleSet rels) rels)

-- |Reflexive subframe of (W, R).
reflSubFrame :: Connection -> IO Frame
reflSubFrame c = do
    let q = "SELECT source, target from links where source = target"
    r <- query_ c q
    return (Frame (S.fromList (flattenTuples r)) (S.fromList r))

symSubFrame :: Connection -> IO Frame
symSubFrame c = do
    let q = "SELECT source, target FROM links \
            \WHERE source <> target \
            \AND (target, source) IN \
              \(select source, target from links)"
    r <- query_ c q
    return (Frame (S.fromList (flattenTuples r)) (S.fromList r))

-- |Transitive subframe of (W, R). Be strict about transitive properties:
-- xRy ^ yRz -> xRz with x, y, z are three different members, min. W size = 3.
transSubFrames :: Connection -> IO (S.Set Frame)
transSubFrames c = do
    ws      <- worldsInLinks c
    tRels <- liftM (filter (/= [])) (mapM (transRelsOf c) ws)
    let tF = [Frame (S.fromList rworlds) (S.fromList r) |
               r <- tRels, let rworlds = flattenTuples r, length rworlds > 2]
    return (S.fromList tF)

-- |Relations that form transitive relations with the given world.
transRelsOf :: Connection -> T.Text -> IO [(T.Text, T.Text)]
transRelsOf c w = do
    -- relations of w without a possible reflexive one
    relsOfw <- liftM (filter (/= (w, w))) (relsStartingWith c w)
    let trgsOfw = map snd relsOfw
    -- relations of targets without backlinks to w and reflexives
    totRels <- liftM (filter (\(x, y) -> y /= w && x /= y))
                 (relsStartingIn c trgsOfw)
    let tOft    = map snd totRels
    if tOft `L.intersect` trgsOfw == tOft   -- w can reach targets of targets
      -- drop possible transitive violations in transitive rels of w
      then return (dropTransViolations (L.nub (relsOfw ++ totRels)))
      else return []

