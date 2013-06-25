{-# LANGUAGE OverloadedStrings #-}

module DB
( dbFrame
, dbModel
, deleteLambdaWorld
, deleteStemLangWorld
, documentFrequency
, formulasInLambda
, indegreeDistribution
, initPageRankTable
, insertLambdaRelation
, insertAccessRel
, insertStemLang
, lambdaAccum
, linkCountAmongWorlds
, myConn
, negateLambaWorlds
, outdegreeDistribution
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
, updateFmlCount
, updatePageRank
, worldCountInLambda
, worldFmlsAndCounts
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
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
      w      = dEntity otnt
      ottlst = oToNtuples2LambdaEntry otnt
      ottlln = fromIntegral (Prelude.length ottlst)
      mRawq   = "INSERT INTO lambda_m        (world, formula, frmcount) \
               \VALUES (?,?,?)"
      mStemq  = "INSERT INTO lambda_mstems   (world, formula, frmcount) \
               \VALUES (?,?,?)"
      mSndexq = "INSERT INTO lambda_msoundex (world, formula, frmcount) \
               \VALUES (?,?,?)"
      bRawq   = "INSERT INTO lambda          (world, formula, frmcount) \
               \VALUES (?,?,?)"
      bStemq  = "INSERT INTO lambda_stems    (world, formula, frmcount) \
               \VALUES (?,?,?)"
      bSndexq = "INSERT INTO lambda_soundex  (world, formula, frmcount) \
               \VALUES (?,?,?)"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    rs <- executeMany c q ottlst
    putStrLn ("insertLambdaRelation " ++ show w ++ " " ++ show lamType ++
      " " ++ show rs)
    when (rs /= ottlln) $
      error ("insertLambdaRelation: inserted only " ++ show rs ++
        " out of " ++ show ottlln)

-- |Update the frmcount field in a lambda Entry
updateFmlCount :: Connection -> LambdaType -> LambdaEntry -> IO ()
updateFmlCount c lamType (LambdaEntry w f cnt) = do
    let
      mRawq   = "UPDATE lambda_m        SET frmcount = ? \
               \WHERE world = ? AND formula = ?"
      mStemq  = "UPDATE lambda_mstems   SET frmcount = ? \
               \WHERE world = ? AND formula = ?"
      mSndexq = "UPDATE lambda_msoundex SET frmcount = ? \
               \WHERE world = ? AND formula = ?"
      bRawq   = "UPDATE lambda          SET frmcount = ? \
               \WHERE world = ? AND formula = ?"
      bStemq  = "UPDATE lambda_stems    SET frmcount = ? \
               \WHERE world = ? AND formula = ?"
      bSndexq = "UPDATE lambda_soundex  SET frmcount = ? \
               \WHERE world = ? AND formula = ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    rs <- execute c q (cnt, w, f)
    putStrLn ("updateFrmCount " ++ show lamType ++ " " ++ show rs)
    when (rs /= 1) $ error ("updateFrmCount: result = " ++ show rs)

-- |Delete all entries of the world in the specified lambda table.
deleteLambdaWorld :: Connection -> LambdaType -> T.Text -> IO ()
deleteLambdaWorld c lamType w = do
    let
      mRawq   = "DELETE FROM lambda_m        WHERE world = ?"
      mStemq  = "DELETE FROM lambda_mstems   WHERE world = ?"
      mSndexq = "DELETE FROM lambda_msoundex WHERE world = ?"
      bRawq   = "DELETE FROM lambda          WHERE world = ?"
      bStemq  = "DELETE FROM lambda_stems    WHERE world = ?"
      bSndexq = "DELETE FROM lambda_soundex  WHERE world = ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    rs <- execute c q (Only w)
    when (rs == 0) $ error ("no entries deleted for " ++ show w)
    putStrLn ("deleteLambdaWorld " ++ show w)

-- |Formulas of the given world.
worldFormulas :: Connection -> LambdaType -> T.Text -> IO [T.Text]
worldFormulas c lamType w =
    let
      mRawq   = "SELECT formula FROM lambda_m        WHERE world = ?"
      mStemq  = "SELECT formula FROM lambda_mstems   WHERE world = ?"
      mSndexq = "SELECT formula FROM lambda_msoundex WHERE world = ?"
      bRawq   = "SELECT formula FROM lambda          WHERE world = ?"
      bStemq  = "SELECT formula FROM lambda_stems    WHERE world = ?"
      bSndexq = "SELECT formula FROM lambda_soundex  WHERE world = ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      liftM (map fromOnly) (query c q (Only w))

-- |The (formula, count) tuples of a world in lambda.
worldFmlsAndCounts :: Connection -> LambdaType -> T.Text -> IO [(T.Text, Int)]
worldFmlsAndCounts c lamType w =
    let
      mRawq   = "SELECT formula, frmcount FROM lambda_m        WHERE world = ?"
      mStemq  = "SELECT formula, frmcount FROM lambda_mstems   WHERE world = ?"
      mSndexq = "SELECT formula, frmcount FROM lambda_msoundex WHERE world = ?"
      bRawq   = "SELECT formula, frmcount FROM lambda          WHERE world = ?"
      bStemq  = "SELECT formula, frmcount FROM lambda_stems    WHERE world = ?"
      bSndexq = "SELECT formula, frmcount FROM lambda_soundex  WHERE world = ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      query c q (Only w)

-- |Count of the given formula in the given world.
termFrequency :: Connection -> LambdaType -> T.Text -> T.Text -> IO Int
termFrequency c lamType w f = do
    let
      mRawq   = "SELECT frmcount FROM lambda_m \
                \WHERE world = ? AND formula = ?"
      mStemq  = "SELECT frmcount FROM lambda_mstems \
                \WHERE world = ? AND formula = ?"
      mSndexq = "SELECT frmcount FROM lambda_msoundex \
                \WHERE world = ? AND formula = ?"
      bRawq   = "SELECT frmcount FROM lambda \
                \WHERE world = ? AND formula = ?"
      bStemq  = "SELECT frmcount FROM lambda_stems \
                \WHERE world = ? AND formula = ?"
      bSndexq = "SELECT frmcount FROM lambda_soundex \
                \WHERE world = ? AND formula = ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    rs <- query c q [w,f]
    if null rs then return 0 else return (fromOnly (head rs))

-- |Count of worlds containing the given formula.
documentFrequency :: Connection -> LambdaType -> T.Text -> IO Int
documentFrequency c lamType f =
    let
      mRawq   = "SELECT count(*) FROM lambda_m        WHERE formula = ?"
      mStemq  = "SELECT count(*) FROM lambda_mstems   WHERE formula = ?"
      mSndexq = "SELECT count(*) FROM lambda_msoundex WHERE formula = ?"
      bRawq   = "SELECT count(*) FROM lambda          WHERE formula = ?"
      bStemq  = "SELECT count(*) FROM lambda_stems    WHERE formula = ?"
      bSndexq = "SELECT count(*) FROM lambda_soundex  WHERE formula = ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      liftM (fromOnly . head) $ query c q (Only f)

-- |Count of worlds in the lambda table.
worldCountInLambda :: Connection -> LambdaType -> IO Int
worldCountInLambda c lamType =
    let
      mRawq   = "SELECT count(DISTINCT world) FROM lambda_m"
      mStemq  = "SELECT count(DISTINCT world) FROM lambda_mstems"
      mSndexq = "SELECT count(DISTINCT world) FROM lambda_msoundex"
      bRawq   = "SELECT count(DISTINCT world) FROM lambda"
      bStemq  = "SELECT count(DISTINCT world) FROM lambda_stems"
      bSndexq = "SELECT count(DISTINCT world) FROM lambda_soundex"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
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

deleteStemLangWorld :: Connection -> T.Text -> IO ()
deleteStemLangWorld c w = do
    rs <- execute c "DELETE FROM world_stem_lang WHERE world = ?" (Only w)
    when (rs /= 1) $ error ("deleteStemLangWorld " ++ show w)
    putStrLn ("deleteStemLangWorld " ++ show w)

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
      mRawq   = "SELECT world FROM lambda_m        WHERE formula = ?"
      mStemq  = "SELECT world FROM lambda_mstems   WHERE formula = ?"
      mSndexq = "SELECT world FROM lambda_msoundex WHERE formula = ?"
      bRawq   = "SELECT world FROM lambda          WHERE formula = ?"
      bStemq  = "SELECT world FROM lambda_stems    WHERE formula = ?"
      bSndexq = "SELECT world FROM lambda_soundex  WHERE formula = ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      liftM (map fromOnly) (query c q (Only f))

-- |Worlds whose lambda intersected with the given world's lambda isn't empty.
worldsWithIntersectingLambda :: Connection -> LambdaType -> [T.Text] ->
                                T.Text -> IO [T.Text]
worldsWithIntersectingLambda c lamType v w =
    let
      mRawq   = "SELECT DISTINCT world FROM lambda_m \
                \WHERE world NOT IN ? AND formula IN \
                  \(SELECT formula FROM lambda_m WHERE world = ?)"
      mStemq  = "SELECT DISTINCT world FROM lambda_mstems \
                \WHERE world NOT IN ? AND formula IN \
                  \(SELECT formula FROM lambda_mstems WHERE world = ?)"
      mSndexq = "SELECT DISTINCT world FROM lambda_msoundex \
                \WHERE world NOT IN ? AND formula IN \
                  \(SELECT formula FROM lambda_msoundex WHERE world = ?)"
      bRawq   = "SELECT DISTINCT world FROM lambda \
                \WHERE world NOT IN ? AND formula IN \
                  \(SELECT formula FROM lambda WHERE world = ?)"
      bStemq  = "SELECT DISTINCT world FROM lambda_stems \
                \WHERE world NOT IN ? AND formula IN \
                  \(SELECT formula FROM lambda_stems WHERE world = ?)"
      bSndexq = "SELECT DISTINCT world FROM lambda_soundex \
                \WHERE world NOT IN ? AND formula IN \
                  \(SELECT formula FROM lambda_soundex WHERE world = ?)"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      liftM (map fromOnly) (query c q (In v, w))

-- |Query all worlds in the lambda or lambda_stems table.
worldsInLambda :: Connection -> LambdaType -> IO [T.Text]
worldsInLambda c lamType =
    let
      mRawq   = "SELECT DISTINCT world FROM lambda_m"
      mStemq  = "SELECT DISTINCT world FROM lambda_mstems"
      mSndexq = "SELECT DISTINCT world FROM lambda_msoundex"
      bRawq   = "SELECT DISTINCT world FROM lambda"
      bStemq  = "SELECT DISTINCT world FROM lambda_stems"
      bSndexq = "SELECT DISTINCT world FROM lambda_soundex"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in  liftM (map fromOnly) (query_ c q)

-- |Query all formulas in the lambda table.
formulasInLambda :: Connection -> LambdaType -> IO [T.Text]
formulasInLambda c lamType =
    let
      mRawq   = "SELECT DISTINCT formula FROM lambda_m"
      mStemq  = "SELECT DISTINCT formula FROM lambda_mstems"
      mSndexq = "SELECT DISTINCT formula FROM lambda_msoundex"
      bRawq   = "SELECT DISTINCT formula FROM lambda"
      bStemq  = "SELECT DISTINCT formula FROM lambda_stems"
      bSndexq = "SELECT DISTINCT formula FROM lambda_soundex"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
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
      mRawq   = "SELECT world FROM lambda_m        WHERE formula IN ? \
                \EXCEPT \
                \SELECT world FROM lambda_m        WHERE formula NOT IN ?"
      mStemq  = "SELECT world FROM lambda_mstems   WHERE formula IN ? \
                \EXCEPT \
                \SELECT world FROM lambda_mstems   WHERE formula NOT IN ?"
      mSndexq = "SELECT world FROM lambda_msoundex WHERE formula IN ? \
                \EXCEPT \
                \SELECT world FROM lambda_msoundex WHERE formula NOT IN ?"
      bRawq   = "SELECT world FROM lambda          WHERE formula IN ? \
                \EXCEPT \
                \SELECT world FROM lambda          WHERE formula NOT IN ?"
      bStemq  = "SELECT world FROM lambda_stems    WHERE formula IN ? \
                \EXCEPT \
                \SELECT world FROM lambda_stems    WHERE formula NOT IN ?"
      bSndexq = "SELECT world FROM lambda_soundex  WHERE formula IN ? \
                \EXCEPT \
                \SELECT world FROM lambda_soundex  WHERE formula NOT IN ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      liftM (map fromOnly) (query c q [In l, In l])

-- |Worlds whose formula set has a non-empty intersect with the given formula
-- set.
worldsWithFmlSetIntersectWith :: Connection -> LambdaType -> [T.Text] ->
                                 IO [T.Text]
worldsWithFmlSetIntersectWith c lamType l =
    let
      mRawq   = "SELECT DISTINCT world FROM lambda_m        WHERE formula IN ?"
      mStemq  = "SELECT DISTINCT world FROM lambda_mstems   WHERE formula IN ?"
      mSndexq = "SELECT DISTINCT world FROM lambda_msoundex WHERE formula IN ?"
      bRawq   = "SELECT DISTINCT world FROM lambda          WHERE formula IN ?"
      bStemq  = "SELECT DISTINCT world FROM lambda_stems    WHERE formula IN ?"
      bSndexq = "SELECT DISTINCT world FROM lambda_soundex  WHERE formula IN ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
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
      mRawq   = "SELECT DISTINCT world FROM lambda_m \
                \WHERE world NOT IN ?"
      mStemq  = "SELECT DISTINCT world FROM lambda_mstems \
                \WHERE world NOT IN ?"
      mSndexq = "SELECT DISTINCT world FROM lambda_msoundex \
                \WHERE world NOT IN ?"
      bRawq   = "SELECT DISTINCT world FROM lambda \
                \WHERE world NOT IN ?"
      bStemq  = "SELECT DISTINCT world FROM lambda_stems \
                \WHERE world NOT IN ?"
      bSndexq = "SELECT DISTINCT world FROM lambda_soundex \
                \WHERE world NOT IN ?"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      liftM (map fromOnly) (query c q (Only (In ws)))

-- |Accumulations of Lambda formulas with fraction of formula total.
lambdaAccum :: Connection -> LambdaType -> IO [(T.Text, Int, Double)]
lambdaAccum c lamType =
    let
      mRawq   = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
                \FROM \
                \(SELECT formula, sum(frmcount) AS fc FROM lambda_m \
                \GROUP BY formula) AS t1, \
                \(SELECT sum(frmcount) AS total FROM lambda_m) AS t2 \
                \ORDER BY t1.fc DESC"
      mStemq  = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
                \FROM \
                \(SELECT formula, sum(frmcount) AS fc FROM lambda_mstems \
                \GROUP BY formula) AS t1, \
                \(SELECT sum(frmcount) AS total FROM lambda_mstems) AS t2 \
                \ORDER BY t1.fc DESC"
      mSndexq = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
                \FROM \
                \(SELECT formula, sum(frmcount) AS fc FROM lambda_msoundex \
                \GROUP BY formula) AS t1, \
                \(SELECT sum(frmcount) AS total FROM lambda_msoundex) AS t2 \
                \ORDER BY t1.fc DESC"
      bRawq   = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
                \FROM \
                \(SELECT formula, sum(frmcount) AS fc FROM lambda \
                \GROUP BY formula) AS t1, \
                \(SELECT sum(frmcount) AS total FROM lambda) AS t2 \
                \ORDER BY t1.fc DESC"
      bStemq  = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
                \FROM \
                \(SELECT formula, sum(frmcount) AS fc FROM lambda_stems \
                \GROUP BY formula) AS t1, \
                \(SELECT sum(frmcount) AS total FROM lambda_stems) AS t2 \
                \ORDER BY t1.fc DESC"
      bSndexq = "SELECT t1.formula, t1.fc, CAST(t1.fc AS REAL) / t2.total \
                \FROM \
                \(SELECT formula, sum(frmcount) AS fc FROM lambda_soundex \
                \GROUP BY formula) AS t1, \
                \(SELECT sum(frmcount) AS total FROM lambda_soundex) AS t2 \
                \ORDER BY t1.fc DESC"
      mp = M.fromList [(MtaRaw,mRawq), (MtaStem,mStemq), (MtaSoundex,mSndexq),
                       (BdyRaw,bRawq), (BdyStem,bStemq), (BdySoundex,bSndexq)]
      q = chooseQuery mp lamType
    in
      query_ c q
 
--------------------------------------------------------------------------------
-- functions for subsets with certain relation properties

-- |Kripke-Model as it is stored in the database.
dbModel :: Connection -> LambdaType -> IO Model
dbModel c lamType = do
    frm <- dbFrame c
    mp  <- lambdaMap c lamType
    return (Model frm (lambdaPure mp))

-- |Kripke-Frame as it is stored in the database.
dbFrame :: Connection -> IO Frame
dbFrame c = do
    let q = "SELECT source, target FROM links"
    rels <- liftM S.fromList (query_ c q)
    return (Frame (flattenTupleSet rels) rels)

-- |Map of worlds and their formulas.
lambdaMap :: Connection -> LambdaType -> IO (M.Map T.Text [T.Text])
lambdaMap c lamType = do
    lamWs <- worldsInLambda c lamType
    wFmls <- mapM (worldFormulas c lamType) lamWs
    return (M.fromList (zip lamWs wFmls))

-- |Reflexive subframe of (W, R).
reflSubFrame :: Connection -> IO Frame
reflSubFrame c = do
    let q = "SELECT source, target from links where source = target"
    r <- query_ c q
    return (Frame (S.fromList (flattenTuples r)) (S.fromList r))

-- |Symmetric subframe of (W, R).
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
 
--------------------------------------------------------------------------------
-- functions for powerlaw analysis

-- |Outdegree distribution as (degree, count) tuples.
outdegreeDistribution :: Connection -> IO [(Int, Int)]
outdegreeDistribution c =
    let q = "SELECT outdegree, count(*) \
            \FROM \
              \(SELECT count(*) AS outdegree \
              \FROM links GROUP BY source) AS tmp \
            \GROUP BY outdegree ORDER BY outdegree"
    in  query_ c q

-- |Indegree distribution as (degree, count) tuples.
indegreeDistribution :: Connection -> IO [(Int, Int)]
indegreeDistribution c =
    let q = "SELECT indegree, count(*) \
            \FROM \
              \(SELECT count(*) AS indegree \
              \FROM links GROUP BY target) AS tmp \
            \GROUP BY indegree ORDER BY indegree"
    in  query_ c q

-- |Count of links between worlds in the given list.
linkCountAmongWorlds :: Connection -> [T.Text] -> IO Int
linkCountAmongWorlds c ws =
    let q = "SELECT count(*) FROM links \
            \WHERE source IN ? AND target IN ?"
    in  liftM (fromOnly . head) $ query c q (In ws, In ws)

linkCountBetweenWorldSets :: Connection -> [T.Text] -> [T.Text] -> IO Int
linkCountBetweenWorldSets c ws1 ws2 =
    let q = "SELECT count(*) FROM links \
            \WHERE source IN ? AND target IN ? \
            \OR source IN ? AND target In ?"
    in  liftM (fromOnly . head) $ query c q (In ws1, In ws2, In ws2, In ws1)

chooseQuery :: M.Map LambdaType Query -> LambdaType -> Query
chooseQuery mp lamType =
    let v = M.lookup lamType mp
    in  fromMaybe (error "no query for LambdaType in Map") v
