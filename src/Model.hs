module Model
( buildR
, buildRelativeR
, buildLambdaStore
, getAndStoreLambdaRel
, lambda
, termAsLamType
) where

import Control.Exception
import Control.Monad (when)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import NLP.Snowball (stem)
import Text.PhoneticCode.Soundex (soundexNARA)

import Conf
import DB
import KripkeTypes
import WebSpider

-- |lambda function of a Kripke Model stored in the db: Formulas of given world.
lambda :: Connection -> LambdaType -> T.Text -> IO [T.Text]
lambda = worldFormulas

-- |Fetch meta/body - raw/stemmed/sonudexed formulas via the given function of
-- a single url and store them.
getAndStoreLambdaRel :: Connection -> Maybe Proxy -> T.Text -> IO ()
getAndStoreLambdaRel c prx url = do
    (LambdaRels mrf msf mef brf bsf bef, sa) <- getLambdaRels prx url
    handle (sqlErrorHandler url) (insertLambdaRelation c MtaRaw mrf)
    handle (sqlErrorHandler url) (insertLambdaRelation c MtaStem msf)
    handle (sqlErrorHandler url) (insertLambdaRelation c MtaSoundex mef)
    handle (sqlErrorHandler url) (insertLambdaRelation c BdyRaw brf)
    handle (sqlErrorHandler url) (insertLambdaRelation c BdyStem bsf)
    handle (sqlErrorHandler url) (insertLambdaRelation c BdySoundex bef)
    when (nTuples bsf /= S.empty || nTuples msf /= S.empty) $
      insertStemLang c url sa

-- |Error handler for the insertions of getAndStoreLambdaRel.
sqlErrorHandler :: T.Text -> SqlError -> IO ()
sqlErrorHandler url e = do
    putStrLn ("insertLambdaRelation failed for " ++ T.unpack url)
    print e

-- |Apply getAndStoreLambdaRel to all sources in links.
buildLambdaStore :: Connection -> Maybe Proxy -> IO ()
buildLambdaStore c prx = do
    worlds <- worldsInLinks c
    mapM_ (getAndStoreLambdaRel c prx) worlds
    close c

-- |Build and store the Accessability Relation starting at an url with a given
-- depth.
buildR :: Connection -> Maybe Proxy -> T.Text -> Int -> IO ()
buildR c prx url depth = do
    _ <- spiderHO prx depth (insertAccessRel c) S.empty url
    return ()

-- |Build and store a relative Accessability Relation starting at an url with a
-- given depth.
buildRelativeR :: Connection -> Maybe Proxy -> T.Text -> Int -> IO ()
buildRelativeR c prx url depth = do
    _ <- spiderRelHO prx depth (insertAccessRel c) S.empty url
    return ()

-- |Postprocess a term according to a LambdaType and to a Maybe world
termAsLamType :: Connection -> LambdaType -> Maybe T.Text -> T.Text -> IO T.Text
termAsLamType _ MtaRaw   _       t = return t
termAsLamType _ BdyRaw   _       t = return t
termAsLamType c MtaStem (Just w) t = do
    sa <- stemLang c w
    if sa == T.empty
      then return t
      else return (stem ((myStemAlgo . read . show) sa) t)
termAsLamType c BdyStem (Just w) t = do
    sa <- stemLang c w
    if sa == T.empty
      then return t
      else return (stem ((myStemAlgo . read . show) sa) t)
termAsLamType _ MtaStem Nothing  t = return t
termAsLamType _ BdyStem Nothing  t = return t
termAsLamType _ MtaSoundex _     t = return ((T.pack . soundexNARA . T.unpack) t)
termAsLamType _ BdySoundex _     t = return ((T.pack . soundexNARA . T.unpack) t)

