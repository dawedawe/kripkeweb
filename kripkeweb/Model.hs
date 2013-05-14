module Model
( buildR
, buildRelativeR
, buildLambdaStore
, getAndStoreLambdaRel
, isBoxPhi
, isDiamondPhi
, lambda
, termAsLamType
) where

import Prelude hiding (pi)
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Set as S
import Database.PostgreSQL.Simple
import NLP.Snowball (stem)
import Text.PhoneticCode.Soundex (soundexNARA)

import Conf
import DB
import KripkeTypes
import WebSpider

-- |lambda function of a Kripke Model: Formulas of given world.
lambda :: Connection -> LambdaType -> T.Text -> IO [T.Text]
lambda = worldFormulas

-- |Is phi in the formula set of the given world?
phiInW :: Connection -> LambdaType -> T.Text -> T.Text -> IO Bool
phiInW c lamType phi w = do
    -- deal with optional stemming
    sa <- case lamType of
            Stem -> stemLang c w
            _    -> return T.empty
    let phi' = if sa == T.empty
                 then phi
                 else stem ((myStemAlgo . read . show) sa) phi

    liftM (phi' `elem`) (lambda c lamType w)

-- |Is []phi for the given world?
isBoxPhi :: Connection -> LambdaType -> T.Text -> T.Text -> IO Bool
isBoxPhi c lamType = isQuantorPhi c lamType and

-- |Is <>phi for the given world?
isDiamondPhi :: Connection -> LambdaType -> T.Text -> T.Text -> IO Bool
isDiamondPhi c lamType = isQuantorPhi c lamType or

-- |Generic Quantor.
isQuantorPhi :: Connection -> LambdaType -> ([Bool] -> Bool) -> T.Text ->
                T.Text -> IO Bool
isQuantorPhi c lamType q w phi = do
    aw <- targetsOf c w
    bs <- mapM (phiInW c lamType phi) aw
    return (q bs)

-- |Fetch unstemmed/stemmed formulas of a single url and store them.
getAndStoreLambdaRel :: Connection -> Maybe Proxy -> T.Text -> IO ()
getAndStoreLambdaRel c prx url = do
    (LambdaRels rf sf ef, sa) <- getLambdaRelation prx url
    insertLambdaRelation c Raw rf
    insertLambdaRelation c Stem sf
    insertLambdaRelation c Soundex ef
    insertStemLang c url sa

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
termAsLamType _ Raw   _       t = return t
termAsLamType c Stem (Just w) t = do
    sa <- stemLang c w
    if sa == T.empty
      then return t
      else return (stem ((myStemAlgo . read . show) sa) t)
termAsLamType _ Stem Nothing  t = return t
termAsLamType _ Soundex _     t = return ((T.pack . soundexNARA . T.unpack) t)

