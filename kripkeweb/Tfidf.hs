module Tfidf
( allTfidf
, allTfidfDB
, allTopTfidf
, storeAllTfidf
, tfidf
, tfidfSortedSearch
, worldsTopTfidf
, worldsTopXPercentTfidf
, worldsTfidf
) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import Model
import Util

-- |Compute the tf-idf for a term in a world.
tfidf :: Connection -> LambdaType -> T.Text -> T.Text -> IO Double
tfidf c lamType w t = do
    t' <- termAsLamType c lamType (Just w) t
    tf <- termFrequency c lamType w t'
    df <- documentFrequency c lamType t'
    n  <- worldCountInLambda c lamType
    return (tfidf' tf df n)

-- |Pure helper for tfidf.
tfidf' :: Int -> Int -> Int -> Double
tfidf' _  0  _ = 0
tfidf' tf df n = let idf = logBase 10 (fromIntegral n / fromIntegral df)
                 in  fromIntegral tf * idf

-- |Compute all tf-idf scores of a world and sort them.
worldsTfidf :: Connection -> LambdaType -> T.Text -> IO [(T.Text, Double)]
worldsTfidf c lamType w = do
    n     <- worldCountInLambda c lamType
    terms <- lambda c lamType w
    freqs <- mapM (helper n) terms
    let tefrs = zip terms freqs
    return $ sortBy (compare `on` snd) tefrs
    where
      helper :: Int -> T.Text -> IO Double
      helper n t = do
          tf <- termFrequency c lamType w t
          df <- documentFrequency c lamType t
          return (tfidf' tf df n)

-- |Top n tf-idf scores of a world.
worldsTopTfidf :: Connection -> LambdaType -> Int -> T.Text ->
                  IO [(T.Text, Double)]
worldsTopTfidf c lamType n w =
    liftM (take n . reverse) (worldsTfidf c lamType w)

-- |Top x percent of the tf-idf scores of a world.
worldsTopXPercentTfidf :: Connection -> LambdaType -> Int -> T.Text ->
                          IO [T.Text]
worldsTopXPercentTfidf c lamType prcnt w = do
    ts <- worldsTfidfDB c lamType w
    return (map fst (keepFirstXPercent prcnt ts))

-- |tf-idf scores of all worlds sorted descending, precompute as much as
-- possible.
allTfidf :: Connection -> LambdaType -> IO [(T.Text, [(T.Text, Double)])]
allTfidf c lamType = do
    dfs <- documentFrequencyMap c lamType
    ws  <- worldsInLambda c lamType
    let n = length ws 
    ts  <- mapM (worldsTfidfPre c lamType n dfs) ws
    return (zip ws ts)

-- |tf-idf scores of all worlds sorted descending, use precomputed DB values.
allTfidfDB :: Connection -> LambdaType -> IO [(T.Text, [(T.Text, Double)])]
allTfidfDB c lamType = do
    ws <- worldsInLambda c lamType
    ts <- mapM (worldsTfidfDB c lamType) ws
    return (zip ws ts)

-- |Top n tf-idf scores of all worlds, precompute as much as possible.
allTopTfidf :: Connection -> LambdaType -> Int ->
               IO [(T.Text, [(T.Text, Double)])]
allTopTfidf c lamType topN = do
    rs <- allTfidf c lamType
    return $ map (second (take topN)) rs

-- |Top n tf-idfs of a world.
-- With precomputed number of worlds and document frequencies.
worldsTopTfidfPre :: Connection -> LambdaType -> Int -> M.Map T.Text Int ->
                     Int -> T.Text -> IO [(T.Text, Double)]
worldsTopTfidfPre c lamType n dfs topN w =
    liftM (take topN) (worldsTfidfPre c lamType n dfs w)

-- |Compute all tf-idfs for the given world and sort them descending.
-- With precomputed number of worlds and document frequencies.
worldsTfidfPre :: Connection -> LambdaType -> Int -> M.Map T.Text Int ->
                  T.Text -> IO [(T.Text, Double)]
worldsTfidfPre c lamType n dfs w = do
    terms     <- lambda c lamType w
    freqs     <- mapM (tfidfPre c lamType n dfs w) terms
    let tefrs = zip terms freqs
    return $ reverse (sortBy (compare `on` snd) tefrs)

-- |Calculate all Document Frequencies.
documentFrequencyMap :: Connection -> LambdaType -> IO (M.Map T.Text Int)
documentFrequencyMap c lamType = do
    frms  <- formulasInLambda c lamType
    freqs <- mapM (documentFrequency c lamType) frms
    return (M.fromList (zip frms freqs))

-- |Compute the tf-idf for a term in a world.
-- With precomputed number of worlds and document frequencies.
tfidfPre :: Connection -> LambdaType -> Int -> M.Map T.Text Int -> T.Text ->
            T.Text -> IO Double
tfidfPre c lamType n dfs w t = do
    tf     <- termFrequency c lamType w t
    let df = fromJust (M.lookup t dfs)
    return (tfidf' tf df n)

-- |Calculate the tfidf for a term in all worlds containing it. Sort the result
-- descending.
tfidfSortedSearch :: Connection -> LambdaType -> T.Text -> IO [(T.Text, Double)]
tfidfSortedSearch c lamType t = do
    ws <- worldsWithFormula c lamType t
    ts <- mapM (flip (tfidf c lamType) t) ws
    let res = zip ws ts
    return (reverse (sortBy (compare `on` snd) res))

--------------------------------------------------------------------------------
-- functions for storing precomputed tfidf score in the database

-- |Precompute all tfidf scores and store them in the database.
storeAllTfidf :: Connection -> LambdaType -> IO ()
storeAllTfidf c lamType = do
    allTs <- allTfidf c lamType
    mapM_ (storeWorldsTfidf c lamType) allTs

-- |Store the given tfidf scores of a world's formulas in the database.
storeWorldsTfidf :: Connection -> LambdaType -> (T.Text, [(T.Text, Double)]) ->
                    IO ()
storeWorldsTfidf c lamType (w, scores) = mapM_ (updateTfidf c lamType w) scores

