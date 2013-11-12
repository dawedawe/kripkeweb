module PageRank
( calcAndUpdatePageRank
, calcAndUpdatePageRanks
, pageRank
) where

import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB

-- |Initialize the pagerank table, calculate and update the scores for i
-- iterations.
calcAndUpdatePageRanks :: Connection -> Int -> IO ()
calcAndUpdatePageRanks c i
    | i < 1     = return ()
    | otherwise = do
        _  <- initPageRankTable c
        ws <- sourcesInLinks c
        _  <- calcAndUpdatePageRanks' c i ws
        close c

-- |Recursive Helper for calcAndUpdatePageRanks.
calcAndUpdatePageRanks' :: Connection -> Int -> [T.Text] -> IO ()
calcAndUpdatePageRanks' c i ws
    | i < 1     = return ()
    | otherwise = do
        _ <- mapM_ (calcAndUpdatePageRank c) ws
        calcAndUpdatePageRanks' c (pred i) ws

-- |Calculate and update the PageRank Score of a single world.
calcAndUpdatePageRank :: Connection -> T.Text -> IO ()
calcAndUpdatePageRank c w = do
    s <- pageRank c w
    _ <- updatePageRank c w s
    return ()

-- |PageRank score for a single world.
pageRank :: Connection -> T.Text -> IO Double
pageRank c w = do
    csnrs <- outLinkCountAndPageRankofSources c w
    return (pageRankPure [(prs, lc) | (_, lc, prs) <- csnrs])

-- |Pure helper of pageRank.
pageRankPure :: [(Double, Int)] -> Double
pageRankPure rnkslnks =
    let qs = [rs / fromIntegral lc | (rs, lc) <- rnkslnks]
    in  0.15 + 0.85 * sum qs

