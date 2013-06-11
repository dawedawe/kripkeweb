{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple (close, connect)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Conf
import DB
import KripkeTypes
import LinkRing
import LogicSearch
import Model
import PageRank
import RoughSet
import Tfidf
import WebSpider

main :: IO ()
main = do
    parsedArgv <- getArgs >>= parseArgv
    let parsedOptions = fst parsedArgv
    createDotDir
    conf <- buildConf parsedOptions

    when (optFlags (opts conf) == S.empty) $ do
      putStrLn usage
      exitFailure

    c <- connect (conInfo conf)

    let lamType = optLamType (opts conf)

    when (BuildR `S.member` optFlags (opts conf)) $
      buildR c (proxy conf) (optUrl (opts conf)) (optRecDepth (opts conf))

    when (BuildRelativeR `S.member` optFlags (opts conf)) $
      buildRelativeR
        c (proxy conf) (optUrl (opts conf)) (optRecDepth (opts conf))

    when (BuildLambdaRel `S.member` optFlags (opts conf)) $
      buildLambdaStore c (getLambdaRelation (proxy conf))

    when (BuildMLambdaRel `S.member` optFlags (opts conf)) $
      buildLambdaStore c (getMetaLambdaRel (proxy conf))

    when (CalcPageRank `S.member` optFlags (opts conf)) $
      calcAndUpdatePageRanks c (optPgIters (opts conf))

    when (LambdaAccum `S.member` optFlags (opts conf)) $ do
      rs <- lambdaAccum c lamType
      mapM_ print rs

    when (TfidfWorld `S.member` optFlags (opts conf)) $ do
      ts <- worldsTfidf c lamType (optUrl (opts conf))
      mapM_ print ts

    when (TfidfSearch `S.member` optFlags (opts conf)) $ do
      rs <- tfidfSortedSearch c lamType (optFml (opts conf))
      mapM_ print rs

    when (PLFmlEval `S.member` optFlags (opts conf)) $ do
      let frm = read (T.unpack (optFml (opts conf))) :: Fml
      putStrLn ("plformula: " ++ show frm ++ " =")
      ws <- satWorlds c lamType frm
      mapM_ print ws

    when (MLFmlEval `S.member` optFlags (opts conf)) $ do
      let frm = read (T.unpack (optFml (opts conf))) :: MLFml
      putStrLn ("mlformula: " ++ show frm ++ " =")
      ws <- satWorlds c lamType frm
      mapM_ print ws

    when (InLinkRings `S.member` optFlags (opts conf)) $ do
      rs <- inLinkRings c (optUrl (opts conf))
      let fr = formatRingsAsCols rs
      mapM_ print fr

    when (OutLinkRings `S.member` optFlags (opts conf)) $ do
      rs <- outLinkRings c (optUrl (opts conf))
      let fr = formatRingsAsCols rs
      mapM_ print fr

    when (RSetLamdaPL `S.member` optFlags (opts conf)) $ do
      let frm = read (T.unpack (optFml (opts conf))) :: Fml
      putStrLn ("formula: " ++ show frm ++ " =")
      rs <- roughSetOfLamPL c lamType frm
      printRoughSet rs

    when (RSetLamdaList `S.member` optFlags (opts conf)) $ do
      wl <- mapM (termAsLamType c lamType Nothing) (optWords (opts conf))
      rs <- roughSetOfLamList c lamType wl
      printRoughSet rs

    when (RSetWorldsLamda `S.member` optFlags (opts conf)) $ do
      rs <- roughSetOfWorldsLam c lamType (optUrl (opts conf))
      printRoughSet rs

    when (RSetInLinks `S.member` optFlags (opts conf)) $ do
      rs <- roughSetOfInLinks c (optWords (opts conf))
      printRoughSet rs

    when (RSetWsInLinks `S.member` optFlags (opts conf)) $ do
      rs <- roughSetOfWorldsInLinks c (optUrl (opts conf))
      printRoughSet rs

    when (RSetOutLinks `S.member` optFlags (opts conf)) $ do
      rs <- roughSetOfOutLinks c (optWords (opts conf))
      printRoughSet rs

    when (RSetWsOutLinks `S.member` optFlags (opts conf)) $ do
      rs <- roughSetOfWorldsOutLinks c (optUrl (opts conf))
      printRoughSet rs

    close c

testQuantors :: Conf -> IO ()
testQuantors conf = do
    c <- connect (conInfo conf)
    let qWorlds = ["http://www.heise.de", "http://www.apple.com"] :: [T.Text]
    let p cn _ w = stemLang cn w >>= \l -> return (l == "Dutch")

    x <- eval2B c Raw (All qWorlds p)
    print x
    y <- eval2B c Raw (Ex qWorlds p)
    print y

testAllTopTfidf :: [String] -> IO ()
testAllTopTfidf args = do
    let n = read (head args)
    c <- connect myConn
    tops <- allTopTfidf c Raw n
    mapM_ print tops

testLambda :: [String] -> IO ()
testLambda args = do
    let url = head args
    c    <- connect myConn
    frms <- lambda c Raw (T.pack url)
    mapM_ print frms

testGetAndInsertLambdaRelation :: [String] -> IO ()
testGetAndInsertLambdaRelation args = do
    let url = T.pack (head args)
    c            <- connect myConn
    cnf          <- buildConf defaultOptions
    (LambdaRels rf sf ef, sa) <- getLambdaRelation (proxy cnf) url
    insertLambdaRelation c Raw rf
    insertLambdaRelation c Stem sf
    insertLambdaRelation c Soundex ef
    insertStemLang c url sa

testPathsTo :: [String] -> IO ()
testPathsTo args = do
    let url = T.pack (head args)
    c  <- connect myConn
    ps <- pathsTo c url
    mapM_ print ps

testSpiderHOdb :: Conf -> IO ()
testSpiderHOdb cnf = do
    let depth = optRecDepth (opts cnf)
    let url   = optUrl (opts cnf)
    c    <- connect myConn
    vstd <- spiderHO (proxy cnf) depth (insertAccessRel c) S.empty url
    putStrLn ("\n\n\n" ++ show (S.size vstd) ++ " visited:\n" ++ show vstd)
 
testSpiderHOconsole :: [String] -> IO ()
testSpiderHOconsole args = do
    let depth = read (head args)
    let url   = args !! 1
    cnf       <- buildConf defaultOptions
    vstd <- spiderHO (proxy cnf) depth print S.empty (T.pack url)
    putStrLn ("\n\n\n" ++ show (S.size vstd) ++ " visited:\n" ++ show vstd)
 
testSpiderRelHOconsole :: String -> Int -> IO ()
testSpiderRelHOconsole url depth = do
    cnf       <- buildConf defaultOptions
    vstd <- spiderRelHO (proxy cnf) depth print S.empty (T.pack url)
    putStrLn ("\n\n\n" ++ show (S.size vstd) ++ " visited:\n" ++ show vstd)
 
testSpiderRelHOdb :: String -> Int -> IO ()
testSpiderRelHOdb url depth = do
    c    <- connect myConn
    cnf  <- buildConf defaultOptions
    vstd <- spiderRelHO (proxy cnf) depth (insertAccessRel c) S.empty (T.pack url)
    putStrLn ("\n\n\n" ++ show (S.size vstd) ++ " visited:\n" ++ show vstd)

