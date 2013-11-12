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
import Logic
import Model
import PageRank
import RoughSet
import Tfidf

-- |Parse the command line options and the config file and obey the user.
main :: IO ()
main = do
    parsedArgv <- getArgs >>= parseArgv
    let parsedOptions = fst parsedArgv
    createDotDir
    conf <- buildConf parsedOptions

    when (S.null (optFlags (opts conf))) $ do
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
      buildLambdaStore c (proxy conf)

    when (CalcTfidf `S.member` optFlags (opts conf)) $ do
      storeAllTfidf c BdyRaw
      storeAllTfidf c BdyStem
      storeAllTfidf c BdySoundex
      storeAllTfidf c MtaRaw
      storeAllTfidf c MtaStem
      storeAllTfidf c MtaSoundex

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
      let frm = read (T.unpack (optFml (opts conf))) :: PLFml
      putStrLn ("plformula: " ++ show frm ++ " =")
      ws <- satWorlds c lamType frm
      mapM_ print ws

    when (PMLFmlEval `S.member` optFlags (opts conf)) $ do
      let frm = read (T.unpack (optFml (opts conf))) :: Fml
      putStrLn ("pmlformula: " ++ show frm ++ " =")
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
      let frm = read (T.unpack (optFml (opts conf))) :: PLFml
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

