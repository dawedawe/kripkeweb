{-# LANGUAGE OverloadedStrings #-}

module FormulaSchemes
( atLeastOneSimilarAccWorld
, deadEndScheme
, lambdaAnded
, lambdaAndedBoxed
, lambdaAndedBoxedNegated
, lambdaAndedDiamonded
, lambdaAndedDiamondedNegated
, lambdaAndedNegated
, lambdaOred
, lambdaOredBoxed
, lambdaOredBoxedNegated
, lambdaOredDiamonded
, lambdaOredDiamondedNegated
, lambdaOredNegated
, tfidfsCuttedOred
, tfidfsTopXOred
, tfidfsTopXOredDiamonded
) where

import Data.Maybe (catMaybes, mapMaybe)
import Database.PostgreSQL.Simple
import qualified Data.Text as T

import DB
import KripkeTypes
import Logic
import Tfidf
import Util

-- |Scheme to decide if a world is a dead end or not.
deadEndScheme :: Fml
deadEndScheme = Not (Diamond (Or (Var "foo") (Not (Var "foo"))))

-- |Construct for every world a formula that demands at least one similar
-- accessable world.
atLeastOneSimilarAccWorld :: Connection -> LambdaType -> Int -> IO [Fml]
atLeastOneSimilarAccWorld c lamType prcnt = do
    ws <- worldsInLinks c
    ts <- mapM (worldsTopXPercentTfidf c lamType prcnt) ws
    return (mapMaybe atLeastOneSimilarAccWorldHelper ts)

atLeastOneSimilarAccWorldHelper :: [T.Text] -> Maybe Fml
atLeastOneSimilarAccWorldHelper fmls =
    let
      f1 = formulasToJunction And fmls
      f2 = formulasToJunction Or fmls
    in
      case (f1, f2) of
        (Just x, Just y) -> Just (And x (Diamond y))
        _                -> Nothing

-- |Ored tfidfs of all worlds with some of the highest and lowest ones dropped.
tfidfsCuttedOred :: Connection -> LambdaType -> Int -> IO [Fml]
tfidfsCuttedOred c lamType prcnt = do
    allTs  <- allTfidf c lamType
    let ws = map (dropFirstAndLastXPercent prcnt . map fst . snd) allTs
    return (mapMaybe (formulasToJunction Or) ws)

-- |The top X percent of each world's tfidf sorted formulas ored together.
tfidfsTopXOred :: Connection -> LambdaType -> Int -> IO [Fml]
tfidfsTopXOred c lamType prcnt = do
    allTs  <- allTfidfDB c lamType
    let ws = map (keepFirstXPercent prcnt . map fst . snd) allTs
    return (mapMaybe (formulasToJunction Or) ws)

-- |Like the formulas from tfidfsTopXOred, but each diamonded.
tfidfsTopXOredDiamonded :: Connection -> LambdaType -> Int -> IO [Fml]
tfidfsTopXOredDiamonded c lamType prcnt = do
    fmls <- tfidfsTopXOred c lamType prcnt
    return (map Diamond fmls)

-- |Lambda sets of all worlds as list of conjunctions.
lambdaAnded :: Connection -> LambdaType -> IO [Fml]
lambdaAnded c lamType = do
    ws   <- worldsInLambda c lamType
    fmls <- mapM (worldsLambdaCombined c lamType And) ws
    return (catMaybes fmls)

-- |Lambda sets of all worlds as list of disjunctions.
lambdaOred :: Connection -> LambdaType -> IO [Fml]
lambdaOred c lamType = do
    ws   <- worldsInLambda c lamType
    fmls <- mapM (worldsLambdaCombined c lamType Or) ws
    return (catMaybes fmls)

-- |Lambda sets of all worlds as list of negated conjunctions.
lambdaAndedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaAndedNegated c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Not andedFmls)

-- |Lambda sets of all worlds as lists of negated disjunctions.
lambdaOredNegated :: Connection -> LambdaType -> IO [Fml]
lambdaOredNegated c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Not oredFmls)

-- |Lambda sets of all worlds as lists of diamond conjunctions.
lambdaAndedDiamonded :: Connection -> LambdaType -> IO [Fml]
lambdaAndedDiamonded c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Diamond andedFmls)

-- |Lambda sets of all worlds as lists of diamond disjunctions.
lambdaOredDiamonded :: Connection -> LambdaType -> IO [Fml]
lambdaOredDiamonded c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Diamond oredFmls)

-- |Lambda sets of all worlds as lists of negated diamond conjunctions.
lambdaAndedDiamondedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaAndedDiamondedNegated c lamType = do
    andedDiamondedFmls <- lambdaAndedDiamonded c lamType
    return (map Not andedDiamondedFmls)

-- |Lambda sets of all worlds as lists of negated diamond disjunctions.
lambdaOredDiamondedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaOredDiamondedNegated c lamType = do
    oredDiamondedFmls <- lambdaOredDiamonded c lamType
    return (map Not oredDiamondedFmls)

-- |Lambda sets of all worlds as lists of diamond conjunctions.
lambdaAndedBoxed :: Connection -> LambdaType -> IO [Fml]
lambdaAndedBoxed c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Box andedFmls)

-- |Lambda sets of all worlds as lists of diamond disjunctions.
lambdaOredBoxed :: Connection -> LambdaType -> IO [Fml]
lambdaOredBoxed c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Box oredFmls)

-- |Lambda sets of all worlds as lists of negated diamond conjunctions.
lambdaAndedBoxedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaAndedBoxedNegated c lamType = do
    andedBoxedFmls <- lambdaAndedBoxed c lamType
    return (map Not andedBoxedFmls)

-- |Lambda sets of all worlds as lists of negated diamond disjunctions.
lambdaOredBoxedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaOredBoxedNegated c lamType = do
    oredBoxedFmls <- lambdaOredBoxed c lamType
    return (map Not oredBoxedFmls)

-- |Lambda formulas of a single world as one disjunction.
worldsLambdaCombined :: Connection -> LambdaType -> (Fml -> Fml -> Fml) ->
                        T.Text -> IO (Maybe Fml)
worldsLambdaCombined c lamType j w = do
    fmls <- worldFormulas c lamType w
    return (formulasToJunction j fmls)
    
-- |Convert a T.Text list into a PL (dis/kon)junction.
formulasToJunction :: (Fml -> Fml -> Fml) -> [T.Text] -> Maybe Fml
formulasToJunction _ []     = Nothing
formulasToJunction j (f:fs) =
    let
      accu = Var f
      fmls = map Var fs
    in
      Just (foldl j accu fmls)

