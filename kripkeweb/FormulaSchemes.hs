module FormulaSchemes
( lambdaAnded
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
) where

import Data.Maybe (catMaybes)
import Database.PostgreSQL.Simple
import qualified Data.Text as T

import DB
import KripkeTypes
import Logic

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

