module Cluster
( boolsDist
, fmlSpacePos
, fmlSpacePoses
) where

import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import LogicSearch
import Model

-- |fmlSpacePos vectors of all given worlds.
fmlSpacePoses :: (AsLambdaType f, PTrueIn f) => Connection -> LambdaType ->
                 [f] -> [T.Text] -> IO [[Bool]]
fmlSpacePoses c lamType fmls ws = do
    dbMod <- dbModel c lamType
    fmls' <- mapM (fmlAsLambdaType c lamType Nothing) fmls
    return (map (fmlSpacePos dbMod fmls') ws)

-- |Position of a world with regard to a list of formulas spanning a space.
-- Expects the formulas to be already in the right LambdaType.
fmlSpacePos :: (PTrueIn f) => Model -> [f] -> T.Text -> [Bool]
fmlSpacePos mdl fmls w = map (isPTrueInWorld mdl w) fmls

-- |Distance of two Bool Lists.
boolsDist :: [Bool] -> [Bool] -> Double
boolsDist x y
    | length x == length y = sqrt (fromIntegral (sum (zipWith boolDist x y)))
    | otherwise            = error "boolsDist: lists of differing length"

-- |Distance of two Bools.
boolDist :: Bool -> Bool -> Int
boolDist x y
    | x == y    = 0
    | otherwise = 1
