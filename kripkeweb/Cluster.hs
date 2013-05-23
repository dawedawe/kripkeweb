module Cluster
( boolsDist
, posFmlSpace
) where

import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import LogicSearch
import Model

-- |Position of a world with regard to a list of formulas spanning a space.
posFmlSpace :: (PTrueIn f) => Connection -> LambdaType -> [f] -> T.Text ->
               IO [Bool]
posFmlSpace c lamType fmls w = do
    dbMod <- dbModel c lamType
    return (map (isPTrueInWorld dbMod w) fmls)

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
