module Measure
( disimilarity
, edgesInDigraphClique
, similarity
) where

import qualified Data.Set as S

-- |Simple similarity measure: |x intersect y| / |x union y|.
similarity :: (Eq a, Ord a) => S.Set a -> S.Set a -> Double
similarity x y
    | S.null x && S.null y = 1.0
    | S.null x /= S.null y = 0.0
    | otherwise            =
        let
          interSize = S.size (x `S.intersection` y)
          unionSize = S.size (x `S.union` y)
        in
          fromIntegral interSize / fromIntegral unionSize

-- |Simple disimilarity measure: 1 - similarity x y.
disimilarity :: (Eq a, Ord a) => S.Set a -> S.Set a -> Double
disimilarity x y = 1 - similarity x y

-- |Edge count in a total directed graph.
edgesInDigraphClique :: Int -> Int
edgesInDigraphClique n = n * (n - 1)

