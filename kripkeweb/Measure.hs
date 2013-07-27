module Measure
( edgesInDigraphClique
) where

-- |Edge count in a total directed graph.
edgesInDigraphClique :: Int -> Int
edgesInDigraphClique n = n * (n - 1)


