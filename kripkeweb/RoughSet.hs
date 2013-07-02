module RoughSet
( RoughSet (..)
, printRoughSet
, rBoundary
, roughSetOfLamPL
, roughSetOfLamList
, roughSetOfWorldsLam
, roughSetOfInLinks
, roughSetOfOutLinks
, roughSetOfWorldsOutLinks
, roughSetOfWorldsInLinks
) where

import Data.List ((\\), intersect, union)
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import LogicSearch

data RoughSet = RoughSet { lowerApprox :: [T.Text]
                         , upperApprox ::  [T.Text]
                         } deriving (Eq, Show)

-- |Pretty print a RoughSet.
printRoughSet :: RoughSet -> IO ()
printRoughSet (RoughSet l u) = do
    putStrLn "lower approximation = {"
    mapM_ print l
    putStrLn "}"
    putStrLn "upper approximation = {"
    mapM_ print u
    putStrLn "}"

-- |Upper Approximation - lower Approximation.
rBoundary :: RoughSet -> [T.Text]
rBoundary (RoughSet l u) = u \\ l

-- |Take the formula set of a world as the target concept.
roughSetOfWorldsLam :: Connection -> LambdaType -> T.Text -> IO RoughSet
roughSetOfWorldsLam c lamType w = do
    lOfW    <- worldFormulas c lamType w
    roughSetOfLamList c lamType lOfW

-- |Take a list of lambda formulas as the target concept.
-- Formulas are expected to be already in LambdaType form.
roughSetOfLamList :: Connection -> LambdaType -> [T.Text] -> IO RoughSet
roughSetOfLamList c lamType l = do
    lApprox <- worldsWithFmlSetSubsetOf c lamType l
    uApprox <- worldsWithFmlSetIntersectWith c lamType l
    return (RoughSet lApprox uApprox)

-- |Map Propositional Logic to combinations of Rough Set calculations.
roughSetOfLamPL :: Connection -> LambdaType -> PLFml -> IO RoughSet
roughSetOfLamPL c lamType fml =
    case fml of
      (PLVar x)   -> roughSetOfLamList c lamType [x]
      (PLNot x)   -> do
                     (RoughSet lx ux) <- roughSetOfLamPL c lamType x
                     upp              <- negateLambaWorlds c lamType lx
                     low              <- negateLambaWorlds c lamType ux
                     return (RoughSet low upp)
      (PLAnd x y) -> do
                     (RoughSet lx ux) <- roughSetOfLamPL c lamType x
                     (RoughSet ly uy) <- roughSetOfLamPL c lamType y
                     let low = lx `intersect` ly
                     let upp = ux `intersect` uy
                     return (RoughSet low upp)
      (PLOr x y)  -> do
                     (RoughSet lx ux) <- roughSetOfLamPL c lamType x
                     (RoughSet ly uy) <- roughSetOfLamPL c lamType y
                     let low = lx `union` ly
                     let upp = ux `union` uy
                     return (RoughSet low upp)
      (PLImp x y) -> roughSetOfLamPL c lamType (PLOr (PLNot x) y)

-- |Take a list of out-links as the target concept.
roughSetOfInLinks :: Connection -> [T.Text] -> IO RoughSet
roughSetOfInLinks c ls = do
    lApprox <- worldsWithILinksSubsetOf c ls
    uApprox <- worldsWithILinksIntersectWith c ls
    return (RoughSet lApprox uApprox)

-- |Take a list of out-links as the target concept.
roughSetOfOutLinks :: Connection -> [T.Text] -> IO RoughSet
roughSetOfOutLinks c ls = do
    lApprox <- worldsWithOLinksSubsetOf c ls
    uApprox <- worldsWithOLinksIntersectWith c ls
    return (RoughSet lApprox uApprox)

-- |Take a world's out-links as the target concept.
roughSetOfWorldsInLinks :: Connection -> T.Text -> IO RoughSet
roughSetOfWorldsInLinks c w = do
    ls <- sourcesOf c w
    roughSetOfInLinks c ls

-- |Take a world's out-links as the target concept.
roughSetOfWorldsOutLinks :: Connection -> T.Text -> IO RoughSet
roughSetOfWorldsOutLinks c w = do
    ls <- targetsOf c w
    roughSetOfOutLinks c ls
