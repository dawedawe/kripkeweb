module Axioms
( reflSymSubFrame
, reflTransSubFrames
, symTransSubFrames
, reflSymTransSubFrames
) where

import Control.Monad (liftM)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import LogicSearch
import Util

-- |Reflexive (T) and symmetric (B) subframe of (W, R).
reflSymSubFrame :: Connection -> IO Frame
reflSymSubFrame c = do
    refFr <- reflSubFrame c
    symFr <- symSubFrame c
    let reflSyms = S.filter (pairIn (wSet refFr)) (accRel symFr)
    -- generate needed reflexivs
    let symRefls = S.fromList [(x, x) | x <- flattenTuples (S.toList reflSyms)]
    let rels     = reflSyms `S.union` symRefls
    return (Frame (S.fromList (flattenTuples (S.toList rels))) rels)

-- |Reflexive (T) and transitive (4) subframes of (W, R).
reflTransSubFrames :: Connection -> IO (S.Set Frame)
reflTransSubFrames c = do
    reflsF <- reflSubFrame c
    transF <- transSubFrames c
    -- both ends of a relation must be reflexive, filter out elems not in reflsF
    let reflTransR = S.filter (/= S.empty)
                       (S.map (S.filter (pairIn (wSet reflsF)))
                       (S.map accRel transF))
    -- generate Frames with reflexive R-elements, then add reflexive rels
    let rtFrames   = [Frame (flattenTupleSet r) r | r <- S.toList reflTransR]
    return (S.fromList [Frame w (addRefls r) | (Frame w r) <- rtFrames])

-- |Symmetric (4) and transitive (4) subframes of (W, R).
symTransSubFrames :: Connection -> IO (S.Set Frame)
symTransSubFrames c = do
    syms  <- symSubFrame c
    trans <- transSubFrames c
    -- drop relations interacting with the source world of a missing symmetry
    let stR = [dropRelsWithElemInS b r | (Frame _ r) <- S.toList trans,
                let b = startOfMissingSyms (accRel syms) r]
    let stF = [Frame (flattenTupleSet r) r | r <- stR]
    return (S.fromList [Frame w (addSyms r) | (Frame w r) <- stF])

-- |Reflexive (T), symmetric (B) and transitive (4) subframes of (W, R).
reflSymTransSubFrames :: Connection -> IO (S.Set Frame)
reflSymTransSubFrames c = do
    reflsF <- reflSubFrame c
    stF    <- symTransSubFrames c
    -- both ends of a relation must be reflexive, filter out elems not in reflsF
    let reflSyTrR = S.filter (/= S.empty)
                      (S.map (S.filter (pairIn (wSet reflsF)))
                      (S.map accRel stF))
    -- generate Frames with reflexive R-elements, then add reflexive rels
    let rstFrames = [Frame (flattenTupleSet r) r | r <- S.toList reflSyTrR]
    return (S.fromList [Frame w (addRefls r) | (Frame w r) <- rstFrames])

-- |True, if both pair elements are elems of the list.
pairIn :: (Eq a, Ord a) => S.Set a -> (a, a) -> Bool
pairIn xs (y, z) = y `S.member` xs && z `S.member` xs

-- |Construct reflexive tuples out of Set elements.
makeReflTuples :: (Ord a) => S.Set a -> S.Set (a, a)
makeReflTuples xs = S.fromList [(x, x) | x <- S.toList xs]

startOfMissingSyms :: (Eq a, Ord a) => S.Set (a, a) -> S.Set (a, a) -> S.Set a
startOfMissingSyms syms rels =
    S.fromList [y | (x, y) <- S.toList rels, (y, x) `S.notMember` syms]

-- |Make a relation symmetric.
addSyms :: (Eq a, Ord a) => S.Set (a, a) -> S.Set (a, a)
addSyms rels =
    S.foldl S.union S.empty (S.map (\(x, y) -> S.fromList [(x, y), (y, x)]) rels)

-- |Make a relation symmetric.
addRefls :: (Eq a, Ord a) => S.Set (a, a) -> S.Set (a, a)
addRefls rels =
    (S.fromList [(x, x) | x <- S.toList (flattenTupleSet rels)]) `S.union` rels

-- |Drops duplicate lists, don't care about keeping order.
dropDuplicates :: (Eq a, Ord a) => [[a]] -> [[a]]
dropDuplicates = map S.toList . L.nub . map S.fromList

--------------------------------------------------------------------------------
-- test functions for the axioms

-- |Test if K (smallest modal logic): [](p -> q) -> ([]p -> []q) holds in the
-- whole database model.
isBigK :: Connection -> LambdaType -> MLFml -> MLFml -> IO Bool
isBigK c lamType (MLVar p) (MLVar q) = do
    let prem = Box (MLImp (MLVar p) (MLVar q))
    let conc = MLImp (Box (MLVar p)) (Box (MLVar q))
    let frm  = MLImp prem conc
    isUniversallyTrue c lamType frm
isBigK _ _       _         _         = error "isBigK: undefined parameters"

-- |Test if T (reflexivity): []p -> p holds in the given frame.
isBigT :: Connection -> LambdaType -> Frame -> MLFml -> IO Bool
isBigT c lamType frm p@(MLVar v) =
    let fml = MLImp (Box p) p
    in  isFUniversallyTrue c lamType frm fml
isBigT _ _       _   _           = error "isBigT: undefined parameters"

-- |Test if D (seriell): []p -> <>p All w: Ex v:(wRv) holds in the whole
-- database model.
isBigD :: Connection -> LambdaType -> MLFml -> IO Bool
isBigD c lamType p@(MLVar v) =
    let frm = MLImp (Box p) (Diamond p)
    in  isUniversallyTrue c lamType frm
isBigD _ _       _           = error "isBigD: undefined parameters"

-- |Test if B (symmetry): p -> []<>p holds in the given frame.
isBigB :: Connection -> LambdaType -> Frame -> MLFml -> IO Bool
isBigB c lamType frm p@(MLVar v) =
    let fml = MLImp p (Box (Diamond p))
    in  isFTrueInWorlds c lamType frm fml (wSet frm)
isBigB _ _       _   _           = error "isBigB: undefined parameters"

-- |Test if 4 (transitivity): []p -> [][]p holds in the given frame.
isBig4 :: Connection -> LambdaType -> Frame -> MLFml -> IO Bool
isBig4 c lamType frm p@(MLVar _) =
    let fml = MLImp (Box p) (Box (Box p))
    in  isFTrueInWorlds c lamType frm fml (wSet frm)
isBig4 _ _       _   _           = error "isBig4: undefined parameters"

-- |Test if 5 (refl. sym. trans.): <>p -> []<>p holds in given frame.
isBig5 :: Connection -> LambdaType -> Frame -> MLFml -> IO Bool
isBig5 c lamType frm p@(MLVar _) =
    let fml = MLImp (Diamond p) (Box (Diamond p))
    in  isFTrueInWorlds c lamType frm fml (wSet frm)
isBig5 _ _       _   _           = error "isBig5: undefined parameters"
