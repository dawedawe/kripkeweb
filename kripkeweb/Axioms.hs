module Axioms
( reflSymSubFrame
, reflTransSubFrames
, symTransSubFrames
, reflSymTransSubFrames
) where

import qualified Data.List as L
import qualified Data.Set as S
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import Logic
import Relation

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
    -- and make sure the relation sets still contain a transitive relation
    let reflTransR = S.filter (containsTransRel . S.toList)
                       (S.map (S.filter (pairIn (wSet reflsF)))
                       (S.map accRel transF))
    -- generate Frames with reflexive R-elements, then add reflexive rels
    let rtFrames   = [Frame (flattenTupleSet r) r | r <- S.toList reflTransR]
    return (S.fromList [Frame w (addRefls r) | (Frame w r) <- rtFrames])

-- |Symmetric (B) and transitive (4) subframes of (W, R).
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
    S.fromList [(x, x) | x <- S.toList (flattenTupleSet rels)] `S.union` rels

-- |Drops duplicate lists, don't care about keeping order.
dropDuplicates :: (Eq a, Ord a) => [[a]] -> [[a]]
dropDuplicates = map S.toList . L.nub . map S.fromList

--------------------------------------------------------------------------------
-- test functions for the axioms

-- |Test if K (smallest modal logic): [](p -> q) -> ([]p -> []q) holds in the
-- given frame.
isK :: Connection -> LambdaType -> Frame -> Fml -> Fml -> IO Bool
isK c lamType frm p@(Var _) q@(Var _) = do
    let prem = Box (Imp p q)
    let conc = Imp (Box p) (Box q)
    let fml  = Imp prem conc
    isFUniversallyTrue c lamType frm fml
isK _ _       _   _           _           = error "isBigK: undefined parameters"

-- |Test if T (reflexive): p -> <>p  (alt: []p -> p) holds in the given frame.
isT :: Connection -> LambdaType -> Frame -> Fml -> IO Bool
isT c lamType frm p@(Var _) =
    let fml = Imp p (Diamond p)
    in  isFUniversallyTrue c lamType frm fml
isT _ _       _   _           = error "isBigT: undefined parameters"

-- |Test if D (seriell): []p -> <>p All w: Ex v:(wRv) holds in the whole
-- database model.
isD :: Connection -> LambdaType -> Frame -> Fml -> IO Bool
isD c lamType frm p@(Var _) =
    let fml = Imp (Box p) (Diamond p)
    in  isFUniversallyTrue c lamType frm fml
isD _ _       _   _           = error "isBigD: undefined parameters"

-- |Test if B (symmetric): p -> []<>p holds in the given frame.
isB :: Connection -> LambdaType -> Frame -> Fml -> IO Bool
isB c lamType frm p@(Var _) =
    let fml = Imp p (Box (Diamond p))
    in  isFTrueInWorlds c lamType frm fml (wSet frm)
isB _ _       _   _           = error "isBigB: undefined parameters"

-- |Test if 4 (transitive): <><>p -> <>p (alt: []p -> [][]p) holds in the given
-- frame.
is4 :: Connection -> LambdaType -> Frame -> Fml -> IO Bool
is4 c lamType frm p@(Var _) =
    let fml = Imp (Diamond (Diamond p)) (Diamond p)
    in  isFTrueInWorlds c lamType frm fml (wSet frm)
is4 _ _       _   _           = error "isBig4: undefined parameters"

-- |Test if 5 (euclidean): <>p -> []<>p holds in given frame.
is5 :: Connection -> LambdaType -> Frame -> Fml -> IO Bool
is5 c lamType frm p@(Var _) =
    let fml = Imp (Diamond p) (Box (Diamond p))
    in  isFTrueInWorlds c lamType frm fml (wSet frm)
is5 _ _       _   _           = error "isBig5: undefined parameters"
