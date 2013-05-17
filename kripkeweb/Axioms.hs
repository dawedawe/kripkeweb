module Axioms
( reflSymSubSet
, reflTransSubSets
, symTransSubSets
, reflSymTransSubSets
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

-- |Reflexive (T) and symmetric (B) subset of R.
reflSymSubSet :: Connection -> IO [(T.Text, T.Text)]
reflSymSubSet c = do
    refls <- reflSubSet c
    syms  <- symSubSet c
    let reflSyms = filter (pairIn refls) syms
    -- generate needed reflexivs
    let symRefls = [(x, x) | x <- flattenTuples reflSyms]
    return (reflSyms ++ symRefls)

-- |Reflexive (T) and transitive (4) subsets of R.
reflTransSubSets :: Connection -> IO [[(T.Text, T.Text)]]
reflTransSubSets c = do
    refls <- reflSubSet c
    trans <- transSubSets c
    -- both ends of a relation must be reflexive
    let reflTrans = filter (/= []) (map (filter (pairIn refls)) trans)
    -- generate needed reflexives to zip together the results
    let transRefls = map (makeReflTuples . flattenTuples) reflTrans
    return (zipWith (++) reflTrans transRefls)

-- |Symmetric (4) and transitive (4) subsets of R.
symTransSubSets :: Connection -> IO [[(T.Text, T.Text)]]
symTransSubSets c = do
    syms  <- symSubSet c
    trans <- transSubSets c
    -- drop relations interacting with the source world of a missing symmetry
    let badOnes    = map (startOfMissingSyms syms) trans
    let symTrans   = [dropRelsWithElemIn bs ts | (ts, bs) <- zip trans badOnes]
    let resultSets = map addSyms symTrans -- add symmetric relations to result
    return (dropDuplicates resultSets) -- we might have constructed duplicates

-- |Reflexive (T), symmetric (B) and transitive (4) subsets of R.
reflSymTransSubSets :: Connection -> IO [[(T.Text, T.Text)]]
reflSymTransSubSets c = do
    refls <- reflSubSet c
    st    <- symTransSubSets c
    -- both ends of a relation must be reflexive
    let refSymTrans = filter (/= []) (map (filter (pairIn refls)) st)
    -- generate needed reflexives to zip together the results
    let symTraRefls = map (makeReflTuples . flattenTuples) refSymTrans
    let resultSets = map L.nub (zipWith (++) refSymTrans symTraRefls)
    return (dropDuplicates resultSets) -- we might have constructed duplicates

-- |True, if both pair elements are elems of the list.
pairIn :: (Eq a) => [a] -> (a, a) -> Bool
pairIn xs (y, z) = y `elem` xs && z `elem` xs

-- |Construct reflexive tuples out of list elements.
makeReflTuples :: [a] -> [(a, a)]
makeReflTuples xs = [(x, x) | x <- xs]

startOfMissingSyms :: (Eq a) => [(a, a)] -> [(a, a)] -> [a]
startOfMissingSyms syms rels = [y | (x, y) <- rels, (y, x) `notElem` syms]

-- |Make a relation symmetric.
addSyms :: (Eq a) => [(a, a)] -> [(a, a)]
addSyms = L.nub . concatMap (\(x, y) -> [(x, y), (y, x)])

-- |Drops duplicate lists, don't care about keeping order.
dropDuplicates :: (Eq a, Ord a) => [[a]] -> [[a]]
dropDuplicates = map S.toList . L.nub . map S.fromList

--------------------------------------------------------------------------------
-- test functions for the axioms

-- |Test if K: [](p -> q) -> ([]p -> []q) holds in every world.
isBigK :: Connection -> LambdaType -> MLFml -> MLFml -> IO Bool
isBigK c lamType (MLVar p) (MLVar q) = do
    let prem = Box (MLImp (MLVar p) (MLVar q))
    let conc = MLImp (Box (MLVar p)) (Box (MLVar q))
    let frm  = MLImp prem conc
    isUniversallyTrue c lamType frm
isBigK _ _       _         _         = error "isBigK: undefined parameters"

-- |Test if T: []p -> p (+K, reflexivity) holds in the given set of worlds.
isBigT :: Connection -> LambdaType -> [T.Text] -> MLFml -> IO Bool
isBigT c lamType ws p@(MLVar v) =
    let frm = MLImp (Box p) p
    in  isTrueInWorlds c lamType frm ws
isBigT _ _       _  _           = error "isBigT: undefined parameters"

-- |Test if D: []p -> <>p (+K, seriell) All w: Ex v(wRv) holds in every worlds.
isBigD :: Connection -> LambdaType -> MLFml -> IO Bool
isBigD c lamType p@(MLVar v) =
    let frm = MLImp (Box p) (Diamond p)
    in  isUniversallyTrue c lamType frm
isBigD _ _       _           = error "isBigD: undefined parameters"

-- |Test if B: p -> []<>p (+T, refl. symmetry) holds in the given set of worlds.
isBigB :: Connection -> LambdaType -> [T.Text] -> MLFml -> IO Bool
isBigB c lamType ws p@(MLVar v) =
    let frm = MLImp p (Box (Diamond p))
    in  isTrueInWorlds c lamType frm ws
isBigB _ _       _  _           = error "isBigB: undefined parameters"

-- |Test if 4: []p -> [][]p (+T, refl. trans.) holds in the given set of worlds.
isBig4 :: Connection -> LambdaType -> [T.Text] -> MLFml -> IO Bool
isBig4 c lamType ws p@(MLVar _) =
    let frm = MLImp (Box p) (Box (Box p))
    in  isTrueInWorlds c lamType frm ws
isBig4 _ _       _  _           = error "isBig4: undefined parameters"

-- |Test if 5: <>p -> []<>p (+T, refl. trans. symm holds in given set of worlds.
isBig5 :: Connection -> LambdaType -> [T.Text] -> MLFml -> IO Bool
isBig5 c lamType ws p@(MLVar _) =
    let frm = MLImp (Diamond p) (Box (Diamond p))
    in  isTrueInWorlds c lamType frm ws
isBig5 _ _       _  _           = error "isBig5: undefined parameters"
