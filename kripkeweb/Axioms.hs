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
    let symRefls = [(x, x) | x <- map fst reflSyms] -- generate needed reflexivs
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
    return (map L.nub (zipWith (++) reflTrans transRefls))

-- |Symmetric (4) and transitive (4) subsets of R.
symTransSubSets :: Connection -> IO [[(T.Text, T.Text)]]
symTransSubSets c = do
    syms  <- symSubSet c
    trans <- transSubSets c
    let trans'     = map (filter (uncurry (/=))) trans
    -- drop relations interacting with the source world of a missing symmetry
    let badOnes    = map (startOfMissingSyms syms) trans' 
    let symTrans   = [dropRelsWithElemIn bs ts | (ts, bs) <- zip trans' badOnes]
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
    let symtraRefls = map (makeReflTuples . flattenTuples) refSymTrans
    let resultSets = map L.nub (zipWith (++) refSymTrans symtraRefls)
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

-- |Drops duplicate lists, don't care about order.
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
    sw <- satWorlds c lamType frm
    ws <- worldsInLambda c lamType
    return (S.fromList sw == S.fromList ws)
isBigK _ _       _         _         = error "isBigK: undefined parameters"
