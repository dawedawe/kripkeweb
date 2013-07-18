module Relation
( containsTransRel
, dropReflRels
, dropRelsWithElemIn
, dropRelsWithElemInS
, dropTransViolations
, dropOverlappingPairs
, flattenTuples
, flattenTupleSet
, hasTransViolation
, sourcesOf'
, relCountAmongWorlds
, relsEndingWith
, relsStartingIn'
, relsStartingWith'
, targetsOf'
, unreflSourcesOf
, unreflRelCountAmongWorlds
, unreflTargetsOf
) where

import qualified Data.List as L
import qualified Data.Set as S

-- |Drop relations containing one or two elements of the given list.
dropRelsWithElemIn :: (Eq a) => [a] -> [(a, a)] -> [(a, a)]
dropRelsWithElemIn xs = filter (\(x, y) -> x `notElem` xs && y `notElem` xs)

-- |Drop relations containing one or two elements of the given list.
dropRelsWithElemInS :: (Eq a, Ord a) => S.Set a -> S.Set (a, a) -> S.Set (a, a)
dropRelsWithElemInS xs =
    S.filter (\(x, y) -> x `S.notMember` xs && y `S.notMember` xs)

-- |Take the elements out of every tuple and remove duplicates.
flattenTuples :: (Eq a) => [(a, a)] -> [a]
flattenTuples xs = L.nub $ concatMap (\(x, y) -> [x, y]) xs

-- |Take the elements out of every tuple and remove duplicates.
flattenTupleSet :: (Eq a, Ord a) => S.Set (a, a) -> S.Set a
flattenTupleSet xs = S.unions (map (\(x, y) -> S.fromList [x, y]) (S.toList xs))

-- |Pure version of relsStartingWith.
relsStartingWith' :: (Eq a) => [(a, a)] -> a -> [(a, a)]
relsStartingWith' rels w = filter ((== w) . fst) rels

-- |Pure version of relsStartingWith.
unreflRelsStartingWith :: (Eq a) => [(a, a)] -> a -> [(a, a)]
unreflRelsStartingWith rels w = filter (\(x, y) -> x == w && y /= w) rels

-- |Pure version of targetsOf
targetsOf' :: (Eq a) => [(a, a)] -> a -> [a]
targetsOf' rels w = map snd (relsStartingWith' rels w)

-- |Pure version of targetsOf, without reflexive relations.
unreflTargetsOf :: (Eq a) => [(a, a)] -> a -> [a]
unreflTargetsOf rels w = map snd (unreflRelsStartingWith rels w)

-- |Pure version of relsStartingIn.
relsStartingIn' :: (Eq a) => [(a, a)] -> [a] -> [(a, a)]
relsStartingIn' rels tgs = filter ((`elem` tgs) . fst) rels

-- |Relations ending with given element.
relsEndingWith :: (Eq a) => [(a, a)] -> a -> [(a, a)]
relsEndingWith rels w = filter (\(_, y) -> y == w) rels

-- |Relations ending with given element, without reflexive relations.
unreflRelsEndingWith :: (Eq a) => [(a, a)] -> a -> [(a, a)]
unreflRelsEndingWith rels w = filter (\(x, y) -> y == w && x /= w) rels

-- |Elements pointing at given element.
sourcesOf' :: (Eq a) => [(a, a)] -> a -> [a]
sourcesOf' rels w = map fst (relsEndingWith rels w)

-- |Elements pointing at given element, without reflexive relations.
unreflSourcesOf :: (Eq a) => [(a, a)] -> a -> [a]
unreflSourcesOf rels w = map fst (unreflRelsEndingWith rels w)

-- |True, if not all targets of targets of w can be reached directly from w.
hasTransViolation :: (Eq a) => [(a, a)] -> a -> Bool
hasTransViolation rels w =
    let
      relsOfW = filter (/= (w, w)) (relsStartingWith' rels w)
      trgsOfw = map snd relsOfW
      totRels = filter ((/= w) . snd) (relsStartingIn' rels trgsOfw)
      tOft    = map snd totRels
    in 
      tOft `L.intersect` trgsOfw /= tOft

-- |Drop relations interacting with worlds, that have transitive violations.
dropTransViolations :: (Eq a) => [(a, a)] -> [(a, a)]
dropTransViolations rels = 
    let vs = filter (hasTransViolation rels) (flattenTuples rels)
    in  dropRelsWithElemIn vs rels

-- |True, if given relations contain at least one transitive relation.
containsTransRel :: (Eq a, Ord a) => [(a, a)] -> Bool
containsTransRel rels = any (hasTransRelation rels) (flattenTuples rels)

-- |True, if given element has a transitive relation in the given relations.
hasTransRelation :: (Eq a, Ord a) => [(a, a)] -> a -> Bool
hasTransRelation rels w =
    let
      tgsOfw   = targetsOf' rels w
      tgsOfTgs = concatMap (targetsOf' rels) tgsOfw
      interS   = tgsOfw `L.intersect` tgsOfTgs
    in
      tgsOfw /= [] && tgsOfTgs /= [] && interS /= [] &&
      S.fromList interS `S.isSubsetOf` S.fromList tgsOfw

-- |Drop additional pairs in the list that share an element.
dropOverlappingPairs :: (Eq a) => [(a, a)] -> [(a, a)]
dropOverlappingPairs [] = []
dropOverlappingPairs ((x1, x2):xs) =
    let
      ps = filter (\(y1,y2) -> y1 `notElem` [x1,x2] && y2 `notElem` [x1,x2]) xs
    in
      (x1, x2) : dropOverlappingPairs ps

-- |Count of relations between elements in the given list.
relCountAmongWorlds :: (Eq a) => [(a, a)] -> [a] -> Int
relCountAmongWorlds rel ws =
    length $ filter (\(s, t) -> s `elem` ws && t `elem` ws) rel

-- |Count of relations between elements in the given list, not counting
-- reflexive ones.
unreflRelCountAmongWorlds :: (Eq a) => [(a, a)] -> [a] -> Int
unreflRelCountAmongWorlds rel ws =
    length $ filter (\(s, t) -> s `elem` ws && t `elem` ws && s /= t) rel

-- |Drop reflexive relations.
dropReflRels :: (Eq a) => [(a, a)] -> [(a, a)]
dropReflRels = filter (\(a, b) -> a /= b)

