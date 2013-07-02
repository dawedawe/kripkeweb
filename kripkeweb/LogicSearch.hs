{-# LANGUAGE OverloadedStrings #-}

module LogicSearch
( AsLambdaType
, PLFml (..)
, Fml (..)
, MTrueIn
, Quantor (..)
, TrueIn
, eval2B
, fmlAsLambdaType
, isFTrueInWorld
, isFTrueInWorlds
, isFUniversallyTrue
, isMTrueInWorld
, isTrueInWorld
, isTrueInWorlds
, isUniversallyTrue
, lambdaAnded
, lambdaAndedBoxed
, lambdaAndedBoxedNegated
, lambdaAndedDiamonded
, lambdaAndedDiamondedNegated
, lambdaAndedNegated
, lambdaOred
, lambdaOredBoxed
, lambdaOredBoxedNegated
, lambdaOredDiamonded
, lambdaOredDiamondedNegated
, lambdaOredNegated
, satWorlds
, satFWorlds
) where

import Control.Monad (filterM, liftM, liftM2)
import Data.List ((\\), elemIndices, intersect, union)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import Model
import Util

-- |Check if x is true in ... Look up needed information in the database.
class TrueIn x where
    isTrueInWorld     :: Connection -> LambdaType -> x -> T.Text -> IO Bool
    isTrueInWorlds    :: Connection -> LambdaType -> x -> [T.Text] -> IO Bool
    isUniversallyTrue :: Connection -> LambdaType -> x -> IO Bool

-- |Like TrueIn but take the frame as an argument instead of out of the
-- database. Further information is looked up in the database.
class FTrueIn x where
    isFTrueInWorld     :: Connection -> LambdaType -> Frame -> x -> T.Text ->
                          IO Bool
    isFTrueInWorlds    :: Connection -> LambdaType -> Frame -> x ->
                          S.Set T.Text -> IO Bool
    isFUniversallyTrue :: Connection -> LambdaType -> Frame -> x -> IO Bool

-- |Uses precomputed model to evaluate x. Expects the given model to have the
-- right lambda type.
class MTrueIn x where
    isMTrueInWorld :: Model -> T.Text -> x -> Bool

-- |Worlds satisfying x.
class SatWorlds x where
    satWorlds :: Connection -> LambdaType -> x -> IO [T.Text]

-- |Worlds in frame satisfying x.
class FSatWorlds x where
    satFWorlds :: Connection -> LambdaType -> Frame -> x -> IO [T.Text]

-- |Stuff that can be transformed to other LambdaType representations.
class AsLambdaType x where
    fmlAsLambdaType :: Connection -> LambdaType -> Maybe T.Text -> x -> IO x

class Eval2Bool x where
    eval2B :: Connection -> LambdaType -> x -> IO Bool

-- |Fml set of Propositional Logic.
data PLFml = PLVar T.Text
           | PLNot PLFml
           | PLAnd PLFml PLFml
           | PLOr  PLFml PLFml
           | PLImp PLFml PLFml

-- |Fml set of Modal Logic
data Fml = Var T.Text
         | Not Fml
         | And Fml Fml
         | Or  Fml Fml
         | Imp Fml Fml
         | Box Fml
         | Diamond Fml

instance SatWorlds PLFml where
  satWorlds c lamType (PLVar phi) = do
    phi' <- termAsLamType c lamType Nothing phi
    worldsWithFormula c lamType phi'

  satWorlds c lamType (PLNot phi) =
    liftM2 (\\) (worldsInLambda c lamType) (satWorlds c lamType phi)

  satWorlds c lamType (PLAnd phi psi) =
    liftM2 intersect (satWorlds c lamType phi) (satWorlds c lamType psi)

  satWorlds c lamType (PLOr phi psi) =
    liftM2 union (satWorlds c lamType phi) (satWorlds c lamType psi)

  satWorlds c lamType (PLImp phi psi) =
    satWorlds c lamType (PLOr (PLNot phi) psi)

instance SatWorlds Fml where
  satWorlds c lamType (Var phi) = do
    phi' <- termAsLamType c lamType Nothing phi
    worldsWithFormula c lamType phi'

  satWorlds c lamType (Not phi) =
    liftM2 (\\) (worldsInLambda c lamType) (satWorlds c lamType phi)

  satWorlds c lamType (And phi psi) =
    liftM2 intersect (satWorlds c lamType phi) (satWorlds c lamType psi)

  satWorlds c lamType (Or phi psi) =
    liftM2 union (satWorlds c lamType phi) (satWorlds c lamType psi)

  satWorlds c lamType (Imp phi psi) =
    satWorlds c lamType (Or (Not phi) psi)

  satWorlds c lamType (Box phi) = do
    allWorlds <- worldsInLambda c lamType
    filterM (isTrueInWorld c lamType (Box phi)) allWorlds

  satWorlds c lamType (Diamond phi) = do
    allWorlds <- worldsInLambda c lamType
    filterM (isTrueInWorld c lamType (Diamond phi)) allWorlds

instance FSatWorlds Fml where
  satFWorlds c lamType (Frame w _) (Var phi) = do
    phi' <- termAsLamType c lamType Nothing phi
    liftM (S.toList w `intersect`) (worldsWithFormula c lamType phi')

  satFWorlds c lamType frm@(Frame w _) (Not phi) =
    liftM (S.toList w \\) (satFWorlds c lamType frm phi)

  satFWorlds c lamType frm@(Frame w _) (And phi psi) = do
    dbWorlds <- liftM2 intersect
                  (satFWorlds c lamType frm phi) (satFWorlds c lamType frm psi)
    return (S.toList w `intersect` dbWorlds)

  satFWorlds c lamType frm@(Frame w _) (Or phi psi) = do
    dbWorlds <- liftM2 union
                  (satFWorlds c lamType frm phi) (satFWorlds c lamType frm psi)
    return (S.toList w `intersect` dbWorlds)

  satFWorlds c lamType frm (Imp phi psi) =
    satFWorlds c lamType frm (Or (Not phi) psi)

  satFWorlds c lamType frm@(Frame w _) (Box phi) =
    filterM (isFTrueInWorld c lamType frm (Box phi)) (S.toList w)

  satFWorlds c lamType frm@(Frame w _) (Diamond phi) =
    filterM (isFTrueInWorld c lamType frm (Diamond phi)) (S.toList w)

instance TrueIn PLFml where
  isTrueInWorld c lamType (PLVar phi) w = isTrueInWorld c lamType (Var phi) w

  isTrueInWorld c lamType (PLNot phi) w =
    liftM not (isTrueInWorld c lamType phi w)

  isTrueInWorld c lamType (PLAnd phi psi) w =
    liftM2 (&&) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (PLOr phi psi) w =
    liftM2 (||) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (PLImp phi psi) w =
    isTrueInWorld c lamType (PLOr (PLNot phi) psi) w

  isTrueInWorlds c lamType fml ws =
    liftM and (mapM (isTrueInWorld c lamType fml) ws)

  isUniversallyTrue c lamType fml =
    liftM2 eqListElems (satWorlds c lamType fml) (worldsInLambda c lamType)

instance MTrueIn PLFml where
  isMTrueInWorld (Model _ lam) w (PLVar phi) = phi `elem` lam w

  isMTrueInWorld mdl w (PLNot phi) = not (isMTrueInWorld mdl w phi)

  isMTrueInWorld mdl w (PLAnd phi psi) =
    isMTrueInWorld mdl w phi && isMTrueInWorld mdl w psi

  isMTrueInWorld mdl w (PLOr phi psi) =
    isMTrueInWorld mdl w phi || isMTrueInWorld mdl w psi

  isMTrueInWorld mdl w (PLImp phi psi) =
    isMTrueInWorld mdl w (PLNot phi) || isMTrueInWorld mdl w psi

instance MTrueIn Fml where
    isMTrueInWorld (Model _ lam) w (Var phi) = phi `elem` lam w

    isMTrueInWorld mdl w (Not phi) = not (isMTrueInWorld mdl w phi)

    isMTrueInWorld mdl w (And phi psi) =
      isMTrueInWorld mdl w phi && isMTrueInWorld mdl w psi

    isMTrueInWorld mdl w (Or phi psi) =
      isMTrueInWorld mdl w phi || isMTrueInWorld mdl w psi

    isMTrueInWorld mdl w (Imp phi psi) =
      isMTrueInWorld mdl w (Or (Not phi) psi)

    isMTrueInWorld mdl@(Model (Frame _ ar) _) w (Box phi) =
      let tgs = targetsOf' (S.toList ar) w
      in  and [isMTrueInWorld mdl t phi | t <- tgs]

    isMTrueInWorld mdl@(Model (Frame _ ar) _) w (Diamond phi) =
      let tgs = targetsOf' (S.toList ar) w
      in  or [isMTrueInWorld mdl t phi | t <- tgs]

instance TrueIn Fml where
  isTrueInWorld c lamType (Var phi) w = do
    phi' <- termAsLamType c lamType (Just w) phi
    fmls <- lambda c lamType w
    return (phi' `elem` fmls)

  isTrueInWorld c lamType (Not phi) w =
    liftM not (isTrueInWorld c lamType phi w)

  isTrueInWorld c lamType (And phi psi) w =
    liftM2 (&&) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (Or phi psi) w =
    liftM2 (||) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (Imp phi psi) w =
    isTrueInWorld c lamType (Or (Not phi) psi) w

  isTrueInWorld c lamType (Box phi) w = do
    tgs <- targetsOf c w
    liftM and (mapM (isTrueInWorld c lamType phi) tgs)
      
  isTrueInWorld c lamType (Diamond phi) w = do
    tgs <- targetsOf c w
    liftM or (mapM (isTrueInWorld c lamType phi) tgs)
 
  isTrueInWorlds c lamType fml ws =
    liftM and (mapM (isTrueInWorld c lamType fml) ws)

  isUniversallyTrue c lamType fml =
    liftM2 eqListElems (satWorlds c lamType fml) (worldsInLambda c lamType)

instance FTrueIn Fml where
  isFTrueInWorld c lamType frm (Var phi) w
    | w `S.member` wSet frm = isTrueInWorld c lamType (Var phi) w
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (Not phi) w
    | w `S.member` wSet frm = liftM not (isFTrueInWorld c lamType frm phi w)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (And phi psi) w
    | w `S.member` wSet frm =
        liftM2 (&&)
          (isFTrueInWorld c lamType frm phi w)
          (isFTrueInWorld c lamType frm psi w)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (Or phi psi) w
    | w `S.member` wSet frm =
        liftM2 (||)
          (isFTrueInWorld c lamType frm phi w)
          (isFTrueInWorld c lamType frm psi w)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (Imp phi psi) w
    | w `S.member` wSet frm =
        isFTrueInWorld c lamType frm (Or (Not phi) psi) w
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (Box phi) w
    | w `S.member` wSet frm = do
        let trgs = targetsOf' (S.toList (accRel frm)) w
        liftM and (mapM (isFTrueInWorld c lamType frm phi) trgs)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (Diamond phi) w
    | w `S.member` wSet frm = do
        let trgs = targetsOf' (S.toList (accRel frm)) w
        liftM or (mapM (isFTrueInWorld c lamType frm phi) trgs)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorlds c lamType frm fml ws
    | ws `S.isSubsetOf` wSet frm =
        liftM and (mapM (isFTrueInWorld c lamType frm fml) (S.toList ws))
    | otherwise             = error "isFTrueInWorlds: not all worlds in frame"

  isFUniversallyTrue c lamType frm@(Frame w _) fml = do
    sw <- satFWorlds c lamType frm fml
    return (S.fromList sw == w)

instance AsLambdaType PLFml where
    fmlAsLambdaType c lamType w (PLVar v) = do
        v' <- termAsLamType c lamType w v
        return (PLVar v')
    fmlAsLambdaType c lamType w (PLNot phi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        return (PLNot phi')
    fmlAsLambdaType c lamType w (PLAnd phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (PLAnd phi' psi')
    fmlAsLambdaType c lamType w (PLOr phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (PLOr phi' psi')
    fmlAsLambdaType c lamType w (PLImp phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (PLImp phi' psi')

instance AsLambdaType Fml where
    fmlAsLambdaType c lamType w (Var v) = do
        v' <- termAsLamType c lamType w v
        return (Var v')
    fmlAsLambdaType c lamType w (Not phi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        return (Not phi')
    fmlAsLambdaType c lamType w (And phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (And phi' psi')
    fmlAsLambdaType c lamType w (Or phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (Or phi' psi')
    fmlAsLambdaType c lamType w (Imp phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (Imp phi' psi')
    fmlAsLambdaType c lamType w (Box phi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        return (Box phi')
    fmlAsLambdaType c lamType w (Diamond phi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        return (Diamond phi')

instance Show PLFml where
    show (PLVar x)         = show x
    show (PLNot (PLVar x)) = "!(" ++ show (PLVar x) ++ ")"
    show (PLNot x)       = '!' : show x
    show (PLAnd x y)     = "(" ++ show x ++ " & " ++ show y ++ ")"
    show (PLOr x y)      = "(" ++ show x ++ " | " ++ show y ++ ")"
    show (PLImp x y)     = "(" ++ show x ++ " -> " ++ show y ++ ")"

instance Show Fml where
    show (Var x)         = show x
    show (Not (Var x)) = "!(" ++ show x ++ ")"
    show (Not x)         = '!' : show x
    show (And x y)       = "(" ++ show x ++ " & " ++ show y ++ ")"
    show (Or x y)        = "(" ++ show x ++ " | " ++ show y ++ ")"
    show (Imp x y)       = "(" ++ show x ++ " -> " ++ show y ++ ")"
    show (Box x)           = "[]" ++ show x
    show (Diamond x)       = "<>" ++ show x

type Predicate = Connection -> LambdaType -> T.Text -> IO Bool

data Quantor = All { quanFmls :: [T.Text],  predicate :: Predicate }
             | Ex  { quanFmls :: [T.Text],  predicate :: Predicate }
     
instance Eval2Bool Quantor where
    eval2B c lamType (All xs prd) =
      liftM and (mapM (prd c lamType) xs)

    eval2B c lamType (Ex xs prd) =
      liftM or (mapM (prd c lamType) xs)

--------------------------------------------------------------------------------
-- parsing related functions

instance Read PLFml where
    readsPrec _ s = [(parsePLFml s, "")]

instance Read Fml where
    readsPrec _ s = [(parseFml s, "")]

parsePLFml :: String -> PLFml
parsePLFml xs
    | xs == ""                     = error "parsePLFml: malformed expression"
    | not (balancedParentheses xs) = error "parsePLFml: missing parenthesis"
    | otherwise                    = parsePLFml' (init (trim xs))

-- |Helper function for parsePLFml.
parsePLFml' :: String -> PLFml
parsePLFml' s =
    case s of
      '(':'V':'a':'r':' ':xs -> PLVar (T.pack (trim xs))
      '(':'N':'o':'t':xs     -> PLNot (parsePLFml xs)
      '(':'A':'n':'d':xs     -> let (p, q) = parseBiOpParms (trim xs)
                                in  PLAnd (parsePLFml p) (parsePLFml q)
      '(':'O':'r':xs         -> let (p, q) = parseBiOpParms (trim xs)
                                in  PLOr (parsePLFml p) (parsePLFml q)
      '(':'I':'m':'p':xs     -> let (p, q) = parseBiOpParms (trim xs)
                                in  PLImp (parsePLFml p) (parsePLFml q)
      _                      -> error "parsePLFml: malformed expression"

parseFml :: String -> Fml
parseFml xs
    | xs == ""                     = error "parseFml: malformed expression"
    | not (balancedParentheses xs) = error "parseFml: missing parenthesis"
    | otherwise                    = parseFml' (init (trim xs))

-- |Helper function for parseFml
parseFml' :: String -> Fml
parseFml' s =
    case s of
      '(':'V':'a':'r':' ':xs -> Var (T.pack (trim xs))
      '(':'N':'o':'t':xs     -> Not (parseFml xs)
      '(':'A':'n':'d':xs     -> let (p, q) = parseBiOpParms (trim xs)
                                in  And (parseFml p) (parseFml q)
      '(':'O':'r':xs         -> let (p, q) = parseBiOpParms (trim xs)
                                in  Or (parseFml p) (parseFml q)
      '(':'I':'m':'p':xs     -> let (p, q) = parseBiOpParms (trim xs)
                                in  Imp (parseFml p) (parseFml q)
      '(':'[':']':xs         -> Box (parseFml xs)
      '(':'<':'>':xs         -> Diamond (parseFml xs)
      _                      -> error "parseFml: malformed expression"

-- |True if String has an equal number of opening and closing parentheses
balancedParentheses :: String -> Bool
balancedParentheses s = length (elemIndices '(' s) == length (elemIndices ')' s)

-- |Parse a String with parameters for a binary operator: "(x) (y)" -> (x, y)
parseBiOpParms :: String -> (String, String)
parseBiOpParms xs =
    let
      x = takeTillParenBalanced xs 0 0
      y = dropWhile (== ' ') (drop (length x) xs)
    in
      (x, y)

-- |Read as much as needed for an expression with balanced parentheses 
takeTillParenBalanced :: String -> Int -> Int -> String
takeTillParenBalanced ('(':s) o c
    | o == c && o /= 0  = ""
    | otherwise         = '(' : takeTillParenBalanced s (succ o) c
takeTillParenBalanced (')':s) o c = ')' : takeTillParenBalanced s o (succ c)
takeTillParenBalanced ""      o c =
    if o == c
      then ""
      else error "missing parenthesis"
takeTillParenBalanced (s:ss)  o c =
    if o == c
      then ""
      else s : takeTillParenBalanced ss o c

--------------------------------------------------------------------------------
-- formula schemes

-- |Lambda sets of all worlds as list of conjunctions.
lambdaAnded :: Connection -> LambdaType -> IO [Fml]
lambdaAnded c lamType = do
    ws   <- worldsInLambda c lamType
    fmls <- mapM (worldsLambdaCombined c lamType And) ws
    return (catMaybes fmls)

-- |Lambda sets of all worlds as list of disjunctions.
lambdaOred :: Connection -> LambdaType -> IO [Fml]
lambdaOred c lamType = do
    ws   <- worldsInLambda c lamType
    fmls <- mapM (worldsLambdaCombined c lamType Or) ws
    return (catMaybes fmls)

-- |Lambda sets of all worlds as list of negated conjunctions.
lambdaAndedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaAndedNegated c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Not andedFmls)

-- |Lambda sets of all worlds as lists of negated disjunctions.
lambdaOredNegated :: Connection -> LambdaType -> IO [Fml]
lambdaOredNegated c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Not oredFmls)

-- |Lambda sets of all worlds as lists of diamond conjunctions.
lambdaAndedDiamonded :: Connection -> LambdaType -> IO [Fml]
lambdaAndedDiamonded c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Diamond andedFmls)

-- |Lambda sets of all worlds as lists of diamond disjunctions.
lambdaOredDiamonded :: Connection -> LambdaType -> IO [Fml]
lambdaOredDiamonded c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Diamond oredFmls)

-- |Lambda sets of all worlds as lists of negated diamond conjunctions.
lambdaAndedDiamondedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaAndedDiamondedNegated c lamType = do
    andedDiamondedFmls <- lambdaAndedDiamonded c lamType
    return (map Not andedDiamondedFmls)

-- |Lambda sets of all worlds as lists of negated diamond disjunctions.
lambdaOredDiamondedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaOredDiamondedNegated c lamType = do
    oredDiamondedFmls <- lambdaOredDiamonded c lamType
    return (map Not oredDiamondedFmls)

-- |Lambda sets of all worlds as lists of diamond conjunctions.
lambdaAndedBoxed :: Connection -> LambdaType -> IO [Fml]
lambdaAndedBoxed c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Box andedFmls)

-- |Lambda sets of all worlds as lists of diamond disjunctions.
lambdaOredBoxed :: Connection -> LambdaType -> IO [Fml]
lambdaOredBoxed c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Box oredFmls)

-- |Lambda sets of all worlds as lists of negated diamond conjunctions.
lambdaAndedBoxedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaAndedBoxedNegated c lamType = do
    andedBoxedFmls <- lambdaAndedBoxed c lamType
    return (map Not andedBoxedFmls)

-- |Lambda sets of all worlds as lists of negated diamond disjunctions.
lambdaOredBoxedNegated :: Connection -> LambdaType -> IO [Fml]
lambdaOredBoxedNegated c lamType = do
    oredBoxedFmls <- lambdaOredBoxed c lamType
    return (map Not oredBoxedFmls)

-- |Lambda formulas of a single world as one disjunction.
worldsLambdaCombined :: Connection -> LambdaType -> (Fml -> Fml -> Fml) ->
                        T.Text -> IO (Maybe Fml)
worldsLambdaCombined c lamType j w = do
    fmls <- worldFormulas c lamType w
    return (formulasToJunction j fmls)
    
-- |Convert a T.Text list into a PL (dis/kon)junction.
formulasToJunction :: (Fml -> Fml -> Fml) -> [T.Text] -> Maybe Fml
formulasToJunction _ []     = Nothing
formulasToJunction j (f:fs) =
    let
      accu = Var f
      fmls = map Var fs
    in
      Just (foldl j accu fmls)

