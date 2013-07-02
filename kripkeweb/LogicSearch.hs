{-# LANGUAGE OverloadedStrings #-}

module LogicSearch
( AsLambdaType
, PLFml (..)
, MLFml (..)
, PTrueIn
, Quantor (..)
, TrueIn
, eval2B
, fmlAsLambdaType
, isFTrueInWorld
, isFTrueInWorlds
, isFUniversallyTrue
, isPTrueInWorld
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
, parsePLFml
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
class PTrueIn x where
    isPTrueInWorld :: Model -> T.Text -> x -> Bool

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
data MLFml = MLVar T.Text
           | MLNot MLFml
           | MLAnd MLFml MLFml
           | MLOr  MLFml MLFml
           | MLImp MLFml MLFml
           | Box MLFml
           | Diamond MLFml

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

instance SatWorlds MLFml where
  satWorlds c lamType (MLVar phi) = do
    phi' <- termAsLamType c lamType Nothing phi
    worldsWithFormula c lamType phi'

  satWorlds c lamType (MLNot phi) =
    liftM2 (\\) (worldsInLambda c lamType) (satWorlds c lamType phi)

  satWorlds c lamType (MLAnd phi psi) =
    liftM2 intersect (satWorlds c lamType phi) (satWorlds c lamType psi)

  satWorlds c lamType (MLOr phi psi) =
    liftM2 union (satWorlds c lamType phi) (satWorlds c lamType psi)

  satWorlds c lamType (MLImp phi psi) =
    satWorlds c lamType (MLOr (MLNot phi) psi)

  satWorlds c lamType (Box phi) = do
    allWorlds <- worldsInLambda c lamType
    filterM (isTrueInWorld c lamType (Box phi)) allWorlds

  satWorlds c lamType (Diamond phi) = do
    allWorlds <- worldsInLambda c lamType
    filterM (isTrueInWorld c lamType (Diamond phi)) allWorlds

instance FSatWorlds MLFml where
  satFWorlds c lamType (Frame w _) (MLVar phi) = do
    phi' <- termAsLamType c lamType Nothing phi
    liftM (S.toList w `intersect`) (worldsWithFormula c lamType phi')

  satFWorlds c lamType frm@(Frame w _) (MLNot phi) =
    liftM (S.toList w \\) (satFWorlds c lamType frm phi)

  satFWorlds c lamType frm@(Frame w _) (MLAnd phi psi) = do
    dbWorlds <- liftM2 intersect
                  (satFWorlds c lamType frm phi) (satFWorlds c lamType frm psi)
    return (S.toList w `intersect` dbWorlds)

  satFWorlds c lamType frm@(Frame w _) (MLOr phi psi) = do
    dbWorlds <- liftM2 union
                  (satFWorlds c lamType frm phi) (satFWorlds c lamType frm psi)
    return (S.toList w `intersect` dbWorlds)

  satFWorlds c lamType frm (MLImp phi psi) =
    satFWorlds c lamType frm (MLOr (MLNot phi) psi)

  satFWorlds c lamType frm@(Frame w _) (Box phi) =
    filterM (isFTrueInWorld c lamType frm (Box phi)) (S.toList w)

  satFWorlds c lamType frm@(Frame w _) (Diamond phi) =
    filterM (isFTrueInWorld c lamType frm (Diamond phi)) (S.toList w)

instance TrueIn PLFml where
  isTrueInWorld c lamType (PLVar phi) w = isTrueInWorld c lamType (MLVar phi) w

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

instance PTrueIn PLFml where
  isPTrueInWorld (Model _ lam) w (PLVar phi) = phi `elem` lam w

  isPTrueInWorld mdl w (PLNot phi) = not (isPTrueInWorld mdl w phi)

  isPTrueInWorld mdl w (PLAnd phi psi) =
    isPTrueInWorld mdl w phi && isPTrueInWorld mdl w psi

  isPTrueInWorld mdl w (PLOr phi psi) =
    isPTrueInWorld mdl w phi || isPTrueInWorld mdl w psi

  isPTrueInWorld mdl w (PLImp phi psi) =
    isPTrueInWorld mdl w (PLNot phi) || isPTrueInWorld mdl w psi

instance PTrueIn MLFml where
    isPTrueInWorld (Model _ lam) w (MLVar phi) = phi `elem` lam w

    isPTrueInWorld mdl w (MLNot phi) = not (isPTrueInWorld mdl w phi)

    isPTrueInWorld mdl w (MLAnd phi psi) =
      isPTrueInWorld mdl w phi && isPTrueInWorld mdl w psi

    isPTrueInWorld mdl w (MLOr phi psi) =
      isPTrueInWorld mdl w phi || isPTrueInWorld mdl w psi

    isPTrueInWorld mdl w (MLImp phi psi) =
      isPTrueInWorld mdl w (MLOr (MLNot phi) psi)

    isPTrueInWorld mdl@(Model (Frame _ ar) _) w (Box phi) =
      let tgs = targetsOf' (S.toList ar) w
      in  and [isPTrueInWorld mdl t phi | t <- tgs]

    isPTrueInWorld mdl@(Model (Frame _ ar) _) w (Diamond phi) =
      let tgs = targetsOf' (S.toList ar) w
      in  or [isPTrueInWorld mdl t phi | t <- tgs]

instance TrueIn MLFml where
  isTrueInWorld c lamType (MLVar phi) w = do
    phi' <- termAsLamType c lamType (Just w) phi
    fmls <- lambda c lamType w
    return (phi' `elem` fmls)

  isTrueInWorld c lamType (MLNot phi) w =
    liftM not (isTrueInWorld c lamType phi w)

  isTrueInWorld c lamType (MLAnd phi psi) w =
    liftM2 (&&) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (MLOr phi psi) w =
    liftM2 (||) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (MLImp phi psi) w =
    isTrueInWorld c lamType (MLOr (MLNot phi) psi) w

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

instance FTrueIn MLFml where
  isFTrueInWorld c lamType frm (MLVar phi) w
    | w `S.member` wSet frm = isTrueInWorld c lamType (MLVar phi) w
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (MLNot phi) w
    | w `S.member` wSet frm = liftM not (isFTrueInWorld c lamType frm phi w)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (MLAnd phi psi) w
    | w `S.member` wSet frm =
        liftM2 (&&)
          (isFTrueInWorld c lamType frm phi w)
          (isFTrueInWorld c lamType frm psi w)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (MLOr phi psi) w
    | w `S.member` wSet frm =
        liftM2 (||)
          (isFTrueInWorld c lamType frm phi w)
          (isFTrueInWorld c lamType frm psi w)
    | otherwise             = error "isFTrueInWorld: world not in frame"

  isFTrueInWorld c lamType frm (MLImp phi psi) w
    | w `S.member` wSet frm =
        isFTrueInWorld c lamType frm (MLOr (MLNot phi) psi) w
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

instance AsLambdaType MLFml where
    fmlAsLambdaType c lamType w (MLVar v) = do
        v' <- termAsLamType c lamType w v
        return (MLVar v')
    fmlAsLambdaType c lamType w (MLNot phi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        return (MLNot phi')
    fmlAsLambdaType c lamType w (MLAnd phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (MLAnd phi' psi')
    fmlAsLambdaType c lamType w (MLOr phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (MLOr phi' psi')
    fmlAsLambdaType c lamType w (MLImp phi psi) = do
        phi' <- fmlAsLambdaType c lamType w phi
        psi' <- fmlAsLambdaType c lamType w psi
        return (MLImp phi' psi')
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

instance Show MLFml where
    show (MLVar x)         = show x
    show (MLNot (MLVar x)) = "!(" ++ show x ++ ")"
    show (MLNot x)         = '!' : show x
    show (MLAnd x y)       = "(" ++ show x ++ " & " ++ show y ++ ")"
    show (MLOr x y)        = "(" ++ show x ++ " | " ++ show y ++ ")"
    show (MLImp x y)       = "(" ++ show x ++ " -> " ++ show y ++ ")"
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

instance Read MLFml where
    readsPrec _ s = [(parseMLFml s, "")]

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

parseMLFml :: String -> MLFml
parseMLFml xs
    | xs == ""                     = error "parseMLFml: malformed expression"
    | not (balancedParentheses xs) = error "parseMLFml: missing parenthesis"
    | otherwise                    = parseMLFml' (init (trim xs))

-- |Helper function for parseMLFml
parseMLFml' :: String -> MLFml
parseMLFml' s =
    case s of
      '(':'V':'a':'r':' ':xs -> MLVar (T.pack (trim xs))
      '(':'N':'o':'t':xs     -> MLNot (parseMLFml xs)
      '(':'A':'n':'d':xs     -> let (p, q) = parseBiOpParms (trim xs)
                                in  MLAnd (parseMLFml p) (parseMLFml q)
      '(':'O':'r':xs         -> let (p, q) = parseBiOpParms (trim xs)
                                in  MLOr (parseMLFml p) (parseMLFml q)
      '(':'I':'m':'p':xs     -> let (p, q) = parseBiOpParms (trim xs)
                                in  MLImp (parseMLFml p) (parseMLFml q)
      '(':'[':']':xs         -> Box (parseMLFml xs)
      '(':'<':'>':xs         -> Diamond (parseMLFml xs)
      _                      -> error "parseMLFml: malformed expression"

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
lambdaAnded :: Connection -> LambdaType -> IO [MLFml]
lambdaAnded c lamType = do
    ws   <- worldsInLambda c lamType
    fmls <- mapM (worldsLambdaCombined c lamType MLAnd) ws
    return (catMaybes fmls)

-- |Lambda sets of all worlds as list of disjunctions.
lambdaOred :: Connection -> LambdaType -> IO [MLFml]
lambdaOred c lamType = do
    ws   <- worldsInLambda c lamType
    fmls <- mapM (worldsLambdaCombined c lamType MLOr) ws
    return (catMaybes fmls)

-- |Lambda sets of all worlds as list of negated conjunctions.
lambdaAndedNegated :: Connection -> LambdaType -> IO [MLFml]
lambdaAndedNegated c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map MLNot andedFmls)

-- |Lambda sets of all worlds as lists of negated disjunctions.
lambdaOredNegated :: Connection -> LambdaType -> IO [MLFml]
lambdaOredNegated c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map MLNot oredFmls)

-- |Lambda sets of all worlds as lists of diamond conjunctions.
lambdaAndedDiamonded :: Connection -> LambdaType -> IO [MLFml]
lambdaAndedDiamonded c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Diamond andedFmls)

-- |Lambda sets of all worlds as lists of diamond disjunctions.
lambdaOredDiamonded :: Connection -> LambdaType -> IO [MLFml]
lambdaOredDiamonded c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Diamond oredFmls)

-- |Lambda sets of all worlds as lists of negated diamond conjunctions.
lambdaAndedDiamondedNegated :: Connection -> LambdaType -> IO [MLFml]
lambdaAndedDiamondedNegated c lamType = do
    andedDiamondedFmls <- lambdaAndedDiamonded c lamType
    return (map MLNot andedDiamondedFmls)

-- |Lambda sets of all worlds as lists of negated diamond disjunctions.
lambdaOredDiamondedNegated :: Connection -> LambdaType -> IO [MLFml]
lambdaOredDiamondedNegated c lamType = do
    oredDiamondedFmls <- lambdaOredDiamonded c lamType
    return (map MLNot oredDiamondedFmls)

-- |Lambda sets of all worlds as lists of diamond conjunctions.
lambdaAndedBoxed :: Connection -> LambdaType -> IO [MLFml]
lambdaAndedBoxed c lamType = do
    andedFmls <- lambdaAnded c lamType
    return (map Box andedFmls)

-- |Lambda sets of all worlds as lists of diamond disjunctions.
lambdaOredBoxed :: Connection -> LambdaType -> IO [MLFml]
lambdaOredBoxed c lamType = do
    oredFmls <- lambdaOred c lamType
    return (map Box oredFmls)

-- |Lambda sets of all worlds as lists of negated diamond conjunctions.
lambdaAndedBoxedNegated :: Connection -> LambdaType -> IO [MLFml]
lambdaAndedBoxedNegated c lamType = do
    andedBoxedFmls <- lambdaAndedBoxed c lamType
    return (map MLNot andedBoxedFmls)

-- |Lambda sets of all worlds as lists of negated diamond disjunctions.
lambdaOredBoxedNegated :: Connection -> LambdaType -> IO [MLFml]
lambdaOredBoxedNegated c lamType = do
    oredBoxedFmls <- lambdaOredBoxed c lamType
    return (map MLNot oredBoxedFmls)

-- |Lambda formulas of a single world as one disjunction.
worldsLambdaCombined :: Connection -> LambdaType -> (MLFml -> MLFml -> MLFml) ->
                        T.Text -> IO (Maybe MLFml)
worldsLambdaCombined c lamType j w = do
    fmls <- worldFormulas c lamType w
    return (formulasToJunction j fmls)
    
-- |Convert a T.Text list into a PL (dis/kon)junction.
formulasToJunction :: (MLFml -> MLFml -> MLFml) -> [T.Text] -> Maybe MLFml
formulasToJunction _ []     = Nothing
formulasToJunction j (f:fs) =
    let
      accu = MLVar f
      fmls = map MLVar fs
    in
      Just (foldl j accu fmls)

