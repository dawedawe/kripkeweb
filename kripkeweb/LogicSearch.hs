{-# LANGUAGE OverloadedStrings #-}

module LogicSearch
( Fml (..)
, MLFml (..)
, PTrueIn
, Quantor (..)
, TrueIn
, eval2B
, isFTrueInWorld
, isFTrueInWorlds
, isFUniversallyTrue
, isPTrueInWorld
, isTrueInWorld
, isTrueInWorlds
, isUniversallyTrue
, satWorlds
, satFWorlds
, parseFml
) where

import Control.Monad (filterM, liftM, liftM2)
import Data.List ((\\), elemIndices, intersect, union)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import Model
import Util

-- |Check if x is true in ...
class TrueIn x where
    isTrueInWorld     :: Connection -> LambdaType -> x -> T.Text -> IO Bool
    isTrueInWorlds    :: Connection -> LambdaType -> x -> [T.Text] -> IO Bool
    isUniversallyTrue :: Connection -> LambdaType -> x -> IO Bool

-- |Like TrueIn but take the frame as an argument instead of out of the
-- database.
class FTrueIn x where
    isFTrueInWorld     :: Connection -> LambdaType -> Frame -> x -> T.Text ->
                          IO Bool
    isFTrueInWorlds    :: Connection -> LambdaType -> Frame -> x ->
                          S.Set T.Text -> IO Bool
    isFUniversallyTrue :: Connection -> LambdaType -> Frame -> x -> IO Bool

class PTrueIn x where
    isPTrueInWorld :: Model -> T.Text -> x -> Bool

-- |Worlds satisfying x.
class SatWorlds x where
    satWorlds :: Connection -> LambdaType -> x -> IO [T.Text]

-- |Worlds in frame satisfying x.
class FSatWorlds x where
    satFWorlds :: Connection -> LambdaType -> Frame -> x -> IO [T.Text]

class Eval2Bool x where
    eval2B :: Connection -> LambdaType -> x -> IO Bool

-- |Fml set of Propositional Logic
data Fml = Var T.Text
         | Not Fml
         | And Fml Fml
         | Or  Fml Fml
         | Imp Fml Fml

-- |Fml set of Modal Logic
data MLFml = MLVar T.Text
           | MLNot MLFml
           | MLAnd MLFml MLFml
           | MLOr  MLFml MLFml
           | MLImp MLFml MLFml
           | Box MLFml
           | Diamond MLFml

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

instance TrueIn Fml where
  isTrueInWorld c lamType (Var phi) w = isTrueInWorld c lamType (MLVar phi) w

  isTrueInWorld c lamType (Not phi) w =
    liftM not (isTrueInWorld c lamType phi w)

  isTrueInWorld c lamType (And phi psi) w =
    liftM2 (&&) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (Or phi psi) w =
    liftM2 (||) (isTrueInWorld c lamType phi w) (isTrueInWorld c lamType psi w)

  isTrueInWorld c lamType (Imp phi psi) w =
    isTrueInWorld c lamType (Or (Not phi) psi) w

  isTrueInWorlds c lamType fml ws =
    liftM and (mapM (isTrueInWorld c lamType fml) ws)

  isUniversallyTrue c lamType fml =
    liftM2 eqListElems (satWorlds c lamType fml) (worldsInLambda c lamType)

instance PTrueIn Fml where
  isPTrueInWorld (Model _ lam) w (Var phi) = phi `elem` lam w

  isPTrueInWorld mdl w (Not phi) = not (isPTrueInWorld mdl w phi)

  isPTrueInWorld mdl w (And phi psi) =
    isPTrueInWorld mdl w phi && isPTrueInWorld mdl w psi

  isPTrueInWorld mdl w (Or phi psi) =
    isPTrueInWorld mdl w phi || isPTrueInWorld mdl w psi

  isPTrueInWorld mdl w (Imp phi psi) =
    isPTrueInWorld mdl w (Not phi) || isPTrueInWorld mdl w psi

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

instance Show Fml where
    show (Var x)       = show x
    show (Not (Var x)) = "!(" ++ show (Var x) ++ ")"
    show (Not x)       = '!' : show x
    show (And x y)     = "(" ++ show x ++ " & " ++ show y ++ ")"
    show (Or x y)      = "(" ++ show x ++ " | " ++ show y ++ ")"
    show (Imp x y)     = "(" ++ show x ++ " -> " ++ show y ++ ")"

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

instance Read Fml where
    readsPrec _ s = [(parseFml s, "")]

instance Read MLFml where
    readsPrec _ s = [(parseMLFml s, "")]

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
      _                      -> error "parseFml: malformed expression"

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
