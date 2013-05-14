{-# LANGUAGE OverloadedStrings #-}

module LogicSearch
( Fml (..)
, MLFml (..)
, Quantor (..)
, eval2B
, satWorlds
, parseFml
) where

import Control.Monad (filterM, liftM, liftM2)
import Data.List ((\\), elemIndices, intersect, union)
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import DB
import KripkeTypes
import Model
import Util

-- |Check if x is satisfied in a given world.
class SatInWorld x where
    isSatInWorld :: Connection -> LambdaType -> x -> T.Text -> IO Bool

-- |Worlds satisfying x.
class SatWorlds x where
    satWorlds :: Connection -> LambdaType -> x -> IO [T.Text]

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
    filterM (isSatInWorld c lamType (Box phi)) allWorlds

  satWorlds c lamType (Diamond phi) = do
    allWorlds <- worldsInLambda c lamType
    filterM (isSatInWorld c lamType (Diamond phi)) allWorlds

instance SatInWorld MLFml where
  isSatInWorld c lamType (MLVar phi) w = do
    phi' <- termAsLamType c lamType (Just w) phi
    fmls <- lambda c lamType w
    return (phi' `elem` fmls)

  isSatInWorld c lamType (MLNot phi) w =
    liftM not (isSatInWorld c lamType phi w)

  isSatInWorld c lamType (MLAnd phi psi) w =
    liftM and
      (sequence [isSatInWorld c lamType phi w, isSatInWorld c lamType psi w])

  isSatInWorld c lamType (MLOr phi psi) w =
    liftM or
      (sequence [isSatInWorld c lamType phi w, isSatInWorld c lamType psi w])

  isSatInWorld c lamType (MLImp phi psi) w =
    isSatInWorld c lamType (MLOr (MLNot phi) psi) w

  isSatInWorld c lamType (Box phi) w = do
    tgs <- targetsOf c w
    liftM and (mapM (isSatInWorld c lamType phi) tgs)
      
  isSatInWorld c lamType (Diamond phi) w = do
    tgs <- targetsOf c w
    liftM or (mapM (isSatInWorld c lamType phi) tgs)
 
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
