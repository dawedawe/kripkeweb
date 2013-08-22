{-# LANGUAGE OverloadedStrings #-}

module KripkeTypes
( Frame (..)
, LambdaEntry (..)
, LambdaRels (..)
, LambdaType (..)
, Model (..)
, MyStemAlgo (..)
, OneToOne (..)
, OneToN (..)
, OneToNtuples (..)
, PageRankEntry (..)
, lambdaPure
, oToN2OneToOnes
, oToNtuples2LambdaEntry
, reduceModel
, toUnreflModel
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import NLP.Snowball (Algorithm (..))

import Relation (dropReflRels, dropRelsWithElemInS)
import Util (unquote)

-- |Type for a Kripke-Model.
data Model = Model { frame      :: Frame
                   , lambdaFunc :: T.Text -> [T.Text]
                   }

-- |Type for a Kripke-Frame.
data Frame = Frame { wSet   :: S.Set T.Text
                   , accRel :: S.Set (T.Text, T.Text)
                   } deriving (Eq, Ord, Show)

-- |Type to hold all versions of postprocessed lambda data of a world.
data LambdaRels = LambdaRels { mtaRawLamRels   :: OneToNtuples
                             , mtaStemLamRels  :: OneToNtuples
                             , mtaSndexLamRels :: OneToNtuples
                             , bdyRawLamRels   :: OneToNtuples
                             , bdyStemLamRels  :: OneToNtuples
                             , bdySndexLamRels :: OneToNtuples
                             } deriving (Show)

-- |Type to distinguish the different types of lambda formula postprocessing.
data LambdaType = MtaRaw
                | MtaStem
                | MtaSoundex
                | BdyRaw
                | BdyStem
                | BdySoundex
                  deriving (Eq, Ord, Read, Show)

-- |Pure version of the lambda function.
lambdaPure :: M.Map T.Text [T.Text] -> T.Text -> [T.Text]
lambdaPure mp w = fromMaybe [] (M.lookup w mp)

-- |Drop the reflexive relations in the model.
toUnreflModel :: Model -> Model
toUnreflModel (Model (Frame w r) l) =
    let r' = S.fromList (dropReflRels (S.toList r))
    in  Model (Frame w r') l

-- |Reduce the model to the given set of worlds, drop relations with other
-- worlds.
reduceModel :: Model -> S.Set T.Text -> Model
reduceModel (Model (Frame ws rels) l) worlds2Keep =
    let rels' = dropRelsWithElemInS (ws S.\\ worlds2Keep) rels
    in  (Model (Frame worlds2Keep rels') l)

--------------------------------------------------------------------------------
-- Types for interaction with the database

-- |Type for 1-1 relations.
data OneToOne = OneToOne { xEntity :: T.Text
                         , yEntity :: T.Text
                         } deriving (Show)

instance FromRow OneToOne where
    fromRow = OneToOne <$> field <*> field

instance ToRow OneToOne where
    toRow (OneToOne x y) = [toField x, toField y]

-- |Type for entries in the lambda table.
data LambdaEntry = LambdaEntry { leWorld   :: T.Text
                               , leFormula :: T.Text
                               , leCount   :: Int
                               } deriving (Show)

instance FromRow LambdaEntry where
    fromRow = LambdaEntry <$> field <*> field <*> field

instance ToRow LambdaEntry where
    toRow (LambdaEntry w f c) = [toField w, toField f, toField c]

-- |Type for entries in the pagerank table.
data PageRankEntry = PageRankEntry { peWorld :: T.Text
                                   , peScore :: Double
                                   } deriving (Show)

instance FromRow PageRankEntry where
    fromRow = PageRankEntry <$> field <*> field

instance ToRow PageRankEntry where
    toRow (PageRankEntry w s) = [toField w, toField s]

-- |Type for 1-n relations.
data OneToN = OneToN { oneEntity :: T.Text
                     , nEntities :: S.Set T.Text
                     } deriving (Eq, Show)

instance Ord OneToN where
    compare (OneToN s1 t1) (OneToN s2 t2) = compare (s1,t1) (s2,t2)

-- |Type for 1-n relations with the right side being (T.Text, Int) tuples.
data OneToNtuples = OneToNtuples { dEntity :: T.Text
                                 , nTuples :: S.Set (T.Text, Int)
                                 } deriving (Eq, Show)

instance Ord OneToNtuples where
    compare (OneToNtuples s1 t1) (OneToNtuples s2 t2) = compare (s1,t1) (s2,t2)

-- |Convert an OneToN to a list of OneToOne.
oToN2OneToOnes :: OneToN -> [OneToOne]
oToN2OneToOnes (OneToN s ts) = [OneToOne s t | t <- S.toList ts]

-- |Convert an OneToNtuples to a list of LambdaEntry
oToNtuples2LambdaEntry :: OneToNtuples -> [LambdaEntry]
oToNtuples2LambdaEntry (OneToNtuples s ts) =
    [LambdaEntry s f c | (f,c) <- S.toList ts]

--------------------------------------------------------------------------------

-- |Wrapper for Algorithm to make it a non-orphan instance of Show, Read
newtype MyStemAlgo = MyStemAlgo { myStemAlgo :: Algorithm }

instance Show MyStemAlgo where
    show (MyStemAlgo  Danish)     = "Danish"
    show (MyStemAlgo  Dutch)      = "Dutch"
    show (MyStemAlgo  English)    = "English"
    show (MyStemAlgo  Finnish)    = "Finnish"
    show (MyStemAlgo  French)     = "French"
    show (MyStemAlgo  German)     = "German"
    show (MyStemAlgo  Hungarian)  = "Hungarian"
    show (MyStemAlgo  Italian)    = "Italian"
    show (MyStemAlgo  Norwegian)  = "Norwegian"
    show (MyStemAlgo  Portuguese) = "Portuguese"
    show (MyStemAlgo  Romanian)   = "Romanian"
    show (MyStemAlgo  Russian)    = "Russian"
    show (MyStemAlgo  Spanish)    = "Spanish"
    show (MyStemAlgo  Swedish)    = "Swedish"
    show (MyStemAlgo  Turkish)    = "Turkish"
    show (MyStemAlgo  Porter)     = "Porter"

instance Read MyStemAlgo where
    readsPrec _ s = case unquote s of
                      "Danish"     -> [(MyStemAlgo Danish, "")]
                      "Dutch"      -> [(MyStemAlgo Dutch, "")]
                      "English"    -> [(MyStemAlgo English, "")]
                      "Finnish"    -> [(MyStemAlgo Finnish, "")]
                      "French"     -> [(MyStemAlgo French, "")]
                      "German"     -> [(MyStemAlgo German, "")]
                      "Hungarian"  -> [(MyStemAlgo Hungarian, "")]
                      "Italian"    -> [(MyStemAlgo Italian, "")]
                      "Norwegian"  -> [(MyStemAlgo Norwegian, "")]
                      "Portuguese" -> [(MyStemAlgo Portuguese, "")]
                      "Romanian"   -> [(MyStemAlgo Romanian, "")]
                      "Russian"    -> [(MyStemAlgo Russian, "")]
                      "Spanish"    -> [(MyStemAlgo Spanish, "")]
                      "Swedish"    -> [(MyStemAlgo Swedish, "")]
                      "Turkish"    -> [(MyStemAlgo Turkish, "")]
                      "Porter"     -> [(MyStemAlgo Porter, "")]
                      _            -> []

