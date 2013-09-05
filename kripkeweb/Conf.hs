module Conf
( Conf (..)
, Flag (..)
, Options (..)
, Proxy
, buildConf
, confFilePath
, createDotDir
, defaultOptions
, getConfItems
, lookupConfItem
, parseArgv
, usage
) where

import Control.Monad (liftM, unless)
import qualified Data.ConfigFile as CF
import Data.Either.Utils (forceEither)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word
import Database.PostgreSQL.Simple
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         getAppUserDataDirectory)
import System.FilePath (pathSeparator)

import KripkeTypes

-- |Data type to hold the whole configuration.
data Conf = Conf { opts    :: Options
                 , proxy   :: Maybe Proxy
                 , conInfo :: ConnectInfo
                 }

-- |Data type to hold the command line options.
data Options = Options { optUrl      :: T.Text
                       , optRecDepth :: Int
                       , optPgIters  :: Int
                       , optFml      :: T.Text
                       , optFlags    :: S.Set Flag
                       , optWords    :: [T.Text]
                       , optLamType  :: LambdaType
                       }

-- |Enumeration of command line flags.
data Flag = BuildR              -- build accessability relation R
          | BuildRelativeR      -- build relative accessability relation R
          | PLFmlEval           -- evaluate propositional logic formula
          | PMLFmlEval          -- evaluate modal logic formula
          | BuildLambdaRel      -- build the lambda relation
          | LambdaAccum         -- group and accumulate Lambda formulas
          | InLinkRings
          | OutLinkRings
          | CalcPageRank        -- calculate and store all pagerank scores
          | CalcTfidf           -- calculate and store all tfidf scores
          | RSetLamdaPL
          | RSetLamdaList
          | RSetWorldsLamda
          | RSetInLinks
          | RSetWsInLinks
          | RSetOutLinks
          | RSetWsOutLinks
          | TfidfWorld
          | TfidfSearch
          | UseLambdaType       -- postprocessing type of Lambda data to use
            deriving (Eq, Ord, Show)

type Proxy = (String, Word32)

-- |Default command line options, used if none given.
defaultOptions :: Options
defaultOptions = Options { optUrl      = T.empty
                         , optRecDepth = 1
                         , optPgIters  = 10
                         , optFml      = T.empty
                         , optFlags    = S.empty
                         , optWords    = []
                         , optLamType  = BdyRaw
                         }

-- |String representation of the executable name.
progName :: String
progName = "kripkeweb"

-- |String representation of the configuration file name.
confFileName :: String
confFileName = progName ++ ".conf"

-- |Command line usage.
usage :: String
usage =
    "Usage: " ++ progName ++
    " [-R url [-d n]] [-f plfrm] [-g frm] [-l] [-p n] [-t url]"

-- |Definition of command line options.
options :: [OptDescr (Options -> Options)]
options = [
      Option "R" ["accrel"]
      (ReqArg (\u optns -> let f = optFlags optns
                           in  optns { optFlags = BuildR `S.insert` f,
                                       optUrl   = T.pack u }) "URL" )
      "start building accessability relation at url"
    , Option "" ["relaccrel"]
      (ReqArg (\u optns -> let f = optFlags optns
                           in  optns { optFlags = BuildRelativeR `S.insert` f,
                                       optUrl   = T.pack u }) "URL" )
      "start building relative accessability relation at url"
    , Option "d" ["depth"]
      (ReqArg (\d optns -> optns { optRecDepth = read d }) "DEPTH" )
      "spider to recursive depth"
    , Option "" ["lamaccum"]
      (NoArg (\optns -> let f = optFlags optns
                        in  optns { optFlags = LambdaAccum `S.insert` f }))
      "group and accumulate lambda formulas"
    , Option "l" ["lamrel"]
      (NoArg (\optns -> let f = optFlags optns
                        in  optns { optFlags = BuildLambdaRel `S.insert` f }))
      "build lambda relation"
    , Option "" ["tfidfcalc"]
      (NoArg (\optns -> let f = optFlags optns
                        in  optns { optFlags   = CalcTfidf `S.insert` f}))
      "calculate and store the tfidf scores"
    , Option "p" ["pagerankcalc"]
      (ReqArg (\i optns -> let f = optFlags optns
                           in  optns { optFlags   = CalcPageRank `S.insert` f,
                                       optPgIters = read i }) "ITERATONS")
      "calculate and store the pagerank score"
    , Option "t" ["tfidf"]
      (ReqArg (\u optns -> let f = optFlags optns
                           in  optns { optFlags = TfidfWorld `S.insert` f,
                                       optUrl   = T.pack u }) "URL" )
      "calculate the tfidf values for the content of a world"
    , Option "g" ["tfidfsearch"]
      (ReqArg (\w optns -> let f = optFlags optns
                           in  optns { optFlags = TfidfSearch `S.insert` f,
                                       optFml   = T.pack w }) "FORMULA" )
      "search for worlds with a lambda formula, sort the results by tf-idf"
    , Option "f" ["plformula"]
      (ReqArg (\fml optns -> let f = optFlags optns
                             in  optns { optFlags = PLFmlEval `S.insert` f,
                                         optFml   = T.pack fml }) "PLFORMULA" )
      "evaluate the PL formula"
    , Option "" ["pmlformula"]
      (ReqArg (\fml optns -> let f = optFlags optns
                             in  optns { optFlags = PMLFmlEval `S.insert` f,
                                         optFml   = T.pack fml }) "PMLFORMULA" )
      "evaluate the PML formula"
    , Option "" ["lambdatype"]
      (ReqArg (\t optns -> let f = optFlags optns
                           in  optns { optFlags   = UseLambdaType `S.insert` f,
                                       optLamType = read t }) "LAMBDATYPE" )
      "use raw/stem/soundex lambda data"
    , Option "" ["inlinkrings"]
      (ReqArg (\u optns -> let f = optFlags optns
                           in  optns { optFlags = InLinkRings `S.insert` f,
                                       optUrl   = T.pack u }) "URL" )
      "calculate in-link rings"
    , Option "" ["outlinkrings"]
      (ReqArg (\u optns -> let f = optFlags optns
                           in  optns { optFlags = OutLinkRings `S.insert` f,
                                       optUrl   = T.pack u }) "URL" )
      "calculate out-link rings"
    , Option "" ["rsetlampl"]
      (ReqArg (\fml optns -> let f = optFlags optns
                             in  optns { optFlags = RSetLamdaPL `S.insert` f,
                                         optFml   = T.pack fml }) "PLFORMULA" )
      "evaluate the PL formula to a rough set"
    , Option "" ["rsetlamlist"]
      (ReqArg (\ls optns -> let f = optFlags optns
                            in  optns { optFlags = RSetLamdaList `S.insert` f,
                                        optWords = T.words (T.pack ls) })
        "FORMULALIST" )
      "rough set for a concept in form of a list of formulas"
    , Option "" ["rsetworldlam"]
      (ReqArg (\w optns -> let f = optFlags optns
                           in  optns { optFlags = RSetWorldsLamda `S.insert` f,
                                       optUrl   = T.pack w }) "WORLD" )
      "rough set for a concept in form of a world's lambda set"
    , Option "" ["rsetilinklist"]
      (ReqArg (\ls optns -> let f = optFlags optns
                            in  optns { optFlags = RSetInLinks `S.insert` f,
                                        optWords = T.words (T.pack ls) })
        "WORLDIST" )
      "rough set for a concept in form of a list of link sources"
    , Option "" ["rsetworldsils"]
      (ReqArg (\w optns -> let f = optFlags optns
                           in  optns { optFlags = RSetWsInLinks `S.insert` f,
                                       optUrl   = T.pack w }) "WORLD" )
      "rough set for a concept in form of a world's in-links set"
    , Option "" ["rsetolinklist"]
      (ReqArg (\lst optns -> let f = optFlags optns
                             in  optns { optFlags = RSetOutLinks `S.insert` f,
                                         optWords = T.words (T.pack lst) })
        "WORLDIST" )
      "rough set for a concept in form of a list of link targets"
    , Option "" ["rsetworldsols"]
      (ReqArg (\w optns -> let f = optFlags optns
                           in  optns { optFlags = RSetWsOutLinks `S.insert` f,
                                       optUrl   = T.pack w }) "WORLD" )
      "rough set for a concept in form of a world's out-links set"
    ]

-- |Parse the cli argument vector.
parseArgv :: [String] -> IO (Options, [String]) 
parseArgv argv = do
    let opt = getOpt RequireOrder options argv
    case opt of
          (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
          (_,_,errs) -> ioError
            (userError (concat errs ++ usageInfo usage options))

-- |Build Conf out of cli options and config file definitions.
buildConf :: Options -> IO Conf
buildConf o = do
    items      <- confFilePath >>= getConfItems
    let prx    = getProxyConf items
    let conInf = getConnectionConf items
    return $ Conf o prx conInf

-- |Read config items out of config file.
getConfItems :: FilePath -> IO [(CF.OptionSpec, String)]
getConfItems path = do
    val    <- CF.readfile CF.emptyCP path
    let cp = forceEither val
    return $ forceEither $ CF.items cp "DEFAULT"

-- |If possible, construct a Proxy out of the config content.
getProxyConf :: [(CF.OptionSpec, String)] -> Maybe Proxy
getProxyConf items =
    let
      n  = lookup "proxyname" items
      p  = lookup "proxyport" items
    in
      if isNothing n || isNothing p
        then Nothing
        else Just (fromJust n, read (fromJust p))

-- |If possible, construct a ConnectInfo out of the config content, otherwise
-- the defaultConnectInfo is returned.
getConnectionConf :: [(CF.OptionSpec, String)] -> ConnectInfo
getConnectionConf items =
    let
      hst = lookup "dbhost" items
      prt = lookup "dbport" items
      usr = lookup "dbuser" items
      pwd = lookup "dbpassword" items
      dbn = lookup "dbname" items
    in
      if any isNothing [hst, prt, usr, pwd, dbn]
        then defaultConnectInfo
        else ConnectInfo (fromJust hst) (read (fromJust prt)) (fromJust usr)
               (fromJust pwd) (fromJust dbn)

-- |Lookup item in tuple list and return it's value.
lookupConfItem :: String -> [(CF.OptionSpec, String)] -> String
lookupConfItem itemName items = 
    let value = lookup itemName items
    in  checkConfItem value itemName

-- |Fail with error if ConfItem can't be parsed.
checkConfItem :: Maybe String -> String -> String
checkConfItem (Just s) _       = s
checkConfItem Nothing itemName = error $ "failed to parse " ++ itemName

-- |Create dotdir with a default config file.
createDotDir :: IO ()
createDotDir = do
    dDir   <- dotDirPath
    exists <- doesDirectoryExist dDir
    _      <- createDirectoryIfMissing (not exists) dDir
    let cPath = dDir ++ [pathSeparator] ++ confFileName
    unless exists $ writeFile cPath defaultConf

-- |Default path to application data directory aka the dotdir.
dotDirPath :: IO String
dotDirPath = getAppUserDataDirectory progName

-- |Default path to config file.
confFilePath :: IO String
confFilePath = liftM (++ pathSeparator : confFileName) dotDirPath

-- |Default configuration with proxy example.
defaultConf :: String
defaultConf =
    "# proxyname = 127.0.0.1\n" ++
    "# proxyport = 8118\n" ++
    "# dbhost = localhost\n" ++
    "# dbport = 5432\n" ++
    "# dbuser = postgres\n" ++
    "# dbpassword = \n" ++
    "# dbname = postgres"

