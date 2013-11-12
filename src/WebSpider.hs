{-# LANGUAGE OverloadedStrings #-}

module WebSpider
( accessabilitySet
, getDomainAsText
, getLambdaRels
, spider
, spiderHO
, spiderRelHO
, relAccessabilitySet
) where

import Control.Arrow ((&&&))
import Data.Foldable (foldlM)
import Data.List (group, isPrefixOf, sort)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S
import qualified Data.Text as T (Text, append, init, isSuffixOf, pack, unpack)
import Network.Curl.Opts
import Network.Shpider
import NLP.Snowball
import Text.HTML.TagSoup
import Text.PhoneticCode.Soundex (soundexNARA)

import Conf (Proxy)
import KripkeTypes
import Util (isAbsoluteHttpUrl, isSoundExHash)
import WebParser

-- |Wrapper for Link to make it a non-orphan instance of Ord, needed for Set.
newtype MyLink = MyLink { myLink :: Link }
                          deriving (Eq, Show)

instance Ord MyLink where
    compare (MyLink (Link a1 _)) (MyLink (Link a2 _)) = compare a1 a2

-- |Get all hosts that are reachable from the given url.
-- The oneEntity of the result is also normalised to the host part of the url.
accessabilitySet :: Maybe Proxy -> T.Text -> IO OneToN
accessabilitySet prx u = do
    lnks <- getAllAbsLinkUrls prx (fromJust (importURL (T.unpack u)))
    return $ OneToN (getDomainAsText u) (S.map T.pack (urlsToHostStrings lnks))

-- |Get all relative links that are reachable from the given url as absolute
-- URLs.
relAccessabilitySet :: Maybe Proxy -> T.Text -> IO OneToN
relAccessabilitySet prx u = do
    lnks <- getAllRelLinkUrls prx (fromJust (importURL (T.unpack u)))
    return $ OneToN u (S.map (T.pack . exportURL) lnks)

-- |Recursivly follow the accessabilitySet to a given depth. Returns Set of all
-- accessability Sets.
spider :: Maybe Proxy -> Int -> (S.Set T.Text, S.Set OneToN) -> T.Text ->
          IO (S.Set T.Text, S.Set OneToN)
spider prx i (v, aSets) u
    | transparentUrls dom `S.intersection` v /= S.empty = do
        putStrLn ("already visited " ++ T.unpack dom)
        return (v, aSets)
    | i == 1                                            = do
        as <- accessabilitySet prx u
        return (S.insert dom v, S.insert as aSets)
    | i > 1            = do
        as         <- accessabilitySet prx u
        let v'     = S.insert dom v
        let aSets' = S.insert as aSets
        foldlM (spider prx (pred i)) (v', aSets') (S.toList (nEntities as))
    | otherwise                                         =
        error "spider: undefined arguments"
    where
      dom = getDomainAsText u

-- |Higher Order version of spider. Apply given function to every
-- accessabilitySet while spidering. Return Set of visited domains.
-- Don't visit any part of a domain a second time.
spiderHO :: Maybe Proxy -> Int -> (OneToN -> IO ()) -> S.Set T.Text -> T.Text ->
            IO (S.Set T.Text)
spiderHO prx i f v u
    | transparentUrls dom `S.intersection` v /= S.empty = do
        _ <- putStrLn ("already visited " ++ T.unpack dom)
        return v
    | i >= 1                                            = do
        _      <- putStrLn ("spiderHO: i = " ++ show i ++ " f on " ++
                    T.unpack u)
        as     <- accessabilitySet prx u
        _      <- f as
        let v' = S.insert dom v
        if i == 1
          then return v'
          else foldlM (spiderHO prx (pred i) f) v' (S.toList (nEntities as))
    | otherwise                                         =
        error "spiderHO: undefined arguments"
    where
      dom = getDomainAsText u

-- |Higher Order version of spider. Apply given function to every
-- accessabilitySet while spidering. Return Set of visited subsites.
-- Stay only within one domain.
spiderRelHO :: Maybe Proxy -> Int -> (OneToN -> IO ()) -> S.Set T.Text ->
               T.Text -> IO (S.Set T.Text)
spiderRelHO prx i f v u
    | transparentUrls u `S.intersection` v /= S.empty = do
        _ <- putStrLn ("already visited " ++ T.unpack u)
        return v
    | i >= 1                                          = do
        _      <- putStrLn ("spiderRelHO: i = " ++ show i ++ " f on " ++
                    T.unpack u)
        as     <- relAccessabilitySet prx u
        _      <- f as
        let v' = S.insert u v
        if i == 1
          then return v'
          else foldlM (spiderRelHO prx (pred i) f) v' (S.toList (nEntities as))
    | otherwise                                       =
        error "spiderRelHO: undefined arguments"

-- |Wrapper for getDomain to deal with Text.
getDomainAsText :: T.Text -> T.Text
getDomainAsText = T.pack . getDomain . T.unpack

-- |We consider x and x/ to be the same domain.
transparentUrls :: T.Text -> S.Set T.Text
transparentUrls u
    | "/" `T.isSuffixOf` u = S.fromList [u, T.init u]
    | otherwise            = S.fromList [u, u `T.append` "/"]

-- |Get all absolute HTTP Links from page except duplicates and invalid ones.
getAllAbsLinkUrls :: Maybe Proxy -> URL -> IO (S.Set URL)
getAllAbsLinkUrls prx u@(URL (Absolute _) _ _) = do
      allLnks     <- getAllLinksRaw prx (exportURL u)
      let valUrls = filterAbsHttpLinks allLnks
      return valUrls
getAllAbsLinkUrls _ _                          =
    error "getAllAbsLinkUrls: undefined arguments"

-- |Get all relative HTTP Links from page except duplicates and invalid ones.
getAllRelLinkUrls :: Maybe Proxy -> URL -> IO (S.Set URL)
getAllRelLinkUrls prx u@(URL (Absolute h) _ _) = do
      allLnks     <- getAllLinksRaw prx (exportURL u)
      let relLnks = S.filter (isRelLink (exportHost h)) allLnks
      let valUrls = filterAbsHttpLinks relLnks
      return valUrls
getAllRelLinkUrls _ _                          =
    error "getAllRelLinkUrls: undefined arguments"

-- |True, if the given String is a Prefix of the linkAddress, False otherwise.
isRelLink :: String -> MyLink -> Bool
isRelLink u (MyLink (Link ladr _))
    | u `isPrefixOf` ladr = True
    | otherwise           = False 

-- |Get all Links from page without any filtering or validation.
getAllLinksRaw :: Maybe Proxy -> String -> IO (S.Set MyLink)
getAllLinksRaw prx url = do
      p        <- getPage prx url
      let myLs = map MyLink (links p)
      return (S.fromList myLs)

-- |Drop invalid, non HTTP, non Absolute Links from list by parsing them to an
-- URL.
filterAbsHttpLinks :: S.Set MyLink -> S.Set URL
filterAbsHttpLinks = S.filter isAbsoluteHttpUrl . validLinks2URLs

-- |Drop Link elements that don't parse to a valid URL element.
validLinks2URLs :: S.Set MyLink -> S.Set URL
validLinks2URLs = setMaybeaToSeta . S.map (importURL . linkAddress . myLink)

-- |Try to parse an URL to an Absolute Host.
urlToHost :: URL -> Maybe Host
urlToHost (URL (Absolute h) _ _) = Just h
urlToHost _                      = Nothing

-- |URLs to list of host Strings.
urlsToHostStrings :: S.Set URL -> S.Set String
urlsToHostStrings = S.map exportHost . setMaybeaToSeta . S.map urlToHost

-- |Convert a Set of Maybe a to a Set of a. Nothing is deleted.
setMaybeaToSeta :: Ord a => S.Set (Maybe a) -> S.Set a
setMaybeaToSeta = S.map fromJust . S.delete Nothing

-- |Just get the page at the given url.
getPage :: Maybe Proxy -> String -> IO Page
getPage prx url =
    runShpider $ do
      setCurlOpts [CurlTimeout 20,
                   CurlFollowLocation True]
      -- addCurlOpts [CurlUserAgent userAgent]
      when (isJust prx) $ do
        let prx' = fromJust prx
        addCurlOpts [CurlProxy (fst prx'),
                     CurlProxyPort (snd prx'),
                     CurlHttpProxyTunnel True]
      (_,p) <- download url
      return p

-- |UserAgent Strint to use if we want to pretend to be a mobile device to get
-- redirected to the mobile site versions.
userAgent :: String
userAgent = "Mozilla/5.0 (Linux; U; Android 2.3.3; de-de; \
    \HTC Desire Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 \
    \Mobile Safari/533.1"

--------------------------------------------------------------------------------
-- functions for the lambda relation

-- |Get all tags from the given url in canonicalized form.
getTags :: Maybe Proxy -> String -> IO [Tag String]
getTags prx url = getPage prx url >>= \p -> return ((canonicalizeTags . tags) p)

-- |Normalized and filttered mainraw/mainstemmed/mainsoundexed,
-- raw/stemmed/soundexed versions of formulas of an url.
getLambdaRels :: Maybe Proxy -> T.Text -> IO (LambdaRels, Maybe Algorithm)
getLambdaRels prx url = do
    tgs         <- getTags prx (T.unpack url)
    feedtgs     <- case parseFeedLink (T.unpack url) tgs of
                     Just u -> getTags prx u
                     _      -> return []
    let mtaFmls = parseMain (tgs ++ feedtgs)
    let bdyFmls = parseBody tgs
    return (constructLambdaRels url tgs mtaFmls bdyFmls)

-- |Construct lambda relations and information fit to store in the database.
constructLambdaRels :: T.Text -> [Tag String] -> [String] -> [String] ->
                       (LambdaRels, Maybe Algorithm)
constructLambdaRels url tgs mtaFmls bdyFmls =
    let
      stalgo = chooseStemAlgo (T.unpack url) tgs
      sf     = constructStemFunc stalgo
    -- main raw
      muFmls = addCount (map T.pack mtaFmls)
      mr1    = OneToNtuples url (S.fromList muFmls)
    -- main stemmed
      msFmls = addCount (map (sf . T.pack) mtaFmls)
      mr2    = OneToNtuples url (S.fromList msFmls)
    -- main soundexed
      meFmls = filter isSoundExHash (map soundexNARA mtaFmls)
      mr3'   = addCount (map T.pack meFmls)
      mr3    = OneToNtuples url (S.fromList mr3')

    -- body raw
      buFmls = addCount (map T.pack bdyFmls)
      br1    = OneToNtuples url (S.fromList buFmls)
    -- body stemmed
      bsFmls = addCount (map (sf . T.pack) bdyFmls)
      br2    = OneToNtuples url (S.fromList bsFmls)
    -- body soundexed
      beFmls = filter isSoundExHash (map soundexNARA bdyFmls)
      br3'   = addCount (map T.pack beFmls)
      br3    = OneToNtuples url (S.fromList br3')
    in
      (LambdaRels mr1 mr2 mr3 br1 br2 br3, stalgo)

-- |Add Count to the list of formulas.
addCount :: (Eq a, Ord a) => [a] -> [(a, Int)]
addCount xs = map (head &&& length) (group (sort xs))

-- |If Algorithm isn't Nothing give back a curried stem function otherwise id.
constructStemFunc :: Maybe Algorithm -> T.Text -> T.Text
constructStemFunc Nothing  = id
constructStemFunc (Just a) = stem a

-- |Heuristic to choose a Snowball stemming Algorithm.
-- First try to parse the HTML lang attribute,
-- if unsuccessfull map the tld to an algorithm.
chooseStemAlgo :: String -> [Tag String] -> Maybe Algorithm
chooseStemAlgo url tgs =
    let lng = parseLang tgs
    in  case lng of
          Just l  -> langAttr2StemAlgo l
          Nothing -> tld2StemAlgo (getDomain url)

--------------------------------------------------------------------------------
-- test helpers

-- |Test Host.
testHost :: Host
testHost = Host (HTTP False) "www.openbsd.org" Nothing

-- |Test URLType.
testUrlType :: URLType
testUrlType = Absolute testHost

-- |Test URL.
testUrl :: URL
testUrl = URL testUrlType "" []

