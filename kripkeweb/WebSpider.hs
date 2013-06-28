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
import Data.Char (isDigit)
import Data.Foldable (foldlM)
import Data.List (group, intersect, isPrefixOf, isSuffixOf, nub, sort)
import Data.List.Utils (replace)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as S
import qualified Data.Text as T (Text, append, init, isSuffixOf, pack, unpack)
import Network.Curl.Opts
import Network.Shpider
import NLP.Tokenize (tokenize)
import NLP.Snowball
import Text.HTML.TagSoup
import Text.PhoneticCode.Soundex (soundexNARA)

import Conf (Proxy)
import KripkeTypes
import Util (hasLetters, lowerString)

-- |Wrapper for Link to make it a non-orphan instance of Ord, needed for Set
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
    | i == 1           = do
        as <- accessabilitySet prx u
        return (S.insert dom v, S.insert as aSets)
    | i > 1            = do
        as <- accessabilitySet prx u
        let v'     =  S.insert dom v
        let aSets' =  S.insert as aSets
        foldlM (spider prx (pred i)) (v', aSets') (S.toList (nEntities as))
    | otherwise        = error "spider: undefined arguments"
    where
      dom = getDomainAsText u

-- |Higher Order version of spider. Apply given function to every
-- accessabilitySet while spidering. Return Set of visited domains.
-- Don't visit any part of a domain a second time.
spiderHO :: Maybe Proxy -> Int -> (OneToN -> IO ()) -> S.Set T.Text -> T.Text ->
            IO (S.Set T.Text)
spiderHO prx i f v u
    | transparentUrls dom `S.intersection` v /= S.empty = do
        putStrLn ("already visited " ++ T.unpack dom)
        return v
    | i >= 1           = do
        putStrLn ("spiderHO: i = " ++ show i ++ " f on " ++ T.unpack u)
        as <- accessabilitySet prx u
        f as
        let v' = S.insert dom v
        if i == 1
          then return v'
          else foldlM (spiderHO prx (pred i) f) v' (S.toList (nEntities as))
    | otherwise        = error "spiderHO: undefined arguments"
    where
      dom = getDomainAsText u

-- |Higher Order version of spider. Apply given function to every
-- accessabilitySet while spidering. Return Set of visited subsites.
-- Stay only within one domain.
spiderRelHO :: Maybe Proxy -> Int -> (OneToN -> IO ()) -> S.Set T.Text ->
               T.Text -> IO (S.Set T.Text)
spiderRelHO prx i f v u
    | transparentUrls u `S.intersection` v /= S.empty = do
        putStrLn ("already visited " ++ T.unpack u)
        return v
    | i >= 1           = do
        putStrLn ("spiderRelHO: i = " ++ show i ++ " f on " ++ T.unpack u)
        as <- relAccessabilitySet prx u
        f as
        let v' = S.insert u v
        if i == 1
          then return v'
          else foldlM (spiderRelHO prx (pred i) f) v' (S.toList (nEntities as))
    | otherwise        = error "spiderRelHO: undefined arguments"

-- |Wrapper for getDomain to deal with Text.
getDomainAsText :: T.Text -> T.Text
getDomainAsText = T.pack . getDomain . T.unpack

-- |We consider x and x/ to be the same domain.
transparentUrls :: T.Text -> S.Set T.Text
transparentUrls u
    | "/" `T.isSuffixOf` u = S.fromList [u, T.init u]
    | otherwise            = S.fromList [u, u `T.append` "/"]

testHost :: Host
testHost = Host (HTTP False) "www.openbsd.org" Nothing

testUrlType :: URLType
testUrlType = Absolute testHost

testUrl :: URL
testUrl = URL testUrlType "" []

-- |Get all absolute HTTP Links from page except duplicates and invalid ones.
getAllAbsLinkUrls :: Maybe Proxy -> URL -> IO (S.Set URL)
getAllAbsLinkUrls prx u@(URL (Absolute _) _ _) = do
      allLnks <- getAllLinksRaw prx (exportURL u)
      let valUrls = filterAbsHttpLinks allLnks
      return valUrls
getAllAbsLinkUrls _ _                          =
    error "getAllAbsLinkUrls: undefined arguments"

-- |Get all relative HTTP Links from page except duplicates and invalid ones.
getAllRelLinkUrls :: Maybe Proxy -> URL -> IO (S.Set URL)
getAllRelLinkUrls prx u@(URL (Absolute h) _ _) = do
      allLnks <- getAllLinksRaw prx (exportURL u)
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
      p <- getPage prx url
      let myLs = map MyLink (links p)
      return (S.fromList myLs)

-- |Drop invalid, non HTTP, non Absolute Links from list by parsing them to an
-- URL.
filterAbsHttpLinks :: S.Set MyLink -> S.Set URL
filterAbsHttpLinks = S.filter isAbsoluteHttpUrl . validLinks2URLs

-- |Drop Link elements that don't parse to a valid URL element.
validLinks2URLs :: S.Set MyLink -> S.Set URL
validLinks2URLs = setMaybeaToSeta . S.map (importURL . linkAddress . myLink)

-- |True, if URL is Absolute and the protocol is HTTP, otherwise False
isAbsoluteHttpUrl :: URL -> Bool
isAbsoluteHttpUrl (URL (Absolute (Host (HTTP _) _ _)) _ _) = True
isAbsoluteHttpUrl _                                        = False

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
      setCurlOpts [CurlTimeout 20, CurlFollowLocation True]
      when (isJust prx) $ do
        let prx' = fromJust prx
        addCurlOpts [CurlProxy (fst prx'), CurlProxyPort (snd prx'),
                     CurlHttpProxyTunnel True]
      (_,p) <- download url
      return p

--------------------------------------------------------------------------------
-- functions for the lambda relation

-- |Get all tags from the given url in canonicalized form.
getTags :: Maybe Proxy -> String -> IO [Tag String]
getTags prx url = getPage prx url >>= \p -> return ((canonicalizeTags . tags) p)

-- |Normalized and filttered metaraw/metastemmed/metasoundexed,
-- raw/stemmed/soundexed versions of formulas of an url.
getLambdaRels :: Maybe Proxy -> T.Text -> IO (LambdaRels, Maybe Algorithm)
getLambdaRels prx url = do
    tgs         <- getTags prx (T.unpack url)
    let mtaFmls = parseMeta tgs
    let bdyFmls = (filterFormulas . map lowerString .
                  tokenize . innerText . filterScript) tgs
    return (constructLambdaRels url tgs mtaFmls bdyFmls)

constructLambdaRels :: T.Text -> [Tag String] -> [String] -> [String] ->
                       (LambdaRels, Maybe Algorithm)
constructLambdaRels url tgs mtaFmls bdyFmls =
    let
    -- meta raw
      muFmls = addCount (map T.pack mtaFmls)
      mr1    = OneToNtuples url (S.fromList muFmls)
    -- meta stemmed
      sa     = chooseStemAlgo (T.unpack url) tgs
      sf     = constructStemFunc sa
      msFmls = addCount (map (sf . T.pack) mtaFmls)
      mr2    = OneToNtuples url (S.fromList msFmls)
    -- meta soundexed
      mefmls = filter isSoundExHash (map soundexNARA mtaFmls)
      mr3'   = addCount (map T.pack mefmls)
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
      (LambdaRels mr1 mr2 mr3 br1 br2 br3, sa)

-- |Add Count to the list of formulas.
addCount :: [T.Text] -> [(T.Text, Int)]
addCount xs =
    let gs = group (sort xs)
    in  map (head &&& length) gs

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

-- |Try to parse HTML lang attribute.
parseLang :: [Tag String] -> Maybe String
parseLang t@(TagOpen "html" _ : _) =
    let
      hl = fromAttrib "lang" (head t)
      xl = fromAttrib "xml:lang" (head t)
      l  = if null hl then xl else hl
    in  if null l then Nothing else Just l
parseLang (_:xs)                   = parseLang xs
parseLang []                       = Nothing

-- |Parse the meta description and keywords out of the tags.
parseMeta :: [Tag String] -> [String]
parseMeta tgs =
    let
      meta      = "meta" :: String
      descTags  = filter (\t ->
                    (t ~== TagOpen meta [("name", "description")]) ||
                    (t ~== TagOpen meta [("name", "Description")]) ||
                    (t ~== TagOpen meta [("name", "DESCRIPTION")]) ||
                    (t ~== TagOpen meta [("http-equiv", "description")]) ||
                    (t ~== TagOpen meta [("http-equiv", "Description")]) ||
                    (t ~== TagOpen meta [("http-equiv", "DESCRIPTION")])) tgs
      keywTags  = filter (\t ->
                    (t ~== TagOpen meta [("name", "keywords")]) ||
                    (t ~== TagOpen meta [("name", "Keywords")]) ||
                    (t ~== TagOpen meta [("name", "KEYWORDS")]) ||
                    (t ~== TagOpen meta [("http-equiv", "keywords")]) ||
                    (t ~== TagOpen meta [("http-equiv", "Keywords")]) ||
                    (t ~== TagOpen meta [("http-equiv", "KEYWORDS")])) tgs
      desc      = getTagContent descTags
      keyw      = getTagContent keywTags
      metaWords = words (replace "," " " (desc ++ " " ++ keyw))
    in
      (nub . filterFormulas . map lowerString . concatMap tokenize) metaWords

-- |Get the content attribute value ouf of the first tag in the given list if
-- it's there.
getTagContent :: [Tag String] -> String
getTagContent []    = ""
getTagContent (x:_) = fromAttrib "content" x

-- |Parse the webpage title ouf of the <title> tag.
parseTitle :: [Tag String] -> Maybe String
parseTitle tgs =
    let tTag  = sections (~== ("<title>" :: String)) tgs
    in  if tTag /= []
          then maybeTagText ((tTag !! 0) !! 1)
          else Nothing 

-- |HTML lang attribute to Snowball stemming Algorithm.
langAttr2StemAlgo :: String -> Maybe Algorithm
langAttr2StemAlgo lng =
    let lng' = takeWhile (/= '-') lng
    -- besides "en" the lang attribute is pretty much equal to the tld
    in  if lng' == "en"
          then Just English
          else tld2StemAlgo ('.' : lng')

-- |TLD to Snowball stemming Algorithm.
tld2StemAlgo :: String -> Maybe Algorithm
tld2StemAlgo url
    | ".com" `isSuffixOf` url = Just English
    | ".org" `isSuffixOf` url = Just English
    | ".de" `isSuffixOf` url  = Just German
    | ".at" `isSuffixOf` url  = Just German
    | ".dk" `isSuffixOf` url  = Just Danish
    | ".nl" `isSuffixOf` url  = Just Dutch
    | ".uk" `isSuffixOf` url  = Just English
    | ".au" `isSuffixOf` url  = Just English
    | ".us" `isSuffixOf` url  = Just English
    | ".gov" `isSuffixOf` url = Just English
    | ".mil" `isSuffixOf` url = Just English
    | ".fi" `isSuffixOf` url  = Just Finnish
    | ".fr" `isSuffixOf` url  = Just French
    | ".hu" `isSuffixOf` url  = Just Hungarian
    | ".it" `isSuffixOf` url  = Just Italian
    | ".no" `isSuffixOf` url  = Just Norwegian
    | ".pt" `isSuffixOf` url  = Just Portuguese
    | ".br" `isSuffixOf` url  = Just Portuguese
    | ".ro" `isSuffixOf` url  = Just Romanian
    | ".ru" `isSuffixOf` url  = Just Russian
    | ".es" `isSuffixOf` url  = Just Spanish
    | ".mx" `isSuffixOf` url  = Just Spanish
    | ".se" `isSuffixOf` url  = Just Swedish
    | ".tr" `isSuffixOf` url  = Just Turkish
    | otherwise               = Nothing

-- |Filter 2 <= length <= 80 and hasLetters.
filterFormulas :: [String] -> [String]
filterFormulas =
    filter (\x -> hasOkLength x && hasLetters x && isNoStopWord x &&
      isAcceptableWord x)

-- |True if length between 2 and 80 inclusive.
hasOkLength :: String -> Bool
hasOkLength s =
    let l = length s
    in  l >= 2 && l <= 80

-- |True if String is not in stopWords.
isNoStopWord :: String -> Bool
isNoStopWord s = s `notElem` stopWords

-- |True if string doesn't contains badChars
isAcceptableWord :: String -> Bool
isAcceptableWord s = s `intersect` badChars == [] 

-- |List of StopWords of different languages.
stopWords :: [String]
stopWords = gerStopWords ++ engStopWords

-- |List of german StopWords.
gerStopWords :: [String]
gerStopWords =
    ["ab", "aber", "alle", "allem", "allen", "alles", "als", "also", "am",
     "an", "andere", "anderem", "anderer", "anderes", "anders", "ans", "auch",
     "auf", "aufs", "aus", "ausser", "ausserdem", "bei", "beide", "beiden",
     "beides", "beim", "bereits", "bestehen", "besteht", "bevor", "bin", "bis",
     "bloss", "brauchen", "braucht", "bzw", "da", "dabei", "dadurch", "dafür",
     "dagegen", "daher", "damit", "danach", "dann", "dar", "daran", "darauf",
     "darf", "darum", "darunter", "darüber", "das", "dass", "davon", "dazu",
     "dein", "dem", "demnach", "den", "denen", "denn", "dennoch", "der",
     "deren", "des", "deshalb", "dessen", "dich", "die", "dies", "diese",
     "dieselbte", "diesem", "diesen", "dieser", "dieses", "diesmal", "dir",
     "doch", "dort", "du", "durch", "durfte", "durften", "dürfen", "ebenfalls",
     "ebenso", "eigene", "eigenem", "eigenen", "eigener", "eigenes", "ein",
     "eine", "einem", "einen", "einer", "eines", "einige", "einiges", "einmal",
     "einzelne", "einzelnen", "einzig", "entweder", "er", "erst", "erste",
     "ersten", "es", "etwa", "etwas", "euch", "falls", "fast", "ferner",
     "folgender", "folglich", "für", "ganz", "ganze", "gar", "geben", "gegen",
     "gehabt", "gekonnt", "gemäss", "getan", "gewesen", "gewollt", "geworden",
     "gibt", "hab", "habe", "haben", "hallo", "hat", "hatte", "hatten",
     "heraus", "herein", "hier", "hin", "hinaus", "hinein", "hinter", "hinzu",
     "hätte", "hätten", "ich", "ihm", "ihn", "ihnen", "ihr", "ihre", "ihrem",
     "ihren", "ihrer", "ihres", "im", "immer", "in", "indem", "infolge",
     "innen", "innerhalb", "ins", "inzwischen", "irgend", "irgendwas",
     "irgendwen", "irgendwer", "irgendwie", "irgendwo", "ist", "jede", "jedem",
     "jeden", "jeder", "jederzeit", "jedes", "jedoch", "jene", "jenem", "jenen",
     "jener", "jenes", "jeweiligen", "jeweils", "kann", "kein", "keine",
     "keinem", "keinen", "keiner", "keines", "kommen", "kommt", "konnte",
     "konnten", "können", "könnte", "könnten", "lassen", "leer", "machen",
     "macht", "machte", "machten", "man", "mehr", "mein", "meine", "meinem",
     "meinen", "meiner", "meist", "meiste", "meisten", "mich", "mit", "muss",
     "musste", "mussten", "möchte", "möchten", "müssen", "müssten", "nach",
     "nachdem", "nacher", "neben", "nein", "nicht", "nichts", "noch", "nun",
     "nur", "nämlich", "ob", "obgleich", "obwohl", "oder", "ohne", "schon",
     "sehr", "seid", "sein", "seine", "seinem", "seinen", "seiner", "seines",
     "seit", "seitdem", "seither", "selber", "selbst", "sich", "sie", "siehe",
     "sind", "so", "sobald", "sofern", "sofort", "sogar", "solange", "solch",
     "solche", "solchem", "solchen", "solcher", "solches", "soll", "sollen",
     "sollte", "sollten", "somit", "sondern", "sonst", "sonstiges", "soweit",
     "sowie", "sowohl", "statt", "stets", "such", "u.v.m.", "um", "ums", "und",
     "uns", "unser", "unsere", "unserem", "unseren", "unserer", "unter", "viel",
     "viele", "vielen", "vieler", "vom", "von", "vor", "vorbei", "vorher",
     "vorüber", "wann", "war", "waren", "warum", "was", "weg", "wegen", "weil",
     "weit", "weiter", "weitere", "weiterem", "weiteren", "weiterer",
     "weiteres", "weiterhin", "welche", "welchem", "welchen", "welcher",
     "welches", "wem", "wen", "wenigstens", "wenn", "wenngleich", "wer",
     "werde", "werden", "weshalb", "wessen", "wie", "wieder", "will", "wir",
     "wird", "wo", "wodurch", "wohin", "wollen", "wollte", "wollten", "worin",
     "wurde", "wurden", "während", "wäre", "wären", "würde", "würden", "z.b",
     "zeigen", "zeigt", "zu", "zudem", "zufolge", "zum", "zur", "zurück",
     "zusammen", "zwar", "zwischen", "zzgl", "über"]

-- |List of english StopWords.
engStopWords :: [String]
engStopWords =
    ["'ll", "'s", "'ve", "about", "above", "after", "again", "against", "all",
     "am", "an", "and", "any", "are", "aren't", "as", "at", "be", "because",
     "been", "before", "being", "below", "between", "both", "but", "by", "can",
     "can't", "cannot", "could", "couldn't", "did", "didn't", "do", "does",
     "doesn't", "doing", "don't", "down", "during", "each", "few", "for",
     "from", "further", "get", "had", "hadn't", "has", "hasn't", "have",
     "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here",
     "here's", "hers", "herself", "him", "himself", "his", "how", "how's",
     "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it",
     "it's", "its", "itself", "let's", "me", "more", "most", "mustn't", "my",
     "myself", "no", "none", "nor", "not", "of", "off", "on", "once", "only",
     "or", "other", "ought", "our", "ours ", "ourselves", "out", "over", "own",
     "same", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't",
     "so", "some", "such", "than", "that", "that's", "the", "their", "theirs",
     "them", "themselves", "then", "there", "there's", "these", "they",
     "they'd", "they'll", "they're", "they've", "this", "those", "through",
     "to", "too", "under", "until", "up", "very", "want", "was", "wasn't", "we",
     "we'd", "we'll", "we're", "we've", "were", "weren't", "what", "what's",
     "when", "when's", "where", "where's", "which", "while", "who", "who's",
     "whom", "why", "why's", "with", "won't", "would", "wouldn't", "you",
     "you'd", "you'll", "you're", "you've", "your", "yours", "yourself",
     "yourselves"]

-- |List of characters a lambda formula word is not allowed to contain.
badChars :: String
badChars = "#$%()*,0123456789:;<>@[]{|}§"

-- |Filter out <script>...</script> parts.
filterScript :: [Tag String] -> [Tag String]
filterScript (TagOpen "script" _ : xs) = filterScript (dropTillScriptEnd xs)
filterScript (x:xs)                    = x : filterScript xs
filterScript []                        = []

-- |Helper for filterScript.
-- Drop elements from list till </script> element was processed.
dropTillScriptEnd :: [Tag String] -> [Tag String]
dropTillScriptEnd xs =
    let xs' = dropWhile (/= TagClose "script") xs
    in  if null xs' then xs' else tail xs'

-- |True if String is a valid SoundEx Hash.
isSoundExHash :: String -> Bool
isSoundExHash (c:x:y:z:[]) =
    c `elem` ['A' .. 'Z'] && isDigit x && isDigit y && isDigit z
isSoundExHash _            = False

