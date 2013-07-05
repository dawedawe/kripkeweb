module WebParser
( langAttr2StemAlgo
, parseBody
, parseLang
, parseMain
, tld2StemAlgo
) where

import Data.List (intersect, isSuffixOf)
import Data.List.Utils (replace)
import Data.Maybe (mapMaybe)
import NLP.Tokenize (tokenize)
import NLP.Snowball
import Text.HTML.TagSoup

import Util (hasLetters, lowerString)

-- |Parse all acceptable inner text out of the given tags.
parseBody :: [Tag String] -> [String]
parseBody =
    filterFormulas . map lowerString . tokenize . innerText . filterScript

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

-- |Parse the meta, title and hx tags as the main information of a page.
parseMain :: [Tag String] -> [String]
parseMain tgs =
    let
      mtgs = parseMeta tgs
      ttgs = parseTitle tgs
      htgs = parseHeadlines tgs
    in
      concat [mtgs, ttgs, htgs]

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
      desc      = getFirstTagContentAttrib descTags
      keyw      = getFirstTagContentAttrib keywTags
      metaWords = words (replace "," " " (desc ++ " " ++ keyw))
    in
      (filterFormulas . map lowerString . concatMap tokenize) metaWords

-- |Get the content attribute value ouf of the first tag in the given list if
-- it's there.
getFirstTagContentAttrib :: [Tag String] -> String
getFirstTagContentAttrib []    = ""
getFirstTagContentAttrib (x:_) = fromAttrib "content" x

-- |Parse the webpage title ouf of the <title> tag.
parseTitle :: [Tag String] -> [String]
parseTitle tgs =
    let tTag  = sections (~== ("<title>" :: String)) tgs
    in  if tTag /= [] && length (head tTag) > 0
          then case head tTag of
                 TagOpen "title" _ : TagText x : _ ->
                   (filterFormulas . map lowerString . concatMap tokenize)
                     (words x)
                 _                                 -> []
          else []

-- |Parse and postprocess data out of hx tags.
parseHeadlines :: [Tag String] -> [String]
parseHeadlines tgs =
    let
      h1s = mapMaybe (parseHx "h1") (sections (~== ("<h1>" :: String)) tgs)
      h2s = mapMaybe (parseHx "h2") (sections (~== ("<h2>" :: String)) tgs)
      h3s = mapMaybe (parseHx "h3") (sections (~== ("<h3>" :: String)) tgs)
      h4s = mapMaybe (parseHx "h4") (sections (~== ("<h4>" :: String)) tgs)
      hxs = concat [h1s, h2s, h3s, h4s]
    in
      (filterFormulas . map lowerString . concatMap tokenize) hxs

-- |Parse a single hx section.
parseHx :: String -> [Tag String] -> Maybe String
parseHx hx tgs =
    let
      tgs' = dropWhile (not . isTagText) (takeWhile (/= TagClose hx) tgs)
    in
      case tgs' of
        TagText txt : _ -> Just txt
        _               -> Nothing

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

-- |Filter ok length, hasLetters, isNoStopWord and isAcceptableWord.
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

