import Data.List (intercalate)
import Data.List.Split (splitOn, wordsBy)
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM, toGregorian)
import GHC.Unicode (toLower)
import System.IO (hGetContents, hPutStr, hSetEncoding, IOMode (ReadMode, WriteMode), openFile,
  hClose, utf8)
import Text.Read (readMaybe)

----------------------------------------------------------------------------------------------------
-- Categories

data Category
  = Out | Lec | Sup | Tut | Pro | Mar
  | Org | Con | Wor | Mee | Sch
  | Conf | Semi | Work | Cont | Coll | Pres | Stud
  deriving (Eq, Read)

instance Show Category where
  show Out = "Outreach"
  show Lec = "Lectures"
  show Sup = "Supervisions"
  show Tut = "Tutorials"
  show Pro = "Problems classes"
  show Mar = "Marking"
  show Org = "Event"
  show Con = "Conference"
  show Wor = "Workshop"
  show Mee = "Meeting"
  show Sch = "School"
  show Conf = "Conference talk"
  show Semi = "Seminar talk"
  show Work = "Workshop talk"
  show Cont = "Contributed talk"
  show Coll = "Colloquium"
  show Pres = "Presentation"
  show Stud = "Study group talk"

-- Categories
----------------------------------------------------------------------------------------------------
-- Items

data Item
  = Activity Int Bool Category String String String String
  | Event Int Bool Category Day Day String String String String
  | Talk Int Bool Category Day String String String String
  deriving Eq

selected :: Item -> Bool
selected (Activity _ cv _ _ _ _ _) = cv
selected (Event _ cv _ _ _ _ _ _ _) = cv
selected (Talk _ cv _ _ _ _ _ _) = cv

category :: Item -> Category
category (Activity _ _ cat _ _ _ _) = cat
category (Event _ _ cat _ _ _ _ _ _) = cat
category (Talk _ _ cat _ _ _ _ _) = cat

instance Read Item where
  readsPrec _ s = case splitOn (";") s of
    index : cv : cat : s' -> case (readMaybe index :: Maybe Int,
      readMaybe cv :: Maybe Bool, readMaybe cat :: Maybe Category) of
      (Just index', Just cv', Just cat')
        | elem cat' [Out, Lec, Sup, Tut, Pro, Mar] -> case s' of
          term : name : institute : notes : s'' ->
            [(Activity index' cv' cat' term name institute notes, concat s'')]
          _ -> []
        | elem cat' [Org, Con, Wor, Mee, Sch] -> case s' of
          begin : end : city : name : link : notes : s'' -> case (parse begin, parse end) of
            (Just begin', Just end') ->
              [(Event index' cv' cat' begin' end' city name link notes, concat s'')]
            _ -> []
          _ -> []
        | otherwise -> case s' of
          day : city : name : event : notes : s'' -> case parse day of
            Just day' -> [(Talk index' cv' cat' day' city name event notes, concat s'')]
            _ -> []
          _ -> []
      _ -> []
    _ -> []
    where
      parse = parseTimeM False defaultTimeLocale "%-d.%-m.%y"

-- Items
----------------------------------------------------------------------------------------------------
-- Pages

data Page = Index | Research | Activities | Events | Talks | Miscellaneous | Lean_LMFDB deriving Eq

instance Show Page where
  show page = case page of
    Index -> "Index"
    Research -> "Research"
    Activities -> "Activities"
    Events -> "Events"
    Talks -> "Talks"
    Miscellaneous -> "Miscellaneous"
    Lean_LMFDB -> "Lean-LMFDB"

pages :: [Page]
pages = [Index, Research, Activities, Events, Talks, Miscellaneous, Lean_LMFDB]

input :: Page -> String
input page = map toLower (show page) ++ ".html"

output :: Page -> String
output Index = "../index.html"
output page = "../" ++ map toLower (show page) ++ "/index.html"

title :: Page -> String
title Index = "David Ang"
title Lean_LMFDB = "Bridging Lean and the LMFDB"
title page = show page

back :: Page -> String
back Index = ""
back _ = "../"

header :: Page -> String
header Index = "<div class=\"me\"> <a href=\"old\" title=\"Click here for some fun!\"> \
  \<img src=\"david.jpg\" alt=\"David\" /> </a> </div>"
header Lean_LMFDB = "Bridging <img src=\"lean.png\" alt=\"Lean\" class=\"lean\" /> and the \
  \<img src=\"lmfdb.png\" alt=\"LMFDB\" class=\"lmfdb\" />"
header page = show page

-- Pages
----------------------------------------------------------------------------------------------------
-- CV

timeCV :: Day -> Day -> String
timeCV begin end = case (d == d', m == m', y == y') of
  (True, True, True) -> format "%-d.%-m.%y" begin
  (_, True, True) -> format "%-d--" begin ++ format "%-d.%-m.%y" end
  (_, _, True) -> format "%-d.%-m--" begin ++ format "%-d.%-m.%y" end
  _ -> format "%-d.%-m.%y--" begin ++ format "%-d.%-m.%y" end
  where
    (y, m, d) = toGregorian begin
    (y', m', d') = toGregorian end
    format = formatTime defaultTimeLocale

showCV :: Item -> String
showCV (Activity _ True cat term name institute notes) = "\\cv{" ++ term ++ "}{"
  ++ (if cat == Out then "" else show cat ++ " for ") ++ "\\emph{" ++ name
  ++ "} in " ++ institute ++ "}{" ++ notes ++ "}"
showCV (Event _ True cat begin end city name link notes) = "\\cv{" ++ timeCV begin end
  ++ "}{\\link[" ++ "\\emph{" ++ name ++ "}]{" ++ link ++ "}, " ++ map toLower (show cat) ++ " in "
  ++ city ++ "}{" ++ notes ++ "}"
showCV (Talk _ True cat day city name event _) = "\\cv{" ++ timeCV day day ++ "}{\\link[" ++ name
  ++ "]{../talks/" ++ map (toLower . head) (wordsBy (flip elem " -\8211") name) ++ ".pdf}}{"
  ++ (if cat == Cont then show cat ++ " for " else "") ++ "\\emph{" ++ event ++ "} in " ++ city
  ++ "}"
showCV _ = ""

showsCV :: [Item] -> [(String, [Category])] -> String
showsCV items = (.) (init . drop 1) . flip foldr "" . (.) (++) . uncurry $ \heading cats ->
  "\n\\section{" ++ heading ++ "}\n"
  ++ unlines [showCV item | item <- items, elem (category item) cats]

activitiesCV :: [Item] -> String
activitiesCV activities = showsCV activities
  [ ("Selected teaching", [Lec, Sup, Tut, Pro])
  , ("Outreach activities", [Out])
  ]

eventsCV :: [Item] -> String
eventsCV events = showsCV events
  [ ("Events organised", [Org])
  , ("Selected conferences", [Con, Wor])
  ]

talksCV :: [Item] -> String
talksCV talks = showsCV talks
  [ ("Selected talks", [Conf, Semi, Work, Cont, Coll])
  ]

-- CV
----------------------------------------------------------------------------------------------------
-- Website

timeWS :: Day -> Day -> String
timeWS begin end
  | begin == end = format "on %A, %-d %B %Y" begin
  | otherwise = format "from %A, %-d %B %Y" begin ++ format " to %A, %-d %B %Y" end
  where
    format = formatTime defaultTimeLocale

showWS :: Item -> String
showWS (Activity _ _ _ term name institute notes) = "        <li> " ++ name ++ " ran in " ++ term
  ++ " at " ++ institute ++ (if null notes then "" else " <br /> " ++ notes) ++ " </li>"
showWS (Event _ _ _ begin end city name link notes) = "        <li> <a href=\"" ++ link ++ "\"> "
  ++ name ++ " </a> held in " ++ city ++ ' ' : timeWS begin end
  ++ (if null notes then "" else " <br /> " ++ notes) ++ " </li>"
showWS (Talk _ _ _ day city name event notes) = "        <li> <a href=\""
  ++ map (toLower . head) (wordsBy (flip elem " -\8211") name) ++ ".pdf\"> " ++ name
  ++ " </a> given in " ++ city ++ ' ' : timeWS day day ++ " as part of " ++ event
  ++ (if null notes then "" else " (" ++ notes ++ ")") ++ " </li>"

showsWS :: [Item] -> [(String, [Category])] -> String
showsWS items = (.) (init . drop 6) . flip foldr "" . (.) (++) . uncurry $ \heading cats ->
  "      <ul>\n        " ++ heading
  ++ '\n' : unlines [showWS item | item <- items, elem (category item) cats] ++ "      </ul>\n"

activitiesWS :: [Item] -> String
activitiesWS activities = showsWS activities
  [ ("Here are the outreach activities I was involved in.", [Out])
  , ("Here are the modules I have given lectures for.", [Lec])
  , ("Here are the activities I have supervised.", [Sup])
  , ("Here are the small group tutorials I have ran.", [Tut])
  , ("Here are the large problems classes I have assisted with.", [Pro])
  , ("Here are the modules I have marked courseworks for.", [Mar])
  ]

eventsWS :: [Item] -> String
eventsWS events = showsWS events
  [ ("Here are the events I have organised.", [Org])
  , ("Here are the conferences I have attended.", [Con])
  , ("Here are the workshops I have attended.", [Wor])
  , ("Here are the meetings I have attended.", [Mee])
  , ("Here are the schools I have attended.", [Sch])
  ]

talksWS :: [Item] -> String
talksWS talks = showsWS talks
  [ ("Here are the talks for my conference proceedings.", [Conf])
  , ("Here are the weekly seminars I was invited to.", [Semi])
  , ("Here are the workshop talks I was invited to.", [Work])
  , ("Here are the conference talks I have contributed.", [Cont])
  , ("Here are the informal colloquiums I have given.", [Coll])
  , ("Here are the presentations I have done during my career.", [Pres])
  , ("Here are the study group talks I have contributed.", [Stud])
  ]

-- Website
----------------------------------------------------------------------------------------------------
-- Generation

readUTF8 :: String -> (String -> IO a) -> IO a
readUTF8 file m = openFile file ReadMode >>= \h -> hSetEncoding h utf8 >> hGetContents h >>= m

writeUTF8 :: String -> String -> IO ()
writeUTF8 file s = openFile file WriteMode >>= \h -> hSetEncoding h utf8 >> hPutStr h s >> hClose h

replaceUTF8 :: String -> String -> [(String, String)] -> IO ()
replaceUTF8 file s dict = writeUTF8 file $
  foldl (flip . uncurry $ \before after -> intercalate after . splitOn before) s dict

generateCV :: String -> [Item] -> [Item] -> [Item] -> IO ()
generateCV template activities events talks = replaceUTF8 "../cv.tex" template
  [ ("{ACTIVITIES}", activitiesCV $ filter selected activities)
  , ("{EVENTS}", eventsCV $ filter selected events)
  , ("{TALKS}", talksCV $ filter selected talks)
  , ("&", "\\&")
  , ("\201", "\\'E")
  , ("\224", "\\`a")
  , ("\232", "\\`e")
  , ("\233", "\\'e")
  , ("\252", "\\\"u")
  , ("\322", "\\l ")
  , ("\8211", "--")
  ]

generateWS :: String -> [Item] -> [Item] -> [Item] -> Page -> IO ()
generateWS template activities events talks page = readUTF8 "../README.md" $ \date ->
  readUTF8 (input page) $ \body -> replaceUTF8 (output page) template
    [ ("{TITLE}", title page)
    , ("{BACK}", back page)
    , ("{HEADER}", header page)
    , ("{DATE}", date)
    , ("{BODY}", intercalate "\n    " $ splitOn "\n" body)
    , ("{ACTIVITIES}", activitiesWS activities)
    , ("{EVENTS}", eventsWS events)
    , ("{TALKS}", talksWS talks)
    ]

main :: IO ()
main = let format = reverse . map read . tail . lines in
  readUTF8 "activities.csv" $ \activities -> let a = format activities in
  readUTF8 "events.csv" $ \events -> let e = format events in
  readUTF8 "talks.csv" $ \talks -> let t = format talks in do
  readUTF8 "template.tex" $ \template -> generateCV template a e t
  readUTF8 "template.html" $ \template -> mapM_ (generateWS template a e t) pages

-- Generation
----------------------------------------------------------------------------------------------------