import Data.List (stripPrefix)

main :: IO ()
main = do
  version <- readFile "../README.md"
  template <- readFile "template.txt"
  index <- readFile "index.txt"
  old <- readFile "old.txt"
  projects <- readFile "projects.txt"
  talks <- readFile "talks.txt"
  conferences <- readFile "conferences.txt"
  teaching <- readFile "teaching.txt"
  notes <- readFile "notes.txt"
  workshop <- readFile "workshop.txt"
  writeFile "../index.html" $
    replaceAll (replaceLookup ["David Kurniadi Angdinata", old, index, version, ""]) template
  writeFile "../projects/index.html" $
    replaceAll (replaceLookup ["Projects", "Projects", projects, version, "../"]) template
  writeFile "../talks/index.html" $
    replaceAll (replaceLookup ["Talks", "Talks", talks, version, "../"]) template
  writeFile "../conferences/index.html" $
    replaceAll (replaceLookup ["Conferences", "Conferences", conferences, version, "../"]) template
  writeFile "../teaching/index.html" $
    replaceAll (replaceLookup ["Teaching", "Teaching", teaching, version, "../"]) template
  writeFile "../notes/index.html" $
    replaceAll (replaceLookup ["Notes", "Notes", notes, version, "../"]) template
  writeFile "../lean-lmfdb/index.html" $
    replaceAll (replaceLookup ["Bridging Lean and the LMFDB", "Bridging <img class=\"lean\" src=\"lean.png\" /> and the <img class=\"lmfdb\" src=\"lmfdb.png\" />", workshop, version, "../"]) template

replaceAll :: Eq a => [([a], [a])] -> [a] -> [a]
replaceAll = flip $ foldl $ flip replaceOne
  where
    replaceOne :: Eq a => ([a], [a]) -> [a] -> [a]
    replaceOne (f, t)
      = foldr ((.) (flip maybe (t ++) <*> stripPrefix f) . (:)) []

replaceLookup :: [String] -> [(String, String)]
replaceLookup
  = zipWith ((,) . ('{' :) . (++ "}")) ["title", "head", "body", "version", "back"]