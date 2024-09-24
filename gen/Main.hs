import Data.List (stripPrefix)

main :: IO ()
main = do
  version <- readFile "../README.md"
  template <- readFile "template.txt"
  index <- readFile "index.txt"
  old <- readFile "old.txt"
  projects <- readFile "projects.txt"
  talks <- readFile "talks.txt"
  notes <- readFile "notes.txt"
  writeFile "../index.html" $
    replaceAll (replaceLookup [old, index, version, ""]) template
  writeFile "../projects/index.html" $
    replaceAll (replaceLookup ["Projects", projects, version, "../"]) template
  writeFile "../talks/index.html" $
    replaceAll (replaceLookup ["Talks", talks, version, "../"]) template
  writeFile "../notes/index.html" $
    replaceAll (replaceLookup ["Notes", notes, version, "../"]) template

replaceAll :: Eq a => [([a], [a])] -> [a] -> [a]
replaceAll = flip $ foldl $ flip replaceOne
  where
    replaceOne :: Eq a => ([a], [a]) -> [a] -> [a]
    replaceOne (f, t)
      = foldr ((.) (flip maybe (t ++) <*> stripPrefix f) . (:)) []

replaceLookup :: [String] -> [(String, String)]
replaceLookup
  = zipWith ((,) . ('{' :) . (++ "}")) ["head", "body", "version", "back"]