import Data.List (partition, delete, (\\))
import Data.List.Split (splitOn)

valid :: Eq a => [(a, a)] -> [a] -> Bool
valid rules update = all (`elem` rules) $ zip update (tail update)

middle :: [a] -> a
middle as = as !! (length as `div` 2)

-- Kahn's algorithm
-- kahn listOfEdges nodesWithNoIncomingEdges accumulatorOfResult = accumulatorOfResult ++ (topological sort of nodes in listOfEdges)
kahn :: (Ord a) => [(a, a)] -> [a] -> [a] -> [a]
kahn [] [] l = reverse l
kahn g [] l = error "graph contains cycle"
kahn g s l =
  let
    n = head s  -- a node with no incoming edges
    es = filter ((== n) . fst) g  -- edges originatitng at n
    ms = snd <$> es -- nodes directly reachable from n
    g' = g \\ es -- remove edges originating from n
    s' = (noIncoming g' ms) ++ (delete n s) -- new nodes w/o incoming edges
  in
    kahn g' s' (n:l)

noIncoming :: (Eq a) => [(a, a)] -> [a] -> [a]
noIncoming g ns = ns \\ (snd <$> g)

-- Perform topological sort of the graph given by edges restricted to the given verts.
topoSort :: (Show a, Ord a) => [(a, a)] -> [a] -> [a]
topoSort edges verts = kahn filteredEdges (noIncoming filteredEdges verts) []
  where
    -- remove edges that don't join any passed in vertices.
    filteredEdges = filter (\(a, b) -> a `elem` verts && b `elem` verts) edges

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let [ruleLns, updateLns] = splitOn [""] ls
  let rules = (\[a,b] -> (a,b)) . splitOn "|" <$> ruleLns
  let updates = splitOn "," <$> updateLns
  let (correct, incorrect) = partition (valid rules) updates
  print $ sum $ read . middle <$> correct
  let fixed = topoSort rules <$> incorrect
  print $ sum $ read . middle <$> fixed
