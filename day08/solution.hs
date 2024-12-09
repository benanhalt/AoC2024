import Data.List (foldl', tails)
import Data.Map.Strict qualified as M
import Data.Set qualified as S

type AntMap = M.Map Char [(Int, Int)]

updateMap :: AntMap -> (Char, (Int, Int)) -> AntMap
updateMap ants ('.', _) = ants
updateMap ants (char, pos) = M.alter update char ants
  where
    update :: Maybe [(Int, Int)] -> Maybe [(Int, Int)]
    update Nothing = Just [pos]
    update (Just ps) = Just (pos : ps)

findAntiNodes :: AntMap -> Char -> [(Int, Int)]
findAntiNodes ants char = concatMap (uncurry antiNodes) allPairs
  where
    Just locs = M.lookup char ants
    allPairs = [(p1, p2) | (p1 : ps) <- tails locs, p2 <- ps]

antiNodes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antiNodes (r1, c1) (r2, c2) =
  [ (r1 + 2 * (r2 - r1), c1 + 2 * (c2 - c1)),
    (r1 - (r2 - r1), c1 - (c2 - c1))
  ]

type OnGrid = ((Int, Int) -> Bool)

allAntiNodes :: OnGrid -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
allAntiNodes onGrid (r1, c1) (r2, c2) =
  (takeWhile onGrid [(r1 + i * (r2 - r1), c1 + i * (c2 - c1)) | i <- [0..]]) ++
  (takeWhile onGrid [(r1 - i * (r2 - r1), c1 - i * (c2 - c1)) | i <- [0..]])

findAllAntiNodes :: OnGrid -> AntMap -> Char -> [(Int, Int)]
findAllAntiNodes onGrid ants char = concatMap (uncurry (allAntiNodes onGrid)) allPairs
  where
    Just locs = M.lookup char ants
    allPairs = [(p1, p2) | (p1 : ps) <- tails locs, p2 <- ps]


main :: IO ()
main = do
  lns <- lines <$> readFile "input.txt"
  let rows = length lns
  let cols = length $ head lns
  let onGrid (r,c) = r >= 0 && r < rows && c >= 0 && c < cols

  let ants = foldl' updateMap M.empty [(char, (r, c)) | (r, ln) <- zip [0 ..] lns, (c, char) <- zip [0 ..] ln]

  let part1 = S.fromList $ concatMap (filter onGrid . findAntiNodes ants) (M.keys ants)
  print $ S.size part1

  let part2 = S.fromList $ concatMap (findAllAntiNodes onGrid ants) (M.keys ants)
  print $ S.size part2
