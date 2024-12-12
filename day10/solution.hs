import Data.Map.Strict qualified as M
import Data.Set qualified as S

type Grid = M.Map (Int, Int) Int

parseInput :: [[Char]] -> (Grid, [(Int, Int)])
parseInput lns = (M.fromList heightMap, zeros)
  where
    heightMap = [((r, c), charToHeight char) | (r, ln) <- zip [0..] lns, (c, char) <- zip [0..] ln]
    charToHeight char = if char == '.' then -1 else read [char]
    zeros = [(r, c) | ((r, c), height) <- heightMap, height == 0]

countTrails :: Grid -> (Int, Int) -> Int
countTrails grid pos = if currentHeight == Just 9
  then 1
  else sum [countTrails grid pos' | pos' <- allDirsFrom pos, M.lookup pos' grid == ((+ 1) <$> currentHeight)]
  where
    currentHeight = M.lookup pos grid
    allDirsFrom (r,c) = [(r + dr, c + dc) | (dr, dc) <- [(-1,0), (1,0), (0, -1), (0, 1)]]

followTrail :: Grid -> [(Int, Int)] -> [[(Int, Int)]]
followTrail grid current = if currentHeight == Just 9
  then [current]
  else concatMap (followTrail grid) [next:current | next <- allDirsFrom pos, M.lookup next grid == ((+ 1) <$> currentHeight)]
  where
    pos = head current
    currentHeight = M.lookup pos grid
    allDirsFrom (r,c) = [(r + dr, c + dc) | (dr, dc) <- [(-1,0), (1,0), (0, -1), (0, 1)]]

scoreTrailHead :: Grid -> (Int, Int) -> Int
scoreTrailHead grid start = S.size endPoints
  where
    endPoints = S.fromList $ head <$> followTrail grid [start]


main :: IO ()
main = do
  lns <- lines <$> readFile "input.txt"
  let (grid, zeros) = parseInput lns
  print $ sum $ scoreTrailHead grid <$> zeros
  print $ sum $ countTrails grid <$> zeros
