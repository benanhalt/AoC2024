import Data.List (transpose, unfoldr)
import Data.Map.Strict qualified as M
import Data.Set qualified as S

data Dir = Up | Down | Rght | Lft
  deriving (Eq, Show, Ord)

nextDir :: Dir -> Dir
nextDir Up = Rght
nextDir Rght = Down
nextDir Down = Lft
nextDir Lft = Up

step :: (Int, Int) -> Dir -> (Int, Int)
step (r, c) Up = (r-1, c)
step (r, c) Down = (r+1, c)
step (r, c) Lft = (r, c-1)
step (r, c) Rght = (r, c+1)

type Grid = M.Map (Int, Int) Char
type Guard = ((Int, Int), Dir)

move :: Grid -> Guard -> Maybe Guard
move grid (pos, d) =
  let
    pos' = step pos d
    adv char = if char /= '#' then Just (pos', d) else move grid (pos, nextDir d)
  in adv =<< M.lookup pos' grid

fullPath :: Grid -> Guard -> [Guard]
fullPath grid = unfoldr update
  where
    update guard = (\g -> (g, g)) <$> move grid guard

hasLoop :: S.Set Guard -> [Guard] -> Bool
hasLoop seen [] = False
hasLoop seen (g:gs) = S.member g seen || hasLoop (S.insert g seen) gs

part2 :: Grid -> Guard -> [(Int, Int)]
part2 grid g = filter makesLoop $ M.keys grid
  where
    makesLoop pos = hasLoop S.empty (fullPath (M.insert pos '#' grid) g)

main :: IO ()
main = do
  lns <- lines <$> readFile "input.txt"
  let grid = M.fromList [((r, c), char) | (r, ln) <- zip [0 ..] lns, (c, char) <- zip [0 ..] ln]
  let pos = head [(r, c) | (r, ln) <- zip [0 ..] lns, (c, char) <- zip [0 ..] ln, char == '^']
  let path = fullPath grid (pos, Up)
  let visited = S.fromList $ fst <$> path
  print $ S.size visited
  let possibilities = part2 grid (pos, Up)
  print $ length possibilities
