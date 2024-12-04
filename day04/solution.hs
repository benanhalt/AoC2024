import Data.Map.Strict qualified as M

type Grid a = M.Map (Int, Int) a

countXmass :: Grid Char -> Int
countXmass grid = sum $ xmassAt grid <$> M.keys grid

xmassAt :: Grid Char -> (Int, Int) -> Int
xmassAt grid (r, c) =
  let dirs = [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
   in length $ filter (xmasInDir grid r c) dirs

xmasInDir :: Grid Char -> Int -> Int -> (Int, Int) -> Bool
xmasInDir grid r c (dr, dc) =
  let steps = [(r + i * dr, c + i * dc) | i <- [0 .. 3]]
   in (Just "XMAS" ==) $ mapM (`M.lookup` grid) steps

isCross :: Grid Char -> (Int, Int) -> Bool
isCross grid (r, c) =
  let
    strAlong steps = mapM (`M.lookup` grid) [(r + dr, c + dc) | (dr, dc) <- steps]
    a = strAlong [(-1, -1), (0, 0), (1, 1)]
    b = strAlong [(1, -1), (0, 0), (-1, 1)]
   in ((a == Just "MAS") || (a == Just "SAM")) && ((b == Just "MAS") || (b == Just "SAM"))

countCrosses :: Grid Char -> Int
countCrosses grid = length $ filter (isCross grid) $ M.keys grid

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let grid =
        M.fromList
          [ ((row, col), char)
            | (row, line) <- zip [0 ..] ls,
              (col, char) <- zip [0 ..] line
          ]

  print $ countXmass grid
  print $ countCrosses grid
