import Data.Map.Strict as M

type Grid a = M.Map (Int, Int) a

countXmass :: Grid Char -> Int
countXmass grid =
  let ((maxRow, maxCol), _) = M.findMax grid
   in sum [xmassAt grid r c | r <- [0 .. maxRow], c <- [0 .. maxCol]]

xmassAt :: Grid Char -> Int -> Int -> Int
xmassAt grid r c =
  let dirs = [(dr, dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
   in sum [1 | dir <- dirs, xmasInDir grid r c dir]

xmasInDir :: Grid Char -> Int -> Int -> (Int, Int) -> Bool
xmasInDir grid r c (dr, dc) =
  let steps = [(r + i * dr, c + i * dc) | i <- [0 .. 3]]
   in (Just "XMAS" ==) $ mapM (`M.lookup` grid) steps

isCross :: Grid Char -> Int -> Int -> Bool
isCross grid r c =
  let a = mapM (`M.lookup` grid) [(r + dr, c + dc) | (dr, dc) <- [(-1, -1), (0, 0), (1, 1)]]
      b = mapM (`M.lookup` grid) [(r + dr, c + dc) | (dr, dc) <- [(1, -1), (0, 0), (-1, 1)]]
   in ((a == Just "MAS") || (a == Just "SAM")) && ((b == Just "MAS") || (b == Just "SAM"))

countCrosses :: Grid Char -> Int
countCrosses grid =
  let ((maxRow, maxCol), _) = M.findMax grid
   in sum [1 | r <- [0 .. maxRow], c <- [0 .. maxCol], isCross grid r c]

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
