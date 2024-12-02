
type Report = [Int]

-- Return whether the difference between two consecutive values is
-- okay for an increasing report.
ok :: Int -> Bool
ok diff = diff >= 1 && diff <= 3

-- Return whether the report is safe as increasing. Reversing the
-- report will indicate if it is safe decreasing.
safeInc :: Report -> Bool
safeInc l = all ok $ zipWith (-) (tail l) l

-- Report is safe in part1 if it is safe increasing or decreasing.
part1Safe :: Report -> Bool
part1Safe r = safeInc r || safeInc (reverse r)

-- Return whether the report is safe increasing when a single
-- value can be dropped.
canDrop :: Report -> Bool
canDrop [x, y] = ok (y - x)
-- Consider the first three values. If they are safe, move forward one
-- value and consider the next three. If not, try dropping one to make
-- it safe and check if the remaining list is safe w/o drops.
canDrop (x:y:z:xs)
  | ok (y - x) && ok (z - y) = canDrop (y:z:xs) -- No need to drop any of first 3 values.
  | ok (y - x) = noDrop (x:z:xs) || noDrop (x:y:xs) -- y and z conflict. Drop one of them.
  | ok (z - y) = noDrop (y:z:xs) -- x and y conflict but not y and z. Drop x.
  | ok (z - x) = noDrop (x:z:xs) -- y conflicts with x or z, but x and z are okay.
  | otherwise = False -- can't reconcile.
  where
    -- Return whether the report is safe increasing when no value can
    -- be dropped.
    noDrop = safeInc


-- Report is safe in part1 if it is safe increasing or decreasing with
-- a single value dropped.
part2Safe :: Report -> Bool
part2Safe r = canDrop r || canDrop (reverse r)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let reports :: [Report] = (fmap read . words) <$> input

  putStrLn "Part 1:"
  print $ length $ filter part1Safe reports

  putStrLn "Part 2:"
  print $ length $ filter part2Safe reports

