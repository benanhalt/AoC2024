
safe :: [Int] -> Bool
safe l =
  let
    diffs = zipWith (-) l $ tail l
    absDiffs = abs <$> diffs
    monotonic = all (> 0) diffs || all (< 0) diffs
    bounded = all (<= 3) absDiffs && all (>= 1) absDiffs
  in
    monotonic && bounded

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let reports :: [[Int]] = (fmap read . words) <$> input
  print $ length $ filter safe reports
