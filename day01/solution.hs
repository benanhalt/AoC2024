import Data.List (sort, transpose)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let rows :: [[Int]] = (\l -> read <$> words l) <$> input
  let [list1, list2] = sort <$> transpose rows

  putStrLn "Part 1:"
  let diffs = zipWith (\a b -> abs (a - b)) list1 list2
  print $ sum diffs

  putStrLn "Part 2:"
  let multiplicities = (\n -> length $ filter (==n) list2) <$> list1
  print $ sum $ zipWith (*) list1 multiplicities
