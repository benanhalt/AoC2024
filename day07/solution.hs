
possibilities :: [Int] -> [Int]
possibilities (x:y:xs) = possibilities (x+y:xs) ++ possibilities (x*y:xs)
possibilities [x] = [x]


possibilities' :: [Int] -> [Int]
possibilities' (x:y:xs) = possibilities' (x+y:xs) ++ possibilities' (x*y:xs) ++ possibilities' (read (show x ++ show y):xs)
possibilities' [x] = [x]

valid :: [Int] -> Bool
valid (v:xs) = v `elem` possibilities xs

valid' :: [Int] -> Bool
valid' (v:xs) = v `elem` possibilities' xs

main :: IO ()
main = do
  lns <- lines <$> readFile "input.txt"
  let eqs :: [[Int]] = map read . words . filter (`elem` " 0123456789") <$> lns

  print $ sum $ head <$> filter valid eqs
  print $ sum $ head <$> filter valid' eqs

