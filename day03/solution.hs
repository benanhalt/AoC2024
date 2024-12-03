{-# LANGUAGE OverloadedRecordDot #-}

import Text.Regex.PCRE (Regex, match, makeRegex)
import Data.List (foldl')

data Op = Mul Int Int | Do | Dont
  deriving Show

parseInput :: String -> [Op]
parseInput input = parseOp <$> match re input
  where
    re :: Regex
    re = makeRegex "(mul)\\(([0-9]{1,3}),([0-9]{1,3})\\)|(do)\\(\\)|(don't)\\(\\)"

parseOp :: [String] -> Op
parseOp [_, "mul", a, b,    _,       _] = Mul (read a) (read b)
parseOp [_,     _, _, _, "do",       _] = Do
parseOp [_,     _, _, _,    _, "don't"] = Dont
parseOp other = error $ show other

data State = State { acc :: Int, doing :: Bool }

type Update = State -> Op -> State

update1 :: Update
update1 s (Mul a b) = s { acc = s.acc + a * b }
update1 s _         = s

update2 :: Update
update2 s (Mul a b) = if s.doing then s { acc = s.acc + a * b } else s
update2 s Do        = s { doing = True }
update2 s Dont      = s { doing = False }


run :: Update -> [Op] -> State
run update = foldl' update (State { acc = 0, doing = True })

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ops = parseInput input

  putStrLn "Part 1:"
  let result = run update1 ops
  print result.acc

  putStrLn "Part 2:"
  let result = run update2 ops
  print result.acc
