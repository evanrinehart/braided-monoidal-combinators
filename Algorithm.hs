module Algorithm where

import Data.List
import Data.List.Split

data Move = Id | Swap | Merge deriving (Show)

calcMerge :: [String] -> [[Move]]
calcMerge [] = []
calcMerge letters0 = f letters0 where
  f letters | separation letters == 0 && nub letters == letters = []
            | otherwise = [concat moves] ++ f letters' where
                segs = toSegs letters
                moves = processSegs segs []
                segs' = applyMoves moves segs
                letters' = toLetters segs'


exam :: [String]
exam = ["a", "b", "a", "b", "a"]

separation :: [String] -> Int
separation xs = sum subSeps where
  letters = nub xs
  subSeps = map (calcSubSep xs) letters
  calcSubSep xs letter = sum . map length . mySplit letter $ xs
  mySplit l = filter (not . null) . splitOn [l] . dropWhile (/= l) . dropWhileEnd (/= l)


data Segment =
  Glob Int String |
  Region [String]
    deriving (Show)

processSegs :: [Segment] -> [Segment] -> [[Move]]
processSegs [] prev = []
processSegs (Glob n s : xs) prev = f n where
  f n | even n = [movesEven] ++ rest
      | odd n = [movesOdd] ++ rest where
          movesEven = replicate (div n 2) Merge 
          movesOdd = (replicate (div n 2) Merge ++ [Id])
          rest = processSegs xs (prev ++ [applyGlobMove (Glob n s)])
processSegs (Region letters : xs) prev = [moves] ++ processSegs xs (prev ++ [Region $ applyRegionMove moves letters]) where
  moves = strategize letters prev xs

strategize :: [String] -> [Segment] -> [Segment] -> [Move]
strategize letters posPrev posNext = answer where
  orig = posPrev ++ [Region letters] ++ posNext
  universe = eas (length letters)
  results = map checkScore universe
  answer = pickBest results
  checkScore :: [Move] -> ([Move], Int)
  checkScore moves = (moves, answer) where
    sep1 = separation (toLetters orig)
    pos' = posPrev ++ [Region $ applyRegionMove moves letters] ++ posNext
    sep2 = separation (toLetters pos')
    answer = abs (sep2 - sep1)

pickBest :: [([Move], Int)] -> [Move]
pickBest [] = error "no moves"
pickBest (x:xs) = f (x:xs) x where
  f :: [([Move], Int)] -> ([Move], Int) -> [Move]
  f ((mv, 2):_) _ = mv
  f ((mv, n):xs) b@(best, m) = if n > m then f xs (mv, n) else f xs b
  f [] (mv, _) = mv

applyMoves :: [[Move]] -> [Segment] -> [Segment]
applyMoves moves seg = f moves seg where
  f [] [] = []
  f (moves:ms) (Region letters : xs) = Region (applyRegionMove moves letters) : f ms xs
  f (moves:ms) (Glob n s : xs) = applyGlobMove (Glob n s) : f ms xs
  f _ _ = error "applyMoves"

applyRegionMove :: [Move] -> [String] -> [String]
applyRegionMove moves letters = f moves letters where
  f [] [] = []
  f (Id:ms) (s:ss) = s : f ms ss
  f (Swap:ms) (s1:s2:ss) = s2 : s1 : f ms ss
  f _ _ = error "applyRegionMove"

applyGlobMove :: Segment -> Segment
applyGlobMove (Glob n s)
  | n < 2 = error "applyGlobMove"
  | n == 2 = Region [s]
  | even n = Glob (div n 2) s
  | odd n = Glob (div n 2 + 1) s

toSegs :: [String] -> [Segment]
toSegs items = toSegs' (group items) where
  toSegs' [] = []
  toSegs' (x:xs) = case length x of
    1 -> let (letters, rest) = span (\x -> length x == 1) (x:xs) in
         Region (map head letters) : toSegs' rest
    _ -> Glob (length x) (head x): toSegs' xs

toLetters :: [Segment] -> [String]
toLetters [] = []
toLetters (Region ss : xs) = ss ++ toLetters xs
toLetters (Glob n s : xs) = (replicate n s) ++ toLetters xs

-- enumerate all swaps on a region of n letters
eas :: Int -> [[Move]]
eas 0 = [[]]
eas 1 = [[Id]]
eas 2 = [[Id,Id],[Swap]] -- speed up
eas n
  | even n = basic ++ weird
  | odd n = basic' ++ weird1' ++ weird2' where
      half = eas (div n 2)
      small = eas (div n 2 - 1)
      oddHalf = eas (div n 2)
      oddSmall = eas (div n 2 - 1)
      basic = [x++y | x <- half, y <- half]
      weird = [x++[Swap]++y | x <- small, y <- small]
      basic' = [x++[Id]++y | x <- oddHalf, y <- oddHalf]
      weird1' = [x++[Swap]++y | x <- oddHalf, y <- oddSmall]
      weird2' = [x++[Swap]++y | x <- oddSmall, y <- oddHalf]
