{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Control.BraidedMonoidalCombinators.Permutation where

import Data.List
import Data.Maybe
import Data.List.Split
import Data.Ord
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Monad

import Debug.Trace

import Control.BraidedMonoidalCombinators.Diagrams hiding (Swap, Id, Merge)

{-
This module is dedicated to the quasiquoter which can auto re-route segments
of a diagram. Usage: [braid| a b c -> c b a c c c] Repeats are allowed which
translate into merges and copies. Unused variables will be converted into
holes or nevers.

How it works:
The algorithm to solve for the best network is not very efficient, but it
does produce good results for reasonbly sized braiding expressions. I did
not want huge inefficient solutions to slow down the compiler.

The process has 3 stages.
Stage 0: if necessary create outer layers to the solution which simply
add holes and nevers to deal with unused variables.

Stage 1: Starting from both sides, the goal is to merge repeated variables
as fast as possible. The less wires there are in the middle, the easier it
will be to swap things into place. Solution proceeds based on the current
"position". Segments with adjacent repeated variables are not allowed to be
swapped and are merged as soon as possible. Heterogenous regions between
locked globs meanwhile are assigned a combination of swaps which reduce
total separation by 2. The algorithm will settle for 1 or do nothing if
necessary. When no repeats are left stage 1 is complete. No attempt is made
to direct final positions of wires based on the progress of the other sides
merging.

Stage 2: When merging is complete from both sides, they now share a single
set of unique variables, but probably out of order. A permutation using only
swaps is selected. To choose the swap strategy, the algorithm goes with the
moves that reduce total "distance" the most. Distance is the sum of squares
of the distance of each variable to their target location.

When up to 5 stages are ready to combine, all moves are converted into Haskell
expressions and then combined. Note that merging in the destination half of
the diagram are implemented as copy.
-}

data Segment =
  Glob Int String |
  Region [String]
    deriving (Show)

data Move = Id | Swap | Merge deriving (Show)

braid = QuasiQuoter
  { quoteExp = braidExpr
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined 
  }

-- magic
braidExpr :: String -> Q Exp
braidExpr s = do
  let [x, y] = splitOn "->" s
  let src = words x
  let dest = words y
  mkDiagram src dest

mkDiagram :: [String] -> [String] -> Q Exp
mkDiagram [] [] = [| empty |]
mkDiagram src dest = do
  let leftBuffer = fillHoles [| hole |] src dest
  let rightBuffer = fillHoles [| never |] dest src
  let src' = filter (\x -> elem x dest) src
  let dest' = filter (\x -> elem x src) dest
  let leftMoves = calcMerge src'
  let rightMoves = calcMerge dest'
  let leftLetters = manyMoves leftMoves src'
  let rightLetters = manyMoves rightMoves dest'
  let middleMoves = calcPermutation leftLetters rightLetters
  let left = map mkSliceLR leftMoves
  let middle = map mkSliceLR middleMoves
  let right = map mkSliceRL (reverse rightMoves)
  let finalPieces = leftBuffer ++ left ++ middle ++ right ++ rightBuffer
  if length finalPieces == 0
    then basicIdentity (length src)
    else mkComposedSlices finalPieces

debug :: Show s => String -> s -> s
debug msg x = Debug.Trace.trace (msg ++ ": " ++ show x ++ "\n") x
debug_ x = Debug.Trace.trace ("debug: " ++ show x ++ "\n") x

fillHoles :: Q Exp -> [String] -> [String] -> [Q Exp]
fillHoles theHole here there = if bufferNeeded here there then answer elements else [] where
  answer [] = []
  answer (x:xs) = [foldl (\acc e -> [| ($(acc)) <> ($(e)) |]) x xs]
  elements = f here
  f [] = []
  f (letter:xs) | elem letter there = [| ident |] : f xs
                | otherwise = theHole : f xs

-- buffer is needed if there are any elements of here that are not in there
bufferNeeded :: [String] -> [String] -> Bool
bufferNeeded here there = any (\e -> not (elem e there)) here

basicIdentity :: Int -> Q Exp
basicIdentity n = foldl (\acc e -> [| ($(acc)) <> ($(e)) |]) [| ident |] (replicate (n-1) [| ident |])

extractLetters :: Q Exp -> Q ([String], [String])
extractLetters encant = do
  LamE [ListP vs] (ListE us) <- encant
  let src = map (\(VarP s) -> show s) vs
  let dest = map (\(VarE s) -> show s) us
  return (src, dest)

mkSliceLR :: [Move] -> Q Exp
mkSliceLR (mv:mvs) = foldl f (g mv) mvs where
  g Id = [| ident |]
  g Swap = [| swap |]
  g Merge = [| merge |]
  f exp mv = [| ($(exp)) <> ($(g mv)) |]

mkSliceRL :: [Move] -> Q Exp
mkSliceRL (mv:mvs) = foldl f (g mv) mvs where
  g Id = [| ident |]
  g Swap = [| swap |]
  g Merge = [| copy |]
  f exp mv = [| ($(exp)) <> ($(g mv)) |]

mkComposedSlices :: [Q Exp] -> Q Exp
mkComposedSlices (e:es) = foldl (\eacc ee -> [| ($(eacc)) >>> ($(ee)) |]) e es


manyMoves :: [[Move]] -> [String] -> [String]
manyMoves turns start = foldl (\pos mvs -> f mvs pos) start turns where
  f [] [] = []
  f (Id:ms) (s:ss) = s : f ms ss
  f (Merge:ms) (s1:s2:ss) | s1 == s2 = s1 : f ms ss
                          | otherwise = error "manyMoves invalid Merge"
  f (Swap:ms) (s1:s2:ss) = s2 : s1 : f ms ss
  f _ _ = error "manyMoves invalid input data"

calcPermutation :: [String] -> [String] -> [[Move]]
calcPermutation start target
  | start == target = []
  | otherwise = [moves] ++ calcPermutation result target where
      moves = strategizePerm start target
      result = applyRegionMove moves start

permDist :: [String] -> [String] -> Int
permDist state1 state2 = sum (map f state1) where
  indexOf x xs = fromJust (elemIndex x xs)
  f letter = (indexOf letter state1 - indexOf letter state2)^2

calcMerge :: [String] -> [[Move]]
calcMerge [] = []
calcMerge letters0 = f letters0 where
  f letters | separation letters == 0 && nub letters == letters = []
            | otherwise = [concat moves] ++ f letters' where
                segs = toSegs letters
                moves = processSegs segs []
                segs' = applyMoves moves segs
                letters' = toLetters segs'

separation :: [String] -> Int
separation xs = sum subSeps where
  letters = nub xs
  subSeps = map (calcSubSep xs) letters
  calcSubSep xs letter = sum . map length . mySplit letter $ xs
  mySplit l = filter (not . null) . splitOn [l] . dropWhile (/= l) . dropWhileEnd (/= l)


processSegs :: [Segment] -> [Segment] -> [[Move]]
processSegs [] prev = []
processSegs (Glob n s : xs) prev = f n where
  f n | even n = [movesEven] ++ rest
      | odd n = [movesOdd] ++ rest where
          movesEven = replicate (div n 2) Merge 
          movesOdd = (replicate (div n 2) Merge ++ [Id])
          rest = processSegs xs (prev ++ [applyGlobMove (Glob n s)])
processSegs (Region letters : xs) prev =
    [moves] ++ processSegs xs (prev ++ [Region $ applyRegionMove moves letters]) where
      moves = strategize letters prev xs

strategize :: [String] -> [Segment] -> [Segment] -> [Move]
strategize letters posPrev posNext = answer where
  orig = posPrev ++ [Region letters] ++ posNext
  universe = eas (length letters)
  results = map checkScore universe
  answer = pickBest results
  checkScore :: [Move] -> ([Move], Int)
  checkScore moves = (moves, answer) where
    sep = separation (toLetters orig)
    pos' = posPrev ++ [Region $ applyRegionMove moves letters] ++ posNext
    sep' = separation (toLetters pos')
    answer = sep - sep'

strategizePerm :: [String] -> [String] -> [Move]
strategizePerm start target = answer where
  universe = eas (length start)
  results = map checkScore universe
  answer = fst (maximumBy (comparing snd) results)
  checkScore :: [Move] -> ([Move], Int)
  checkScore moves = (moves, answer) where
    dist = permDist start target
    dist' = permDist (applyRegionMove moves start) target
    answer = dist - dist'

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
  f hmm1 hmm2 = error ("applyRegionMove" ++ show hmm1 ++ show hmm2)

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
  | n < 0 = error "eas of negative"
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
