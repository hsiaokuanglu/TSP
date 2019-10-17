import Data.List
import Data.Maybe
import Text.Read

type Matrix = [ [ Double ] ]

-- 5.5
--  1.
numRows :: Matrix -> Int
numRows [] = 0
numRows [[]] = 0
numRows lst = length lst
--  2.
isEmpty :: Matrix -> Bool
isEmpty [] = True
isEmpty lst = foldr (\ x y -> (x == []) && y) True lst

-- try
-- numRows [[1,2],[3,4]]

--  3.
numCols :: Matrix -> Int
numCOls [] = 0
numCols (h:t) = length h

--  4.
rectangular :: Matrix -> Bool
rectangular [] = True
rectangular [a] = True
rectangular (h:t)
    | length h == length (head t)   = rectangular t
    | otherwise                     = False

--  5.
transpose1 :: Matrix -> Matrix
transpose1 lst
    | isEmpty lst = []
    | otherwise = (map head lst):transpose1 (map tail lst)

--  6.
tupleWithf :: (Int -> Int) -> [Int] -> [(Int,Int)]
tupleWithf f [] = []
tupleWithf f (h:t) = (f h, h): tupleWithf f t

-- try
-- tupleWithf (^2) [1,2,3,4,5,6,7,8,9]

--  7.
maxFirst :: (Int,Int) -> (Int,Int) -> (Int,Int)
maxFirst (x1,y1) (x2,y2) = if x1 >= x2 then (x1,y1) else (x2,y2)
minFirst :: (Int,Int) -> (Int,Int) -> (Int,Int)
minFirst (x1,y1) (x2,y2) = if x1 < x2 then (x1,y1) else (x2,y2)
--try
-- maxFirst (5,3) (4,6)
-- maxFirst (1,2) (3,4)
-- maxFirst (0,1) (0,2)

--  8.

maximumFirst :: [(Int,Int)] -> (Int,Int)
maximumFirst [x] = x
maximumFirst (h:t) = maxFirst h (maximumFirst t)

minimumFirst :: [(Int,Int)] -> (Int,Int)
minimumFirst [x] = x
minimumFirst (h:t) = minFirst h (minimumFirst t)

--try
--  maximumFirst [(1,2),(3,5),(5,3),(2,100),(6,3)]

--  9.
maxf :: (Int -> Int) -> [Int] -> Int
maxf f lst = snd (maximumFirst (tupleWithf f lst))
--try
-- maxf (1 `div`) [4,5,6,7,8,9]

--  10.
minf :: (Int -> Int) -> [Int] -> Int
minf f lst = snd (minimumFirst (tupleWithf f lst))

--  11.
maxIndexf :: (Int -> Int) -> [Int] -> Int
maxIndexf f lst = fromMaybe 0 (elemIndex (maxf f lst) lst)

-- try
-- maxIndexf (*1) [0,1,2,3,4,5]
-- maxIndexf (1 `div`) [5,4,3,2,1]

--  12.
square :: Matrix -> Bool
square m = not (isEmpty m) && rectangular m && (numRows m == numCols m)

--  13.
symmetrix :: Matrix -> Bool
symmetrix m = (m == transpose1 m)

-- try
-- symmetrix [[1,2,3],[2,1,4],[3,4,1]]

-- 14.
all0 :: [Double] -> Bool
all0 [x] = x == 0
all0 (h:t) = (h == 0) && all0 t

-- 15.
-- shrink_recur _ i = []
-- shrink_recur (h:t) i
--     | i == numCols (h:t)  = []
--     | otherwise     = h:shrink_recur t (i+1)

-- removeLast [x] = []
-- removeLast (h:t) = h:removeLast(t)
-- shrink m = map removeLast (shrink_recur m 1)

getIndex :: [[a]] -> Int -> a
getIndex m i = m!!i!!i
maindiag :: Matrix -> [Double]
maindiag m = foldr (\ x y -> (getIndex m x):y) [] [0..(numRows m -1)]

-- 16.
triangle :: Matrix -> Bool
triangle m = all0 (maindiag m) && (symmetrix m)

-- try
-- triangle [[0,2,4,7],[2,0,5,9],[4,5,0,1],[7,9,1,0]]

-- Brute Force
ins :: Int -> [Int] -> [[Int]]
ins x [] = [[x]]
ins x (h:t) = (x:h:t): [h:p| p <- ins x t]

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms (h:t) = concat [ins h x | x <- perms t]

-- Cheap Tours
type Town = Int
type Tour = [Town]

--  17.
pairTour [] = []
pairTour lst = zip lst (tail lst)++[(last lst, head lst)]
cost :: Matrix -> Tour -> Double
cost m lst = foldr (\ x y -> m!!(fst x)!!(snd x)+y) 0 (pairTour lst)

-- try
-- cost [[0,7,12,10],[7,0,5,7],[12,5,0,10],[10,7,10,0]] [0,1,2,3]

--  18.
getcost :: Matrix -> [Tour] -> [Double]
getcost m tours = [cost m x | x <- tours]

m1 (x1,y1) (x2,y2) = if x1 < x2 then (x1,y1) else (x2,y2)
m2 [x] = x
m2 (h:t) = m1 h (m2 t)
m3 lst = snd (m2 lst)
m4 lst = fst (m2 lst)

{-
maxFirst :: (Int,Int) -> (Int,Int) -> (Int,Int)
maxFirst (x1,y1) (x2,y2) = if x1 >= x2 then (x1,y1) else (x2,y2)
minFirst :: (Int,Int) -> (Int,Int) -> (Int,Int)
minFirst (x1,y1) (x2,y2) = if x1 < x2 then (x1,y1) else (x2,y2)
-}

cheapest :: Matrix -> [Tour] -> Tour
cheapest m tours = m3 (zip [cost m x | x <- tours] tours)

cheapest_tour_cost :: Matrix -> [Tour] -> (Double, Tour)
cheapest_tour_cost m tours = m2 (zip [cost m x | x <- tours] tours)

perms2 [] = [[]]
perms2 (h:t)
  | (length t) < 3 = [(h:t)]
  | otherwise      = concat [ ins h p | p <- perms2 t]

dist :: [[Double]]
dist = [[0,5,3,6],[5,0,4,5],[3,4,0,3],[6,5,3,0]]
tours = perms [0,1,2,3]

--try
-- cheapest dist tours

-- distance between two inputted coordinates
type Coord = (Int, Int)

-- takes in two coordinates and returns distance between them
inputDist :: Coord -> Coord -> Double
inputDist (x1, y1) (x2, y2) = sqrt ((fromIntegral(x2 - x1))^2 + (fromIntegral (y2 - y1))^2)

-- creates a distance matrix using townDist function
createDistMatrix :: [Coord] -> Matrix
createDistMatrix [] = [[]]
createDistMatrix lst = foldr (\ x y -> (townDist x lst) : y) [] lst

-- computes distance of all coordinates relative to one coordinate
-- making a row of the matrix
townDist :: Coord -> [Coord] -> [Double]
townDist towncoord [] = []
townDist towncoord (h:t) = (inputDist towncoord h) : (townDist towncoord t)

-- input dist Matrix for 4 towns
run4 =
  do
    putStrLn "Enter coordinates for town 1"
    t1 <- getLine
    putStrLn "Enter coordinates for town 2"
    t2 <- getLine
    putStrLn "Enter coordinates for town 3"
    t3 <- getLine
    putStrLn "Enter coordinates for town 4"
    t4 <- getLine
    let distMatrix = createDistMatrix [(read t1),(read t2),(read t3),(read t4)]
    let cheapestTrip = cheapest distMatrix (perms [0,1,2,3])
    putStrLn (show cheapestTrip)
    return ()

ask :: String -> IO String
ask q =
  do
      putStr q
      line <- getLine
      return line

-- input a list of existing towns 'lst' and the number of towns to add 'c'
input_towns :: [Coord] -> Int -> IO ()
input_towns lst c =
  do
    putStrLn ("Tours = " ++ show lst)
    if (c == 0)
      then
        calc lst
      else do
        putStr ("Town " ++ (show (c-1)) ++"'s ")
        z <- ask ("(x,y): ")
        if checkInt z
          then
            input_towns ((readPair z):lst) (c-1)
          else do
            putStrLn ("*Incorrect input*")
            input_towns lst c

splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t


stringIsInt :: String -> Bool
stringIsInt strInt 
    | (readMaybe strInt :: Maybe Int) == Nothing = False
    | otherwise = True


checkInt :: String -> Bool
checkInt st 
  | length st == 0 = False
  | otherwise       = foldr (\x y -> stringIsInt x && y) True (split_with_comma (disregard_brackets st))

build_pair :: String -> [Int]
build_pair s = foldr (\x y -> (read x :: Int) : y) [] (split_with_comma (disregard_brackets s))

readPair :: String -> (Int,Int)
readPair st = (head (build_pair st), last (build_pair st))

split_with_comma :: String -> [String]
split_with_comma (h:t) = splitsep (==',') (h:t)

disregard_brackets :: String -> String
disregard_brackets [] = []
disregard_brackets (h:t)
    | h == '(' = disregard_brackets t
    | h == ')' = disregard_brackets t
    | otherwise = h:(disregard_brackets t) 

-- calculate the cheapestTrip with the given Coordinates of towns
calc :: [Coord] -> IO ()
calc lst =
  do
    let distMatrix = createDistMatrix lst
    let result = cheapest_tour_cost distMatrix (perms2 [0.. (length distMatrix - 1)])
    let cheapestTrip = snd result
    let cheapestCost = fst result
    putStrLn ("Tour: " ++ show cheapestTrip ++ "\nCost: " ++ show cheapestCost)
    return ()

-- input 5 towns
run5 :: IO ()
run5 = input_towns [] 5


nearest :: Matrix -> Town -> [Town] -> Town
nearest dm x (h:t)
    | (dm !! x !! h == minimum[dm !! x !! y | y <- (h:t), (y /= x)]) = h 
    | otherwise = nearest dm x t