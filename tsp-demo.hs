import Data.List
import Data.Maybe
import Text.Read
import Graphics.UI.GLUT
import Data.Function (on)
import Data.List (sortBy)

type Matrix1 = [ [ Double ] ]

-- 5.5
--  1.
numRows :: Matrix1 -> Int
numRows [] = 0
numRows [[]] = 0
numRows lst = length lst
--  2.
isEmpty :: Matrix1 -> Bool
isEmpty [] = True
isEmpty lst = foldr (\ x y -> (x == []) && y) True lst

-- try
-- numRows [[1,2],[3,4]]

--  3.
numCols :: Matrix1 -> Int
numCOls [] = 0
numCols (h:t) = length h

--  4.
rectangular :: Matrix1 -> Bool
rectangular [] = True
rectangular [a] = True
rectangular (h:t)
    | length h == length (head t)   = rectangular t
    | otherwise                     = False

--  5.
transpose1 :: Matrix1 -> Matrix1
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
square :: Matrix1 -> Bool
square m = not (isEmpty m) && rectangular m && (numRows m == numCols m)

--  13.
symmetrix :: Matrix1 -> Bool
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
maindiag :: Matrix1 -> [Double]
maindiag m = foldr (\ x y -> (getIndex m x):y) [] [0..(numRows m -1)]

-- 16.
triangle :: Matrix1 -> Bool
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
cost :: Matrix1 -> Tour -> Double
cost m lst = foldr (\ x y -> m!!(fst x)!!(snd x)+y) 0 (pairTour lst)

-- try
-- cost [[0,7,12,10],[7,0,5,7],[12,5,0,10],[10,7,10,0]] [0,1,2,3]

--  18.
getcost :: Matrix1 -> [Tour] -> [Double]
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

cheapest :: Matrix1 -> [Tour] -> Tour
cheapest m tours = m3 (zip [cost m x | x <- tours] tours)

cheapest_tour_cost :: Matrix1 -> [Tour] -> (Double, Tour)
cheapest_tour_cost m tours = m2 (zip [cost m x | x <- tours] tours)

brute :: Matrix1 -> [Town] -> Tour
brute m towns = m3 (zip [cost m x | x <- (perms towns)] (perms towns))


perms2 [] = [[]]
perms2 (h:t)
  | (length t) < 3 = [(h:t)]
  | otherwise      = concat [ ins h p | p <- perms2 t]

--try
-- cheapest dist tours

-- distance between two inputted coordinates
type Coord = (Int, Int)

-- takes in two coordinates and returns distance between them
inputDist :: Coord -> Coord -> Double
inputDist (x1, y1) (x2, y2) = sqrt ((fromIntegral(x2 - x1))^2 + (fromIntegral (y2 - y1))^2)

-- creates a distance Matrix1 using townDist function
createDistMatrix1 :: [Coord] -> Matrix1
createDistMatrix1 [] = [[]]
createDistMatrix1 lst = foldr (\ x y -> (townDist x lst) : y) [] lst

-- computes distance of all coordinates relative to one coordinate
-- making a row of the Matrix1
townDist :: Coord -> [Coord] -> [Double]
townDist towncoord [] = []
townDist towncoord (h:t) = (inputDist towncoord h) : (townDist towncoord t)

ask :: String -> IO String
ask q =
  do
      putStr q
      line <- getLine
      return line

-- input a list of existing towns 'lst' and the number of towns to add 'c'
input_towns :: (Matrix1 -> [Town] -> Tour) -> [Coord] -> Int -> IO ()
input_towns f lst c =
  do
    putStrLn ("Tours = " ++ show lst)
    if (c == 0)
      then
        calc f lst
      else do
        putStr ("Town " ++ (show (c-1)) ++"'s ")
        z <- ask ("(x,y): ")
        if checkInt z
          then
            input_towns f ((readPair z):lst) (c-1)
          else do
            putStrLn ("*Incorrect input*")
            input_towns f lst c

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


-- calculate TSP with the given list of Coordinates and the given algorithm f
calc f lst =
  do
    let distMatrix1 = createDistMatrix1 lst
    let result_f = f distMatrix1 [0.. (length distMatrix1 - 1)]
    let result_f_cost = cost distMatrix1 result_f
    putStrLn ("Tour: "++ show result_f
                ++ "\nCost: " ++ show result_f_cost)
    return ()

just_tour :: (Matrix1 -> [Town] -> Tour) -> [Coord] -> Tour
just_tour f lst = f distMatrix1 [0.. (length distMatrix1 - 1)]
                where distMatrix1 = createDistMatrix1 lst
-- input 5 towns
run5 :: IO ()
run5 = input_towns brute [] 5

-- nearest dm x ys gives the town from ys which is nearest to the
-- town x according to the distance Matrix1 dm
nearest :: Matrix1 -> Town -> [Town] -> Town
nearest dm x (h:t)
    | (dm !! x !! h == minimum[dm !! x !! y | y <- (h:t), (y /= x)]) = h
    | otherwise = nearest dm x t

-- nn dm xs returns a tour containing all towns from xs, starting
-- from thefirst town in xs and then picking the town nearest to the
-- last town visited at every step.
nn :: Matrix1 -> [Town] -> Tour
nn dm (h:t) = nnLoop dm t [h]

-- auxiliary function which takes in the list of towns visited so far
nnLoop :: Matrix1 -> [Town] -> Tour -> Tour
nnLoop dm [] tour = tour
nnLoop dm lst tour = nnLoop dm (delete (nearest dm (last tour) lst) lst)
                      (tour ++ [nearest dm (last tour) lst])

-- returns a tour obtained by inserting t somewhere in ts
-- (without reordering ts itself), choosing of all the possibilities
-- for doing so which has minimal total distance according to
-- the distance Matrix1 dm
insTown :: Matrix1 -> Tour -> Town -> Tour
insTown dm [] t = [t]
insTown dm ts t = cheapest dm (ins t ts)

-- adds all towns from us to the tour ts, at each step adding the head
-- of us using insTown until us is empty (in which case it returns ts).
addTowns :: Matrix1 -> Tour -> [Town] -> Tour
addTowns m ts [] = ts
addTowns m ts (h:t) = addWithTour m ts t (insTown m ts h)

addWithTour :: Matrix1 -> Tour -> [Town] -> Tour -> Tour
addWithTour m ts [] toursofar = toursofar
addWithTour m ts (h:t) toursofar = addWithTour m ts t (insTown m toursofar h)

-- aT m ts returns a tour constructed by repeated use of insTown
aT :: Matrix1 -> [Town] -> Tour
aT m [] = []
aT m ts = addTowns m [] ts

-- remove a town from a list of towns, ie. removeTown t ts returns a list
-- containing all towns in ts except t. Assume t occurs exactly once in ts
removeTown :: Town -> [Town] -> [Town]
removeTown town [] = []
removeTown town (h:t)
    | h == town = t
    | otherwise = h : removeTown town t

-- the remoteness of a town with respect to a list of towns and a distance
-- Matrix1 is the sum of the distances between the town each each town in
-- the list of towns
remoteness :: Matrix1 -> [Town] -> Town -> Double
remoteness dm [] town = 0
remoteness dm (h:t) town = dm!!h!!town + (remoteness dm t town)


-- remotest returns the most remote town in a list of towns
-- such that remoteness m ts u is the highest of all
remotest :: Matrix1 -> [Town] -> [Town] -> Town
remotest dm ts (h:t)
      | (remoteness dm ts h == maximum [remoteness dm ts x | x <- (h:t)]) = h
      | otherwise = remotest dm ts t

-- repeatedly adding the remotest town relative to the Matrix1 and the
-- tour constructed so far
aRT1 :: Matrix1 -> [Town] -> Tour
aRT1 m ts = aRT1WithTours m (delete (remotest m ts ts) ts) [remotest m ts ts]

-- helper function for aRT1 that keeps track of the tour so far
aRT1WithTours :: Matrix1 -> [Town] -> Tour -> Tour
aRT1WithTours m [] toursofar = toursofar
aRT1WithTours m ts toursofar = aRT1WithTours m (delete (remotest m toursofar ts) ts)
                                (toursofar ++ [remotest m toursofar ts])

sortByRemoteness :: Matrix1 -> [Town] -> [Town]
sortByRemoteness m ts = map fst (sortZippedRemoteness (zip ts (remotenessList m ts)))


remotenessList :: Matrix1 -> [Town]  -> [Double]
remotenessList m ts = [remoteness m ts x | x <- ts]

sortZippedRemoteness :: [(Town, Double)] -> [(Town, Double)]
sortZippedRemoteness zipped = sortBy (flip compare `on` snd) zipped

aRT2 :: Matrix1 -> [Town] -> Tour
aRT2 m ts = aT m (sortByRemoteness m ts)
-- visual

gridSize :: Float
gridSize = 10

getScale :: [Coord] -> Float
getScale lst = fromIntegral (foldr (\x y -> if x >= y then x else y) 0 (foldr (\x y -> if fst x >= snd x then fst x:y else snd x:y) [] lst ))

gridR :: Float -> [GLfloat]
gridR gridSize = foldr (\x y -> x:x:y) [] [ x / gridSize | x <- [-gridSize..gridSize] ]
gridL :: Float -> [GLfloat]
gridL gridSize = foldr (\x y -> -1:1:y) [] [ x / gridSize | x <- [-gridSize..gridSize] ]
gridLineX :: Float -> [(GLfloat,GLfloat,GLfloat)]
gridLineX gridSize = [ (fst x, snd x, 0::GLfloat) | x <- zip (gridL gridSize) (gridR gridSize)]
gridLineY :: Float -> [(GLfloat,GLfloat,GLfloat)]
gridLineY gridSize = [ (fst x, snd x, 0::GLfloat) | x <- zip (gridR gridSize) (gridL gridSize)]


type SCoord = (Float, Float)

nn_tour :: Tour
nn_tour = [0,8,1,9,2,4,7,6,3,5]

brute_tour :: Tour
brute_tour = [9,1,8,0,5,7,4,6,3,2]

smallMap :: [Coord]
smallMap = [(2,1),(6,2),(3,5),(1,3)]

smallTour :: Tour
smallTour = [2,1,0,3]

-- scale coordinate
scale_coord :: [Coord] -> Float -> [SCoord]
scale_coord lst s= [( (fromIntegral (fst x) / s + offset ) , ( fromIntegral (snd x) / s + offset)) | x <- lst]
              where offset = -1

-- pair town number with its coordinate
town_record :: [SCoord] -> [(Town, SCoord)]
town_record lst = zip [0..((length lst)-1)] lst


coord_lookup :: Town -> [(Town, SCoord)] -> SCoord
coord_lookup x [] = (0,0)
coord_lookup x (h:t)
  | x == fst h  = snd h
  | otherwise   = coord_lookup x t

tour_to_coord :: Tour -> [(Town, SCoord)] -> [SCoord]
tour_to_coord t lst = foldr (\x y -> (coord_lookup x lst):y) [] t

getX :: [SCoord] -> [Float]
getX lst = [(fst x) | x <- lst]

getY :: [SCoord] -> [Float]
getY lst = [(snd x) | x <- lst]

tour_to_vector :: Tour -> [(Town, SCoord)] -> [(GLfloat,GLfloat,GLfloat)]
tour_to_vector t lst = [ (fst x, snd x, 0) | x <- zip (getX lst2) (getY lst2)]
                  where lst2 = tour_to_coord t lst

myLoop :: [(GLfloat,GLfloat,GLfloat)]
myLoop = [ (fst x, snd x, 0) | x <- zip [0,0.5,0.5] [0,0.5,-0.5]]

win_size :: Size
win_size = Size 500 500

show_tour :: (Matrix1 -> [Town] -> Tour) -> [Coord] -> IO ()
show_tour f lst = do
  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize $= win_size
  initialWindowPosition $= Position 400 130
  _window <- createWindow "TSP"
  displayCallback $= display f lst
  actionOnWindowClose $= MainLoopReturns
  mainLoop   

display :: (Matrix1 -> [Town] -> Tour) -> [Coord] -> DisplayCallback
display f lst = do 
  let scaleFactor = fromIntegral(floor (getScale lst / 2)+ 1) :: GLfloat
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  let xLines = gridLineX scaleFactor
  let yLines = gridLineY scaleFactor
  
  pointSize $= 6
  lineWidth $= 1
  clear [ColorBuffer]
  renderPrimitive Lines $ do
    -- gridlines
    color3f 0 0 1
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) xLines
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) yLines
  lineWidth $= 2
  renderPrimitive Lines $ do
    -- x, y-axis lines
    color3f 0 1 0
    --let axis = (-1 + 0.5/(scaleFactor/2))
    let axis = -1
    vertex $ (Vertex3 (-1) axis 0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (1) axis 0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 axis (-1) 0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 axis (1) 0 :: Vertex3 GLfloat)
  -- tour lines
  renderPrimitive LineLoop $ do
    color3f 1 1 1
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (tour_to_vector (just_tour f lst) (town_record (scale_coord lst scaleFactor)))
  --
  renderPrimitive Points $ do
    color3f 1 1 0
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (tour_to_vector (just_tour f lst) (town_record (scale_coord lst scaleFactor)))
  flush

dist :: [[Double]]
dist = [[0,5,3,6],[5,0,4,5],[3,4,0,3],[6,5,3,0]]
tours = perms [0,1,2,3]
mapT :: [Coord]
mapT = [(1,5),(6,4),(10,2),(34,3),(16,14),(4,3),(1,1),(8,3),(9,12),(8,14),(13,6),(8,15),(23,3),(13,17),(21,14),(16,18)]
mapT2 :: [Coord]
mapT2 = [(1,5),(6,4),(10,2),(4,3),(1,1),(8,3),(9,12),(8,14),(13,6),(8,15)]
cali :: [Coord]
cali = [(0,0),(0,1),(1,1),(1,0),(3,3)]
bigMap :: [Coord]
bigMap = zip ([1..25]++[25,24..1]) [1..50]

-- demo 
-- try
-- input_towns brute [] 5

-- :set +s
-- calc brute mapT
-- calc nn mapT
-- calc aT mapT
-- calc aRT1 mapT

-- show_tour nn mapT
testAll = do
  show_tour nn mapT
  show_tour nn mapT2
  show_tour nn bigMap
test1 = show_tour nn mapT
test2 = show_tour nn mapT2
test3 = show_tour nn bigMap
test4 = show_tour aT mapT
test5 = show_tour aT mapT2
test6 = show_tour aT bigMap

demoAll = 
  do
    demo_nn
    demo_aT
    demo_aRT2

demo_brute = show_tour brute mapT
demo_nn = show_tour nn mapT
demo_aT = show_tour aT mapT
demo_aRT2 = show_tour aRT2 mapT