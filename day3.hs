{-
Module: Day 3
Description: Solution to the Advent of Code Day3 problem. (Two) wires are given as a list of directions for each segment of the wire, e.g. "R 7" denotes the wire continuing 7 spaces to the Right from where it currently is. The goal is to find the closest overlapping segments of wire from the origin.
Maintainer: Nate Launchbury 
 
-}



import qualified Data.Map as Map
import Data.Map(Map)
import Data.List
import Control.Applicative
import Library

-- Note: this currently takes minutes to run: Closest point: (970,1159) at distance 2129
part1 file = do 
  input <- readFile file
  let segs = map (run parseSegments) (lines input)
  let wires = map translatePath segs
  let diagram = storePaths wires Map.empty
  let closest = nearestOrigin diagram
  putStrLn $ "Closest point: " ++ show closest ++ 
             " at distance " ++ show (manhattanDist (0,0) closest)

-- Note: overlaps = [(970,1159),(970,1387),(1381,1428),(1381,1565),(1403,1850),(1906,1428),(2072,1383),(2326,1383),(2487,1383),(2511,1383),(2546,1383),(3385,1259),(3791,1598),(3791,1974),(4052,1974),(4303,1752),(4503,1752)]
part2 file = do 
  input <- readFile file
  let segs = map (run parseSegments) (lines input)
  let wires = map translatePath segs
  let diagram = storePaths wires Map.empty
  let overs = overlaps diagram
  putStrLn (show overs)  


data Segment = U Int | D Int | L Int | R Int
  deriving Show

makeSegment :: Char -> Int -> Segment
makeSegment 'U' n = U n 
makeSegment 'D' n = D n 
makeSegment 'L' n = L n 
makeSegment 'R' n = R n 
makeSegment  _  n = error "illegitimate segment direction"


-- |Parsing ------------------------------
parseSegment = do 
  d <- one
  n <- num
  return (makeSegment d n)

parseSegments = many ((parseSegment <* lit ',') <|> parseSegment)


-- |Computation ------------------------------

-- |Coords naturally have the origin in the top-left corner, we can swap the 
-- up and down functions to make this the bottom-left corner instead 
destructSegment :: Segment -> (Coord -> Coord, Int)
destructSegment (U n) = (down, n)
destructSegment (D n) = (up, n)
destructSegment (L n) = (left, n)
destructSegment (R n) = (right, n)

-- |A path crossing with itself doesn't count so we remove duplicates from a path
translatePath :: [Segment] -> [Coord]
translatePath ds = nub $ translatePathH ds (0,0)

translatePathH :: [Segment] -> Coord -> [Coord]
translatePathH [] _ = []
translatePathH (d:ds) c = let (f,n) = destructSegment d 
                              c' = take (n+1) (iterate f c) 
                          in  c' ++ (translatePathH ds (last c'))

-- |Store a path in a map marking how many times each coord has been visited 
storePath :: [Coord] -> Map Coord Int -> Map Coord Int
storePath [] m = m
storePath (c:cs) m = storePath cs (Map.insertWith (+) c 1 m)

-- |Stores a list of Paths in a map using 'StorePath'
storePaths :: [[Coord]] -> Map Coord Int -> Map Coord Int
storePaths [] m = m
storePaths (cs:css) m = storePaths css (storePath cs m)

-- |Find the overlap nearest to (0,0) using 'manhattanDist'
nearestOrigin :: Map Coord Int -> Coord
nearestOrigin m = head $ sortOn (manhattanDist (0,0)) (overlaps m)
                  
-- |Gives all points where at least two paths cross 
overlaps :: Map Coord Int -> [Coord]
overlaps m = filter (/=(0,0)) (Map.keys (Map.filter (>1) m))

-- |Manhattan distance metric 
manhattanDist :: Coord -> Coord -> Int
manhattanDist (a,b) (x,y) = abs (a-x) + abs (b-y) 






-- |Examples ------------------------------
exwire1 = [R 8, U 5, L 5, D 3]
exwire2 = [U 7, R 6, D 4, L 4]
ex1 = storePath (translatePath exwire1) Map.empty
ex2 = storePath (translatePath exwire2) ex1



