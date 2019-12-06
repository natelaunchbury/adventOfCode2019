{-
Module: Library
Description: A library for the Advent of Code 2018 problems. Contains applicative parsing, queues, coordinate definitions and functions, printing and animations, and some other useful functions. 
Maintainer: Nate Launchbury 
 -}

module Library where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.List
import Codec.Picture
import Data.Map(Map)
import qualified Data.Map as Map

-- Library for Advent of Code 

--main = undefined

 -- -- -- -- -- ------------ Parsing ----------------- -- -- -- -- --

newtype Parser s a = Parser {apply :: [s] -> Maybe (a,[s])}

instance Monad (Parser s)  where
  return x = Parser $ \xs -> Just (x,xs) 
  m >>= k  = Parser $ \xs -> case apply m xs of
                              Nothing -> Nothing 
                              Just (x,xs') -> (apply (k x) xs')

instance Functor (Parser s) where
  fmap f p = return f <*> p

instance Applicative (Parser s) where
  pure    = return
  p <*> q = do {f <- p; x <- q; return (f x)}

instance Alternative (Parser s) where
  empty   = Parser $ \xs -> Nothing
  p <|> q = Parser $ \xs -> case apply p xs of 
                             Nothing -> apply q xs 
                             Just r  -> Just r

-- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- primitives 

 -- return : creates a parser              :: a -> Parser s a
 -- empty  : always fails parser           :: Parser s a

one :: Parser s s
one = Parser $ \xs -> case xs of 
                       []      -> Nothing
                       (x:xs') -> Just (x,xs')

sat :: (a -> Bool) -> Parser a a 
sat p = Parser $ \xs -> case xs of 
                         []     -> Nothing
                         (x:xs) -> if p x then Just (x,xs) else Nothing 


-- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- combinators 

 -- <*>  : applicative sequencing        :: Parser s (a -> b) -> Parser s a -> Parser s b 
 -- <|>  : left-prejudice alternative    :: Parser s a -> Parser s a -> Parser s a
 -- <*   : discards RHS                  :: Parser s a -> Parser s b -> Parser s a
 --  *>  : discards LHS                  :: Parser s a -> Parser s b -> Parser s b
 -- many : runs p repeatedly to failure  :: Parser s a -> Parser s [a]
 -- some : fails instead of returning [] :: Parser s a -> Parser s [a]
 
lit c = sat (==c) 

space = lit ' '

opt d p = p <|> return d 


num :: Parser Char Int 
num = pure read <*> some (sat isDigit) 

num' :: Parser Char Int 
num' = do 
  n <- some (sat isDigit) 
  return (read n)
    
-- grabs next int from stream, can be negative 
nextInt :: Parser Char Int
nextInt = do 
  cs <- goNum
  n <- some (sat isDigit)
  if last cs=='-'
    then return (read ('-':n))
    else return (read n) 
       
currentInt :: Parser Char Int 
currentInt = do 
  n   <- one 
  num <- many (sat isDigit)
  return (read (n:num)) 

takeUntil :: (a -> Bool) -> Parser a [a] 
takeUntil p = many (sat (not . p))

takeUntilAny :: [(a -> Bool)] -> Parser a [a]
takeUntilAny ps = many (sat (not . (\c -> or [p c | p <- ps])))

  
skipPast c = takeUntil (==c) *> one

goNum :: Parser Char [Char]
goNum = takeUntil isDigit

pair p c = pure f <*> p <* lit c <*> p
  where f x y = (x,y) 

pair' p c = do 
   a <- p
   _ <- lit c
   b <- p
   return (a,b)

str :: String -> Parser Char String  
str [] =  pure [] 
str (s:ss) = pure (:) <*> lit s <*> str ss

str' [] = return []
str' (s:ss) = do 
   c  <- lit s
   cs <- str' ss
   return (s:ss) 

word :: Parser Char String
word = some (sat isLetter) 

goWord :: Parser Char String
goWord = takeUntil isLetter

commaList p = many (p <* (lit ',' *> space) <|> p)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- running

run p xs = case apply p xs of 
            Nothing      -> error "No parse"
            Just (x,xs') -> x


-- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- examples
-- from Advent of Code 2018 day 3
-- parse "#1 @ 3,1: 4x4" into (1,(3,1),(4,4)) or similar

libEx = "#1 @ 3,1: 4x4"

exLine = pure f       <* lit '#' <*>
         num          <* goNum   <*>
         pair num ',' <* goNum   <*>
         pair num 'x'
    where f = (,,) 

exLine' = do 
  _    <- goNum
  n    <- num
  _    <- goNum
  loc  <- pair num ','
  _    <- goNum
  size <- pair num 'x'
  return (n,loc,size)


 -- -- -- -- -- ------------ Queue ----------------- -- -- -- -- --
 
type Queue a = ([a],[a])

-- |Predicate to test if a queue is empty
isEmpty :: Queue a -> Bool
isEmpty ([],[]) = True
isEmpty _ = False

-- |The empty queue
empty :: Queue a
empty = ([],[])

-- |Takes the front of the queue and returns it and the remaining
front :: Queue a -> (a, Queue a) 
front ((a:as),bs) = (a, (as,bs))
front ([],bs) = front (reverse bs,[])

-- |Puts an element on the back of the queue 
back :: Queue a -> a -> Queue a 
back (as,bs) x = (as,x:bs) 
 
-- |Puts a list on the back of a queue, in order
enqueue :: Queue a -> [a] -> Queue a
enqueue ([],[]) xs = (xs,[])
enqueue q xs = foldl back q xs 

-- |The size of a queue 
size :: Queue a -> Int
size (as,bs) = length as + length bs



 -- -- -- -- -- ------------ Coords ----------------- -- -- -- -- --

type Coord = (Int,Int)

-- |Makes a coordinate grid from (0,0) to (a,b) sorted on the x-values (reading order) 
--
-- > coords 2 2
--   [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] 
coords :: Int -> Int -> [Coord]
coords a b = sortOn snd [(x,y) | x <- [0..a], y <- [0..b]]

-- |Makes a coordinate grid from a given top-left and bottom-right corner
coordsWithin :: Coord -> Coord -> [Coord]
coordsWithin (lox,loy) (hix,hiy) = sortOn snd [(x,y) | x <- [lox..hix], y <- [loy..hiy]]

-- |Comparison function for reading-ordering of Coords
compareCoord :: (Coord,a) -> (Coord,a) -> Ordering
compareCoord ((u,v),_) ((x,y),_)
  | v < y = LT
  | v > y = GT
  | u < x = LT
  | u > x = GT
  | True  = EQ

-- |The top-left and bottom-right coords from a list (smallest box which contains all points) 
boundingBox :: [Coord] -> (Coord,Coord)
boundingBox cs = let lox = minimum (map fst cs)
                     hix = maximum (map fst cs) 
                     loy = minimum (map snd cs) 
                     hiy = maximum (map snd cs) 
                 in ((lox,loy), (hix,hiy))

-- |Gives all the coords between a pair of coords 
coordSpan :: (Coord,Coord) -> [Coord]
coordSpan (lo,hi) = coordsWithin lo hi 

-- |Simple functions for manipulating Coords 
up,left,right,down :: (Coord -> Coord) 
up (x,y) = (x,y-1)
left (x,y) = (x-1,y)
right (x,y) = (x+1,y)
down (x,y) = (x,y+1)

-- |Returns the adjacent coords (no diagonals) 
adjacent :: Coord -> [Coord]
adjacent c = [up c, left c, right c, down c]

-- |Returns all 8 neighboring coords 
neighbors :: Coord -> [Coord]
neighbors c = adjacent c ++ [up (left c), down (left c), up (right c), down (right c)]

 -- -- -- -- -- ------------ Printing/Animation ----------------- -- -- -- -- --

-- |Turns a Map into a string given a supplied display function using `outputMapH`
outputMap :: Map Coord a -> (a -> Char) -> String
outputMap map display = let ordered = sortBy compareCoord (Map.toList map)
                        in tail $ outputMapH ordered display []

-- |Alternate version of `outputMap` which allows a list of special rules for specific coordinates
outputMapWithSpecials :: Map Coord a -> (a -> Char) -> [(Coord,Char)] -> String
outputMapWithSpecials map display specials = let ordered = sortBy compareCoord (Map.toList map)
                                      in tail $ outputMapH ordered display specials

-- |Adds appropriate line breaks while applying the display function wherever special printing is not used 
outputMapH :: [(Coord,a)] -> (a -> Char) -> [(Coord,Char)] -> String
outputMapH [] _ _ = ""
outputMapH (((x,y),a):cs) display specials = 
  case lookup (x,y) specials of 
     Just s  -> if x==0 then '\n':s:rest else s:rest
     Nothing -> if x==0 then '\n': display a : rest else display a : rest
  where rest = outputMapH cs display specials


-- |Prints a list, with each element on its own line 
printLines p = mapM_ putStrLn (map show p) 

-- |Generic function to make drawing a coordinate map more convenient (Eric) 
drawCoords :: Pixel p => (Coord,Coord) -> (Coord -> p) -> Image p 
drawCoords ((lox,loy), (hix,hiy)) f = generateImage toPixel width height
  where toPixel x y = f (lox+x,loy+y)
        width = hix - lox + 1
        height = hiy - loy + 1


-- |Generic function to write a gif from a list of images (Eric) 
writeAnimation :: String -> Int -> [Image PixelRGB8] -> IO ()
writeAnimation file delay imgs = 
  case writeGifAnimation file delay LoopingForever imgs of 
     Left e -> fail e
     Right io -> io 



 -- -- -- -- -- ------------ Functions ----------------- -- -- -- -- --

-- |Iterate a function to a fixed point
limit :: Eq a => (a -> a) -> a -> a
limit f x 
  | x' == x   = x 
  | otherwise = limit f x'
  where x' = f x

-- |Iterate a function until a fixed point can be found using m 
-- Ex. limitUsing length functionOnLists initialList 
limitUsing :: Eq b => (a -> b) -> (a -> a) -> a -> a
limitUsing m f x 
  | m x' == m x = x
  | otherwise   = limitUsing m f x'
  where x' = f x 

-- |Apply a function to an index of a list 
listApply :: (a -> a) -> Int -> [a] -> [a] 
listApply _ _ [] = error "index too large in Library.listApply" 
listApply f 0 (x:xs) = f x : xs
listApply f n (x:xs) = x : listApply f (n-1) xs






