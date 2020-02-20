module IntComp where 

import Data.Array
import Data.IntMap(IntMap)
import qualified Data.IntMap as Map
import Data.List
import Text.Megaparsec (Parsec, sepBy, parse, errorBundlePretty)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Void


main = undefined

-- ToDo: internalInput function
--       executeC via executeSkeleton framework 
--       rewrite runAmpsFeedback to not need -1 PC on Halt (just check if "get p pc == Halt")

-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- types
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- | The 'tape' stores the initial program state as an immutable array 
-- while the 'diff' tracks changes as an int map
-- the 'inps' stores input values in a list 
-- and the 'outp' stores output values in a list 
data Program = Program { tape :: Array Int Int
                       , diff :: IntMap Int
                       , inps :: [Int]
                       , outp :: [Int]
                       }
  deriving Show 

data Operation = Add | Mul | Inp | Out | Jt | Jf | Lt | Eq | RBO | Halt
  deriving (Show, Eq)

data PMode = Pos | Imm | Rel 
  deriving Show 

type Parameters = (PMode, PMode, PMode)

-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- execution
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- | General case for program execution, relying on the Output operation 
execute :: Program -> IO ()
execute p = do 
  _ <- executeH p 
  putStrLn "Done"

-- | Program execution which shows the result (used before Output command)
executeO :: Program -> IO ()
executeO p = do 
  r <- executeS p 
  putStrLn $ show (result r) 

-- | A special case of execution where the first two arguments are changed 
executeWithArguments :: Program -> Int -> Int -> IO (Program)
executeWithArguments p a b = do
   let p'  = set p  1 a
   let p'' = set p' 2 b
   executeH p''

-- |Framework for program execution, used to implement execution patterns 
executeSkeleton :: Program                          -- ^The program being executed 
                -> Int                              -- ^The program counter (starts at 0) 
                -> Int                              -- ^The relative base (starts at 0) 
                -> (Program -> IO (Int,Program))    -- ^Function used to obtain input 
                -> (Program -> Int -> IO (Program)) -- ^Function used to display output 
                -> (Program -> Int -> Int -> IO ()) -- ^Debugging function 
                -> IO (Program,Int,Int)             -- ^Program, program counter, relative base 
executeSkeleton p pc rb finput foutput fdebug = do
  fdebug p pc rb 
  case instr of
    Halt -> do return (p,pc,rb) 
    Add  -> do executeSkeleton (calculate Add (a,b,c) p pc rb) (pc+4) rb finput foutput fdebug
    Mul  -> do executeSkeleton (calculate Mul (a,b,c) p pc rb) (pc+4) rb finput foutput fdebug
    Inp  -> do (inp,p') <- finput p;  
               let p'' = set p' (modeToPos a p' rb (get p (pc+1))) inp
               executeSkeleton p'' (pc+2) rb finput foutput fdebug
    Out  -> do p' <- foutput p (modeToVal a p rb (get p (pc+1)))
               executeSkeleton p' (pc+2) rb finput foutput fdebug
    Jt   -> let pc' = jumpOn p (/=0) (a,b,c) pc rb in executeSkeleton p pc' rb finput foutput fdebug 
    Jf   -> let pc' = jumpOn p (==0) (a,b,c) pc rb in executeSkeleton p pc' rb finput foutput fdebug 
    Lt   -> executeSkeleton (calculate Lt (a,b,c) p pc rb) (pc+4) rb finput foutput fdebug 
    Eq   -> executeSkeleton (calculate Eq (a,b,c) p pc rb) (pc+4) rb finput foutput fdebug 
    RBO  -> let rb' = newbase p (a,b,c) pc rb in executeSkeleton p (pc+2) rb' finput foutput fdebug 
  where (instr, (a,b,c)) = unpackage (get p pc) 

-- |Execution pattern with silent input/output and no debugging 
executeS p = do 
  (p',pc',rb') <- executeSkeleton p 0 0 immediateSilentInput silentOutput noDebug 
  return p' 

-- |(Default) execution pattern with silent input but printing output and no debugging 
executeH p = do 
  (p',pc',rb') <- executeSkeleton p 0 0 immediateSilentInput printOutput noDebug 
  return p' 

-- |Execution pattern printing input and output and prompting/printing at each step 
executeI p = do 
  (p',pc',rb') <- executeSkeleton p 0 0 immediateInput printOutput promptDebug 
  return p' 

-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- input, output, debugging 
-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- placeholders during refactoring 
output = printOutput
input = immediateInput
prompt = promptDebug

-- |Input function which gets user input if no internal input is available, printing if it is 
-- ToDo: use Haskeline instead
immediateInput :: Program -> IO (Int,Program)
immediateInput p = 
  case inps p of 
     [] -> do 
         putStr "Please enter an int: "
         n <- getLine
         return (read n,p)
     (i:is) -> do   
         putStrLn $ "Input used: " ++ show i 
         return (i, Program {tape = tape p, diff = diff p, inps = is, outp = outp p})

-- |Input function which gets user input if no internal input is available 
-- ToDo: use Haskeline instead
immediateSilentInput :: Program -> IO (Int,Program)
immediateSilentInput p = 
  case inps p of 
     [] -> do 
         putStr "Please enter an int: "
         n <- getLine
         return (read n,p)
     (i:is) -> do   
         return (i, Program {tape = tape p, diff = diff p, inps = is, outp = outp p})

--internalInput :: Program -> IO (Int,Program)
--internalInput p = case inps p of 
--                    [] -> return (


-- |Output function which updates the Program but doesn't print anything 
silentOutput p o = return (Program {tape = tape p, diff = diff p, inps =  inps p, outp = o:outp p})

-- |Output function which updates the Program and prints
printOutput :: Program -> Int -> IO (Program)
printOutput p n = do
  let p' = Program {tape = tape p, diff = diff p, inps = inps p, outp = n:outp p}
  putStrLn $ "Output: " ++ (show n) 
  return p'


-- |Trivial debug function 
noDebug _ _ _ = return () 

-- |Degub function which displays program state then waits for the user 
promptDebug :: Program -> Int -> Int -> IO ()
promptDebug p pc rb = do 
   putStrLn $ show p ++ " PC: " ++ show pc ++ ", RB: " ++ show rb 
   _ <- getLine 
   return ()

-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- amplifiers  
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- | Execution that passes a continuation when input is needed from the console
executeC :: Program -> Int -> Int -> IO (Int, Program)
executeC p pc rb 
  | instr == Halt = return ((-1),p)   -- distinguish Halt from awaiting input 
  | instr == Add  = executeC (calculate Add (a,b,c) p pc rb) (pc+4) rb 
  | instr == Mul  = executeC (calculate Mul (a,b,c) p pc rb) (pc+4) rb 
  | instr == Inp  = if null (inps p)
                     then return (pc, p) 
                     else do (inp,p') <- input p; 
                             executeC (set p' (modeToPos a p' rb (get p' (pc+1))) inp) (pc+2) rb
  | instr == Out  = do p' <- output p (modeToVal a p rb (get p (pc+1))); executeC p' (pc+2) rb 
  | instr == Jt   = let pc' = jumpOn p (/=0) (a,b,c) pc rb in executeC p pc' rb
  | instr == Jf   = let pc' = jumpOn p (==0) (a,b,c) pc rb in executeC p pc' rb 
  | instr == Lt   = executeC (calculate Lt (a,b,c) p pc rb) (pc+4) rb 
  | instr == Eq   = executeC (calculate Eq (a,b,c) p pc rb) (pc+4) rb 
  | instr == RBO  = let rb' = newbase p (a,b,c) pc rb in executeC p (pc+2) rb'
  | otherwise = error $ "tape at " ++ show pc ++ " encountered " ++ show (tape p ! pc)
  where (instr, (a,b,c)) = unpackage (get p pc)


-- |Runs the amplifiers sequentially, providing the phase and previous output as input 
runAmps :: Program           -- ^The program to be executed  
        -> [Int]             -- ^The phase order 
        -> Int               -- ^The previous amp's output 
        -> IO (Int)          -- ^The final amp's output 
runAmps p []     _   = return (head (outp p))
runAmps p (n:ns) out = do 
    p' <- executeH (Program {tape=tape p, diff=diff p, inps=n:out:inps p, outp = []}) 
    runAmps p' ns (head (outp p'))

-- |Run the amplifiers "in parallel" so that non-final output is given to the next amp 
runFeedbackAmps :: [(Int,Program)]  -- ^Program queue
                -> [Int]            -- ^The phase order
                -> Int              -- ^The previous amp's output
                -> IO (Int)         -- ^The final amp's output
runFeedbackAmps (((-1),p):ps) [] out = return out  -- this occurs 1 round after first Halt 
runFeedbackAmps ((pc,p):ps) [] out = do
    (pc',p') <- executeC (Program {tape=tape p, diff=diff p, inps=out:inps p, outp=[]}) pc 0 
    runFeedbackAmps (ps ++ [(pc',p')]) [] (head (outp p'))
runFeedbackAmps ((pc,p):ps) (n:ns) out = do 
    (pc',p') <- executeC (Program {tape=tape p, diff=diff p, inps=n:out:inps p, outp=[]}) pc 0 
    runFeedbackAmps (ps ++ [(pc',p')]) ns (head (outp p'))


    

-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- functionality 
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- | unpackages an instruction into the operation and parameter modes of the arguments
unpackage :: Int -> (Operation, Parameters)
unpackage n = let o = opcode (n `mod` 100)
                  a = paraMode (2 `thDigit` n)
                  b = paraMode (3 `thDigit` n)
                  c = paraMode (4 `thDigit` n)
              in (o, (a,b,c))

-- | Returns the n-th digit of a number; right-to-left from 0 and supporting leading 0's
-- >>> 4 `thDigit` 15302
-- 1
thDigit :: Int -> Int -> Int 
thDigit i n = n `div` (10^i) `mod` 10 

-- |Opcode decoder 
opcode :: Int -> Operation
opcode 1 = Add
opcode 2 = Mul
opcode 3 = Inp
opcode 4 = Out 
opcode 5 = Jt
opcode 6 = Jf
opcode 7 = Lt
opcode 8 = Eq
opcode 9 = RBO
opcode 99 = Halt 
opcode n  = error $ "invalid opcode: " ++ show n 

-- |Parameter mode decoder 
paraMode :: Int -> PMode
paraMode 0 = Pos
paraMode 1 = Imm 
paraMode 2 = Rel

-- |Computes the binary operation after resolving the parameter modes of the arguments 
calculate :: Operation -> Parameters -> Program -> Int -> Int -> Program 
calculate op (a,b,c) p pc rb = let val1  = modeToVal a p rb (get p (pc+1)) 
                                   val2  = modeToVal b p rb (get p (pc+2)) 
                                   store = modeToPos c p rb (get p (pc+3))
                               in set p store (compute op val1 val2)

-- |Given a parameter mode, returns the immediate value 
modeToVal :: PMode -> Program -> Int -> Int -> Int
modeToVal Pos p rb i = get p i
modeToVal Imm p rb i = i 
modeToVal Rel p rb i = get p (rb + i)

-- |Given a parameter mode, returns the position 
modeToPos :: PMode -> Program -> Int -> Int -> Int
modeToPos Pos p rb i = i 
modeToPos Imm p rb i = error "modeToPos used on Imm parameter mode"
modeToPos Rel p rb i = rb + i 

-- |Gets the instruction at a given index in the program, absent non-negative entires are 0 
get :: Program -> Int -> Int
get p i = let (lo,hi) = bounds (tape p)
          in case Map.lookup i (diff p) of
                 Just v  -> v
                 Nothing -> if i<=hi then tape p ! i else 0 

-- |Sets a given index in a program to a given value
set :: Program -> Int -> Int -> Program 
set p i v 
  | i<=hi && tape p ! i == v = Program {tape = tape p, diff = Map.delete i (diff p)
                                       ,inps = inps p, outp = outp p}
  | otherwise                = Program {tape = tape p, diff = Map.insert i v (diff p)
                                       ,inps = inps p, outp = outp p}
  where (lo,hi) = bounds (tape p) 

-- |Interpretation of binary Operations 
compute :: Operation -> Int -> Int -> Int
compute Add a b = a+b
compute Mul a b = a*b
compute Lt  a b = if a<b then 1 else 0
compute Eq  a b = if a==b then 1 else 0

-- |Given a predicate, calculates the new program counter 
jumpOn :: Program -> (Int -> Bool) -> Parameters -> Int -> Int -> Int
jumpOn p predicate (a,b,_) pc rb  
  | predicate val1 = val2
  | otherwise      = pc + 3
  where val1 = modeToVal a p rb (get p (pc+1))  
        val2 = modeToVal b p rb (get p (pc+2)) 

-- |Calculates new relative base from old
newbase :: Program -> Parameters -> Int -> Int -> Int  
newbase p (a,_,_) pc rb = let off = modeToVal a p rb (get p (pc+1)) in rb + off 

-- |In programs without output, the result is often stored at position 0
result :: Program -> Int
result p = get p 0 


-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- solutions
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- Execute the prog with inputs 12 and 2 
day2p1 = do 
  p <- parseProg "../input2.txt"
  p' <- executeWithArguments p 12 2
  putStrLn $ show (result p') 

-- Find inputs to make the result 19690720
day2p2 = do 
  p <- parseProg "../input2.txt"
  let is = [(v,n) | v<-[0..99], n <-[0..99]]
  r <- go p is 
  case r of 
    Nothing -> putStrLn "No inputs in [0..99] to output 19690720"
    Just (p',v,n) -> putStrLn $ "Inputs: " ++ show v ++ ", " ++ show n 
  where go p [] = return Nothing
        go p (i:is)  = do 
            let (v,n) = i 
            p' <- executeWithArguments p v n 
            if result p' == 19690720
               then return (Just (p',v,n))
               else go p is 

-- Execute the TEST program with input 0 and get the diagnostic code output 
day5p1 = do 
  p <- parseProg "../input5.txt"
  putStrLn "Enter 1 at the prompt."
  execute p 
  
-- Execute the TEST program with input 5 and get the diagnostic code output 
day5p2 = do 
  p <- parseProg "../input5.txt"
  putStrLn "Enter 5 at the prompt."
  execute p 

-- Finds the phase ordering to maximize the output signal through the 5 amps 
day7p1 = do 
  p <- parseProg "../input7.txt"
  results <- go p (permutations [0,1,2,3,4])
  putStrLn $ "Max value to thrusters: " ++ show (maximum results) 
  where go :: Program -> [[Int]] -> IO ([Int])
        go p [] = return []
        go p (perm:perms) = do 
          r <- runAmps p perm 0 
          rs <- go p perms 
          return (r:rs)

-- Finds the phase ordering to maximize the output signal through the 5 amps 
day7p2 = do 
  p <- parseProg "../input7.txt"
  results <- go p (permutations [5,6,7,8,9])
  putStrLn $ "Max value from feedback to thrusters: " ++ show (maximum results)
  where go :: Program -> [[Int]] -> IO ([Int])
        go p [] = return []
        go p (perm:perms) = do 
          r <- runFeedbackAmps (take 5 (repeat (0,p))) perm 0
          rs <- go p perms
          return (r:rs) 

-- Runs the BOOST program to check for incorrectly implemented operations 
day9p1 = do
  p <- parseProg "../input9.txt"
  putStrLn "Enter 1 at the prompt."
  execute p 

-- Runs the BOOST program to find a coordinate value
day9p2 = do
  p <- parseProg "../input9.txt"
  putStrLn "Enter 2 at the prompt."
  execute p 


-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- parsing
-- -- -- -- -- -- -- -- -- -- -- -- -- 

parseList xs = case parse integers "" xs of 
                  Left err -> error (errorBundlePretty err)
                  Right xs -> xs

integers :: Parsec Void String [Int]
integers = integer `sepBy` char ','

integer = signed (return ()) decimal 

parseProg file = do
  input <- readFile file
  let list = parseList input
  return (Program {tape = listArray (0,length list - 1) list 
                  ,diff = Map.empty, inps=[], outp = []})


-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- testing
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- | Convenient function for turning a String into a Program 
makeProg :: String -> Program
makeProg s = let xs = parseList s
             in Program {tape = listArray (0,(length xs - 1)) xs
                        ,diff = Map.empty, inps = [], outp = []}

exprog = Program {tape=listArray (0,11) [1,9,10,3,2,3,11,0,99,30,40,50], 
                  diff = Map.empty, inps = [], outp = []}
ioprog = Program {tape=listArray (0,4) [3,0,4,0,99], diff = Map.empty, inps = [], outp = []}
