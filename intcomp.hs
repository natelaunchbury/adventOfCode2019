import Data.Array
import Data.IntMap(IntMap)
import qualified Data.IntMap as Map
import Data.List
import Control.Applicative
import Library

-- todo: test RBO and Rel

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
  _ <- executeH p 0 0 
  putStrLn "Done"

-- | Program execution which shows the result 
executeO :: Program -> IO ()
executeO p = do 
  r <- executeH p 0 0 
  putStrLn $ show (result r) 

-- | A special case of execution where the first two arguments are changed 
executeWithArguments :: Program -> Int -> Int -> IO (Program)
executeWithArguments p a b = do
   let p'  = set p  1 a
   let p'' = set p' 2 b
   executeH p'' 0 0

-- | Driving force of program execution; executes a single instruction then loops 
executeH :: Program             -- ^The program being executed 
         -> Int                 -- ^The program counter 
         -> Int                 -- ^The relative base 
         -> IO (Program)        -- ^The final program state
executeH p pc rb
  | instr == Halt = return p
  | instr == Add  = executeH (calculate Add (a,b,c) p pc rb) (pc+4) rb
  | instr == Mul  = executeH (calculate Mul (a,b,c) p pc rb) (pc+4) rb
  | instr == Inp  = do (inp,p') <- input p; executeH (set p' (get p' (pc+1)) inp) (pc+2) rb
  | instr == Out  = do p' <- output p (modeToVal a p rb (get p (pc+1))); executeH p' (pc+2) rb
  | instr == Jt   = let pc' = jumpOn p (/=0) (a,b,c) pc rb in executeH p pc' rb
  | instr == Jf   = let pc' = jumpOn p (==0) (a,b,c) pc rb in executeH p pc' rb
  | instr == Lt   = executeH (calculate Lt (a,b,c) p pc rb) (pc+4) rb
  | instr == Eq   = executeH (calculate Eq (a,b,c) p pc rb) (pc+4) rb
  | instr == RBO  = let rb' = offset p (a,b,c) pc rb in executeH p (pc+2) rb' 
  | otherwise = error $ "tape at " ++ show pc ++ " encountered " ++ show (tape p ! pc)
  where (instr, (a,b,c)) = unpackage (get p pc)

-- | Execution that passes a continuation when input is needed from the console
executeC :: Program -> Int -> Int -> IO (Int, Program)
executeC p pc rb 
  | instr == Halt = return ((-1),p)   -- distinguish Halt from awaiting input 
  | instr == Add  = executeC (calculate Add (a,b,c) p pc rb) (pc+4) rb 
  | instr == Mul  = executeC (calculate Mul (a,b,c) p pc rb) (pc+4) rb 
  | instr == Inp  = if inps p == [] 
                     then return (pc, p) 
                     else do (inp,p') <- input p; executeC (set p' (get p' (pc+1)) inp) (pc+2) rb
  | instr == Out  = do p' <- output p (modeToVal a p rb (get p (pc+1))); executeC p' (pc+2) rb 
  | instr == Jt   = let pc' = jumpOn p (/=0) (a,b,c) pc rb in executeC p pc' rb
  | instr == Jf   = let pc' = jumpOn p (==0) (a,b,c) pc rb in executeC p pc' rb 
  | instr == Lt   = executeC (calculate Lt (a,b,c) p pc rb) (pc+4) rb 
  | instr == Eq   = executeC (calculate Eq (a,b,c) p pc rb) (pc+4) rb 
  | instr == RBO  = let rb' = offset p (a,b,c) pc rb in executeC p (pc+2) rb'
  | otherwise = error $ "tape at " ++ show pc ++ " encountered " ++ show (tape p ! pc)
  where (instr, (a,b,c)) = unpackage (get p pc)


-- |Runs the amplifiers sequentially, providing the phase and previous output as input 
runAmps :: Program           -- ^The program to be executed  
        -> [Int]             -- ^The phase order 
        -> Int               -- ^The previous amp's output 
        -> IO (Int)          -- ^The final amp's output 
runAmps p []     _   = return (head (outp p))
runAmps p (n:ns) out = do 
    p' <- executeH (Program {tape=tape p, diff=diff p, inps=n:out:inps p, outp = []}) 0 0
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
                                   store = get p (pc+3)
                               in set p store (compute op val1 val2)

-- |Given a parameter mode, returns the immediate value 
modeToVal :: PMode -> Program -> Int -> Int -> Int
modeToVal Pos p rb i = get p i
modeToVal Imm p rb i = i 
modeToVal Rel p rb i = get p (rb + i)


-- |Gets the instruction at a given index in the program 
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

-- |Calculates position from relative base offset 
offset :: Program -> Parameters -> Int -> Int -> Int  
offset p (a,_,_) pc rb = modeToVal a p rb (get p (pc+1))

-- |In programs without output, the result is often stored at position 0
result :: Program -> Int
result p = get p 0 

-- |Simple function to handle printing to the terminal 
output :: Program -> Int -> IO (Program)
output p n = do
  let p' = Program {tape = tape p, diff = diff p, inps = inps p, outp = n:outp p}
  putStrLn $ "Output: " ++ (show n) 
  return p'

-- |Simple function to handle input from the terminal 
-- ToDo: use Haskeline instead
input :: Program -> IO (Int,Program)
input p 
  | inps p == [] = do 
       putStr "Please enter an int: "
       n <- getLine
       return (read n,p)
  | otherwise    = do 
       let (i:is) = inps p
       putStrLn $ "Input used: " ++ show i 
       return (i, Program {tape = tape p, diff = diff p, inps = is, outp = outp p})

-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- solutions
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- Execute the prog with inputs 12 and 2 
day2p1 = do 
  p <- parseProg "input2.txt"
  p' <- executeWithArguments p 12 2
  putStrLn $ show (result p') 

-- Find inputs to make the result 19690720
day2p2 = do 
  p <- parseProg "input2.txt"
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
  p <- parseProg "input5.txt"
  putStrLn "Enter 1 at the prompt."
  execute p 
  
-- Execute the TEST program with input 5 and get the diagnostic code output 
day5p2 = do 
  p <- parseProg "input5.txt"
  putStrLn "Enter 5 at the prompt."
  execute p 

-- Finds the phase ordering to maximize the output signal through the 5 amps 
day7p1 = do 
  p <- parseProg "input7.txt"
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
  p <- parseProg "input7.txt"
  results <- go p (permutations [5,6,7,8,9])
  putStrLn $ "Max value from feedback to thrusters: " ++ show (maximum results)
  where go :: Program -> [[Int]] -> IO ([Int])
        go p [] = return []
        go p (perm:perms) = do 
          r <- runFeedbackAmps (take 5 (repeat (0,p))) perm 0
          rs <- go p perms
          return (r:rs) 


-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- parsing
-- -- -- -- -- -- -- -- -- -- -- -- -- 

-- ToDo: this doesn't work if the last num is negative 
parseList = many (currentInt <* lit ',' <|> num)

parseProg file = do
  input <- readFile file
  let list = run parseList input
  return (Program {tape = listArray (0,length list - 1) list 
                  ,diff = Map.empty, inps=[], outp = []})

-- -- -- -- -- -- -- -- -- -- -- -- -- 
-- testing
-- -- -- -- -- -- -- -- -- -- -- -- -- 

---- | Interactive program execution which waits for the user at each instruction 
---- Essentially the same as 'executeH' but with 'prompt' calls 
--executeI :: Program -> Int -> IO (Program)
--executeI p pc 
--  | instr == Halt = return p
--  | instr == Add  = do prompt p pc; executeI (calculate Add (a,b,c) p pc) (pc+4)
--  | instr == Mul  = do prompt p pc; executeI (calculate Mul (a,b,c) p pc) (pc+4)
--  | instr == Inp  = do inp <- input; executeI (set p (get p (pc+1)) inp) (pc+2)
--  | instr == Out  = do putStrLn "Output incomming"; prompt p pc; 
--                       output (get p (get p (pc+1))); executeI p (pc+2)
--  | otherwise   = error $ "tape at " ++ show pc ++ " encountered " ++ show (tape p ! pc)
--  where (instr, (a,b,c)) = unpackage (get p pc)

executeI :: Program -> Int -> Int -> IO (Program)
executeI p pc rb
  | instr == Halt = return p
  | instr == Add  = do prompt p pc rb; executeI (calculate Add (a,b,c) p pc rb) (pc+4) rb
  | instr == Mul  = do prompt p pc rb; executeI (calculate Mul (a,b,c) p pc rb) (pc+4) rb
  | instr == Inp  = do prompt p pc rb; (inp,p') <- input p; executeI (set p' (get p' (pc+1)) inp) (pc+2) rb
  | instr == Out  = do prompt p pc rb; p' <- output p (modeToVal a p rb (get p (pc+1))); executeI p' (pc+2) rb
  | instr == Jt   = let pc' = jumpOn p (/=0) (a,b,c) pc rb in do prompt p pc rb; executeI p pc' rb
  | instr == Jf   = let pc' = jumpOn p (==0) (a,b,c) pc rb in do prompt p pc rb; executeI p pc' rb
  | instr == Lt   = do prompt p pc rb; executeI (calculate Lt (a,b,c) p pc rb) (pc+4) rb
  | instr == Eq   = do prompt p pc rb; executeI (calculate Eq (a,b,c) p pc rb) (pc+4) rb
  | instr == RBO  = let rb' = offset p (a,b,c) pc rb in do prompt p pc rb; executeI p (pc+2) rb' 
  | otherwise = error $ "tape at " ++ show pc ++ " encountered " ++ show (tape p ! pc)
  where (instr, (a,b,c)) = unpackage (get p pc)


prompt :: Program -> Int -> Int -> IO ()
prompt p pc rb = do 
   putStrLn $ show p ++ " PC: " ++ show pc ++ ", RB: " ++ show rb 
   _ <- getLine 
   return ()

makeProg :: String -> Program
makeProg s = let xs = run parseList s
             in Program {tape = listArray (0,(length xs - 1)) xs
                        ,diff = Map.empty, inps = [], outp = []}

exprog = Program {tape=listArray (0,11) [1,9,10,3,2,3,11,0,99,30,40,50], 
                  diff = Map.empty, inps = [], outp = []}
ioprog = Program {tape=listArray (0,4) [3,0,4,0,99], diff = Map.empty, inps = [], outp = []}
