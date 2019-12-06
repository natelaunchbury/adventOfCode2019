import Library


main = do 
  part1 "input1.txt"
  part2 "input1.txt"


part1 file = do 
  input <- readFile file
  let masses = map (run num) (lines input)
  let fuels = sum (map fuel masses) 
  putStrLn (show fuels) 

part2 file = do 
  input <- readFile file
  let masses = map (run num) (lines input)
  let fuels = sum (map actualFuel masses)
  putStrLn (show fuels)

-- |Fuel calculation
fuel :: Int -> Int
fuel n = n `div` 3 - 2 

-- |Fuel calculation which takes into account the added mass of fuel
actualFuel :: Int -> Int
actualFuel n = sum (go n [])
  where go :: Int -> [Int] -> [Int]
        go n fs = let f = fuel n 
                  in if f <= 0 
                       then fs
                       else go f (f:fs)








