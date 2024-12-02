convert :: String -> [Integer]
convert str = map read (words str)

evalReport :: [Integer] -> Bool
evalReport (x : xs)
  | null xs = True
  | otherwise = eval (x : xs)

evalReport2 :: [Integer] -> Bool
evalReport2 (x : xs)
  | null xs = True
  | otherwise = any (eval . remove (x : xs)) [-1, 0 .. length xs]
  where
    lists :: [[Integer]]
    lists = map (remove (x : xs)) [-1, 0 .. length xs]

remove :: [Integer] -> Int -> [Integer]
remove (x : xs) i
  | null xs && i == 0 = []
  | null xs = [x]
  | i == 0 = remove xs (i - 1)
  | otherwise = x : remove xs (i - 1)

eval :: [Integer] -> Bool
eval (x : xs)
  | null xs = True
  | otherwise = checkLevels (x : xs) (x >= head xs)
  where
    checkLevels :: [Integer] -> Bool -> Bool
    checkLevels (x : xs) descending
      | null xs = True
      | descending && diff >= 1 && diff <= 3 = nextIter
      | not descending && diff <= -1 && diff >= -3 = nextIter
      | otherwise = False
      where
        diff = x - head xs
        nextIter = checkLevels xs descending

task1 :: IO ()
task1 = do
  input <- readFile "input"
  let inputLines = lines input
  let res = foldr (\x i -> if evalReport $ convert x then i + 1 else i) 0 inputLines
  print $ "Task 1 Result: " ++ show res ++ ", should be 314"

task2 :: IO ()
task2 = do
  input <- readFile "input"
  let inputLines = lines input
  let res = foldr (\x i -> if evalReport2 $ convert x then i + 1 else i) 0 inputLines
  print $ "Task 2 Result: " ++ show res ++ ", should be 373"
