-- Final Question 1
myCube :: Integer -> Integer
myCube a = a * a * a

-- Final Question 2
testQ2 :: Integer -> Integer -> Integer -> Bool
testQ2 u v w = not ((u == v) && (v == w))

--testQ2 1 2 2
  -- ~-> not((1 == 2) && (2 == 2))
  -- ~-> not((False) && (2 == 2))
  -- ~-> not(False)
  -- ~-> True

--testQ2 2 2 1
  -- ~-> not((2 == 2) && (2 == 1))
  -- ~-> not((True) && (2==1))
  -- ~-> not((True) && (False))
  -- ~-> not(False)
  -- ~-> True

-- Final Question 3
testQ3 :: Int -> String -> String
testQ3 n s
  | n < 0 = tQ4 'b' s
  | n == 0 = tQ4 'c' s
  | otherwise = tQ4 'd' s

-- Final Question 4
tQ4 :: Char -> String -> String
tQ4 a [] = []
tQ4 a (x:xs)
  | x == 'a' = a : tQ4 a xs
  | otherwise = x : tQ4 a xs

-- Final Question 5

tQ5 :: [Int] -> [Int]
tQ5 [] = []
tQ5 (x:xs) = (filter (<100) (filter(>=0) [(2 * x + 3)])) ++ tQ5 xs

-- Final Question 6
myLength :: [a] -> Integer
myLength a = sum (map (const 1) a)

-- Final Question 7

squareSum :: Integer -> Integer
squareSum n
  | n < 1 = error "Input number >= 1"
  | otherwise = sum (map(^2) [1..n])

-- Final Question 8
allEvens :: [Int] -> Bool
allEvens a = foldr (&&) True (map(\y->(y `mod` 2 == 0)) a)

-- Final Question 9
doubleIncr :: Integer -> Integer
doubleIncr n = double(increment n)

increment :: Integer -> Integer
increment n = n + 1

double :: Integer -> Integer
double n = n * 2

-- Final Question 10
oddNums = [1,3 ..]
evenNums = [2, 4 ..]
addStarting :: Int -> [Integer] -> [Integer] -> Integer
addStarting n xs ys
  | n <= 0 = 0
  | otherwise = head xs + head ys + addStarting(n-1) (tail xs) (tail ys)

--addStarting 2 oddNums, evenNums
-- ~> 1 + 2 + (addStarting(1) ([3,5 ..]) ([4,6 ..]))
-- ~> 1 + 2 + (3 + 4 + (addStarting(0) ([5,7 ..]) ([6, 8 ..])))
-- ~> 1 + 2 + (3 + 4 + (0))
-- ~> 1 + 2 + (7)
-- ~> 10