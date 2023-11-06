nonZeros :: [Int] -> [Int]
nonZeros [] = []

nonZeros (x:xs) | x == 0 = nonZeros xs

                | otherwise = x:nonZeros xs

getLast :: [a] -> a
getLast [x] = x
getLast (x:xs) = getLast xs -- [5]

drop1 :: [a] -> [a]
drop1 [x] = [] 
drop1 (x:xs) = x:drop1 xs -- [1.2.3.4]



rotateRight1 :: [a] -> [a]
 
rotateRight1 (x:xs) = (getLast (x:xs)):(drop1 (x:xs))

compareLists :: Eq a => [a] -> [a] -> Bool
compareLists _ [] = False
compareLists [] _ = False
compareLists [] [] = True
compareLists (x:xs) (y:ys) | x == y = compareLists xs ys
                            | otherwise = False               

getHead :: [a] -> a
getHead [x] = x
getHead(x:_) = x --x   

rotateLeft :: [a] -> Int -> [a]
rotateLeft  [] n = []
rotateLeft (x:xs) n | n == 1 = (xs) ++ [x] 
                    | otherwise = rotateLeft((xs) ++ [x]) (n-1)

plusOne :: Int -> Int
plusOne x = x + 1
xs = [1,2,3,4,5,6]
ys = map plusOne xs

removeOne :: Eq a => a -> [a] -> [a]
removeOne n [] = []
removeOne n (x:xs) | n == x = xs 
                   | otherwise = x:removeOne n xs

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x:replicate' (n-1) x 





--join :: [[a]] -> a -> [a]
--join [[]] n  = [] -- empty 

--join [[x:xs]] n = concat [x:xs] (++) n