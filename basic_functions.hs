-- Functions -- 
fac :: Int -> Int 
fac n = if n<= 1 then 1 else n * fac (n-1) -- recursion

fac1 :: Int -> Int 
fac1 n 
     | n<= 1 = 1
     | otherwise = n * fac (n-1) --Guards-- 


-- Lists ---

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' (xs) 


sumIt :: [Int] -> Int
sumIt [] = 0
sumIt (x:xs) = x + sumIt xs 

getHead :: [a] -> a
getHead (x:_) = x  

getLast :: [a] -> a
getLast [x] = x
getLast (_:xs) = getLast xs

isElement :: Eq a => a -> [a] -> Bool
isElement n [] = False
isElement n (x:xs)
    | n == x  = True
    | otherwise = isElement n xs


getTail :: [a] -> [a]
getTail [] = []
getTail (_:xs) = xs


getInit :: [a] -> [a]
getInit [_] = []
getInit (x:xs) = x : getInit xs


combine :: [a] -> [a] -> [a]
combine [] y = y
combine (x:xs) y = x : combine xs y 




max' :: [Int] -> Int
max' [x] = x
max' (x:y:z) 
     | x > y = max'(x:z)
     | otherwise = max'(y:z)
            


reverse' :: [a] -> [a]
reverse'[] = []
reverse' (x:xs) = reverse' xs ++  [x]



scalar :: [Int] -> [Int] -> Int

scalar [] [] = 0 
scalar (x:xs) (y:ys) = x*y  + scalar xs ys


nonZeros :: [Int] -> [Int]

nonZeros [] = []
nonZeros (x:xs) 
         | x == 0 = nonZeros xs
         | otherwise = x: nonZeros xs

rotateLeft1 :: [a] -> [a]
rotateLeft1 [] = [] 
rotateLeft1 (x:xs) = xs ++ [x]


rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight (x:xs) = last xs : x : init xs 



oddMembers :: [Int] -> [Int]
oddMembers [] = []
oddMembers (x:xs) 
           | x `mod` 2 == 1 = x : oddMembers xs 
           | otherwise = oddMembers xs

countOddMembers :: [Int] -> Int
countOddMembers x = length (oddMembers x) 

compareLists :: Eq a => [a] -> [a] -> Bool
compareLists [] [] = True
compareLists (x:xs) (y:ys)
             | x == y = compareLists xs ys 
             | otherwise = False

take' :: Int -> [a] -> [a]
take' 0 _ = [] 
take' _ [] = [] -- list is empty 
take' n (x:xs) = x: take' (n-1)  xs

drop' :: Int -> [a] -> [a]
drop' 0 x = x -- why its not as= drop' 0 [] = []
drop' _ [] = [] -- list is emty 
drop' n (_:xs) =  drop' (n-1) xs

minimum' :: Ord a => [a] -> a
minimum'[x] = x
minimum'(x:y:z) 
        | x < y = minimum'(x:z)
        | otherwise = minimum'(y:z)

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