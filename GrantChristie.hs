--1. Write a function inlist which given a list of integers and an integer n,
-- returns a Boolean indicating whether n occurs in the list.

inlist :: [Int] -> Int -> Bool

inlist [] n = False
inlist (x:xs) n = x == n || inlist xs n

examples1 = do 
  print ("inlist [2,3,2,4,7,9] 7 == True")
  print (inlist [2,3,2,4,7,9] 7)
  print ("inlist [2..100] 101 == False")
  print (inlist [2..100] 101)
  print ("inlist [2..] 101 == True")
  print (inlist [2..] 101)

--2. Write a function exactlyonce which takes a list of Integers and an Integer n,
-- then returns a Boolean indicating whether n occurs exactly once in the list.

exactlyonce :: [Int] -> Int -> Bool
exactlyonce [] n = False
exactlyonce xs n = length(filter (==n) xs) == 1

--4. Write a function replace which takes an integer x and a list of integers. 
--It returns a list where each element y of the list equals (x-y)*(x-y). 

replace :: Int -> [Int] -> [Int]
replace x [] = []
replace x (y:ys) = f y : replace x ys where f y = (x-y)*(x-y)

--5. Define a function addthemup that takes a list of lists of integers, sums the numbers in each of the lists within the list,
-- then multiplies the resulting sums with each other.

addthemup :: [[Int]] -> Int

addthemup list = foldr (*) 1 (map sum list)

--8. Define a function sequenceones such that sequenceones xs = True if and only if xs contains the substring 11.

sequenceones :: [Int] -> Bool

sequenceones (1:1:_) = True
sequenceones (x:xs) = sequenceones xs
sequenceones _ = False

