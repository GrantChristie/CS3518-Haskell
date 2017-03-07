import Debug.Trace

--1. Write a function inlist which given a list of integers and an integer n,
-- returns a Boolean indicating whether n occurs in the list.

inlist :: [Int] -> Int -> Bool

inlist [] n = False --when the list is empty return false.
inlist (x:xs) n = x == n || inlist xs n -- recursively iterate through list checking each element to see if there is a match

examples1 = do 
  print ("inlist [2,3,2,4,7,9] 7 == True")
  print (inlist [2,3,2,4,7,9] 7)
  print ("inlist [2..100] 101 == False")
  print (inlist [2..100] 101)
  print ("inlist [2..] 101 == True")
  print (inlist [2..] 101)

--2. Write a function exactlyonce which takes a list of Integers and an Integer n,
-- then returns a Boolean indicating whether n occurs exactly once in the list.

--this function cannot include the infinite lists. [[EXPAND]]

exactlyonce :: [Int] -> Int -> Bool

exactlyonce [] n = False -- when the list is empty return false
exactlyonce xs n = length(filter (==n) xs) == 1 -- use filter to get all occurences of n, if the length of this list is 1 then n has only occured once.

examples2 = do
  print ("exactlyonce [2,3,2,4,3] 3 == False")
  print (exactlyonce [2,3,2,4,3] 3)
  print ("exactlyonce [3..30] 15 == True")
  print (exactlyonce [3..30] 15)
  print ("exactlyonce [-10..10] (-5) == True")
  print (exactlyonce [-10..10] (-5))
  print ("exactlyonce [-5,1,5,-5,2] (-5) == False")
  print (exactlyonce [-5,1,5,-5,2] (-5))

--3. Define a function equalones which takes two lists of integers and decides whether it's true
-- that each of the lists contain equal numbers of ones.

--equalones :: [Int] -> [Int] -> Bool

--4. Write a function replace which takes an integer x and a list of integers. 
--It returns a list where each element y of the list equals (x-y)*(x-y). 

replacenew :: Int -> [Int] -> [Int]
replacenew x [] = [] -- if the provided list is empty return an empty list
replacenew x (y:ys) = f y : replacenew x ys where 
  f y = (x-y)*(x-y)

examples4 = do 
  print ("replacenew 2 [3,6,9] == [1,16,49]")
  print (replacenew 2 [3,6,9])

--5. Define a function addthemup that takes a list of lists of integers, sums the numbers in each of the lists within the list,
-- then multiplies the resulting sums with each other.

addthemup :: [[Int]] -> Int

addthemup list = foldr (*) 1 (map sum list)

--6. Define a function repeatnew that repeats the application of a function to an argument a given number of times. 

--7. Define a function antepenultimate1 such that, if xs is a list of integers then 
-- antepenultimate1 xs is True iff the antepenultimate member of xs is 1.

--8. Define a function sequenceones such that sequenceones xs = True if and only if xs contains the substring 11.

sequenceones :: [Int] -> Bool

sequenceones (1:1:_) = True -- If the first two values of the list are two 1s then return true
sequenceones (x:xs) = sequenceones xs -- -- recursively iterate through list checking for consecutive 1s.
sequenceones _ = False -- Any other inputs are false

