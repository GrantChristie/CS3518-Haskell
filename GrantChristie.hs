--Misc. Functions for reference in questions
--Returns a given number squared
square :: (Num a) => a -> a
square x = x * x
---------------------------------------------------------------------------------------------
--To execute example runs of each question, type "examplesX".
--where x is the number of the question you want to see example runs of.
---------------------------------------------------------------------------------------------

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
  print ("inlist [4..20] 2 == False")
  print (inlist [4..20] 2)
  print ("inlist [-5..5] (-2) == True")
  print (inlist [-5..5] (-2))

--2. Write a function exactlyonce which takes a list of Integers and an Integer n,
-- then returns a Boolean indicating whether n occurs exactly once in the list.

--This function cannot produce the result from the last example. This is because [1..] is an infinite list.
--Therefore [1..] 15 cannot be computed because after it finds the first instance of 15 it is possible that there could be another 15 at some point in the list.

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
equalones :: [Int] -> [Int] -> Bool

equalones xs ys = if length(filter (==1) xs) == length(filter (==1) ys) then True --use filter to get all occurenes of 1 for each list, if the length of these filtered lists are the same then there are the same number of 1s.
else False --otherwise there are not the same number of 1s in each list.

examples3 = do
  print ("equalones [1,2,0] [3,5,1,1] == False")
  print (equalones [1,2,0] [3,5,1,1])
  print ("equalones [1,0] [0] == False")
  print (equalones [1,0] [0])
  print ("equalones [1,0,0,1] [0,1,1,0] == True")
  print (equalones [1,0,0,1] [0,1,1,0])
  print ("equalones [][] == True")
  print (equalones [][])
  print ("equalones [-1,-1,-2] [-1,-1,4,5,1] == False")
  print (equalones [-1,-1,-2] [-1,-1,4,5,1])

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
  print ("replacenew 5 [2..5] == [9,4,1,0]")
  print (replacenew 5 [2..5])
  print ("replacenew 3 [-2,4,3,2] == [25,1,0,1]")
  print (replacenew 3 [-2,4,3,2])


--5. Define a function addthemup that takes a list of lists of integers, sums the numbers in each of the lists within the list,
-- then multiplies the resulting sums with each other.

addthemup :: [[Int]] -> Int

addthemup list = foldr (*) 1 (map sum list) -- use sum to get the number of the numbers in each list, then use map to obtain the result of multiplying the sums with each other.

examples5 = do
  print ("addthemup [[1,3],[3,7]] == 40")
  print (addthemup [[1,3],[3,7]])
  print ("addthemup [[1,2,3],[9]] == 54")
  print (addthemup [[1,2,3],[9]])
  print ("addthemup [[1,2],[]] == 0")
  print (addthemup [[1,2],[]])
  print ("addthemup [[1,2],[1,3],[4,5,7],[2]] == 384")
  print (addthemup[[1,2],[1,3],[4,5,7],[2]])
  print ("addthemup[[1,2,3],[-1,-2]] == -18")
  print (addthemup[[1,2,3],[-1,-2]])
  print ("addthemup[[1..3],[4..6]] == 90")
  print (addthemup[[1..3],[4..6]])

--6. Define a function repeatnew that repeats the application of a function to an argument a given number of times.

repeatnew :: (a -> a) -> Int -> a -> a

repeatnew function number value = iterate function value !! number -- use prelude function 'iterate' to repeat the function up to the value and use !! to return the correct element

examples6 = do
  print ("repeatnew square 1 2 == 4")
  print(repeatnew square 1 2)
  print ("repeatnew square 2 2 == 16")
  print(repeatnew square 2 2)
  print ("repeatnew square 4 1 == 1")
  print(repeatnew square 1 2)
  print ("repeatnew square 10 0.999 == 0.35897928..")
  print(repeatnew square 10 0.999)

--7. Define a function antepenultimate1 such that, if xs is a list of integers then
-- antepenultimate1 xs is True iff the antepenultimate member of xs is 1.

antepenultimate1 :: [Int] -> Bool

antepenultimate1 xs = if (length xs) < 3 then False -- if the list is smaller than 3 then there can be no antepenultimate
else if (reverse xs) !! 2 == 1 then True -- use reverse to flip the list and use !! 2 to access the antepenultimate, if it is 1 then return true
else False -- otherwise the antepenultimate is not 1 and therefore false

examples7 = do
  print ("antepenultimate1 [] == False")
  print (antepenultimate1 [])
  print ("antepenultimate1 [1,0,1,1,0] == True")
  print (antepenultimate1 [1,0,1,1,0])
  print ("antepenultimate1 [1,0,0,1,1] == False")
  print (antepenultimate1 [1,0,0,1,1])
  print ("antepenultimate1 [1,0,1,0,1,0] == False")
  print (antepenultimate1 [1,0,1,0,1,0])
  print ("antepenultimate1[1,1] == False")
  print (antepenultimate1[1,1])

--8. Define a function sequenceones such that sequenceones xs = True if and only if xs contains the substring 11.

sequenceones :: [Int] -> Bool

sequenceones (1:1:_) = True -- If the first two values of the list are two 1s then return true
sequenceones (x:xs) = sequenceones xs -- -- recursively iterate through list checking for consecutive 1s.
sequenceones _ = False -- Any other inputs are false

examples8 = do
  print ("sequenceones [] == False")
  print (sequenceones [])
  print ("sequenceones [1,0,1,1,0] == True")
  print (sequenceones [1,0,1,1,0])
  print ("sequenceones [1,0,0,1,0,1] == False")
  print (sequenceones [1,0,0,1,0,1])
  print ("sequenceones [1,0,1,0,1] == False")
  print (sequenceones [1,0,1,0,1])
  print ("sequenceones [-1,-1]")
  print (sequenceones [-1,-1])
