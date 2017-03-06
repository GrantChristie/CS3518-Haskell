--1. Write a function inlist which given a list of integers and an integer n,
-- returns a Boolean indicating whether n occurs in the list.

inlist :: [Int] -> Int -> Bool

inlist [] n = False
inlist (x:xs) n = x == n || inlist xs n

--4. Write a function replace which takes an integer x and a list of integers. 
--It returns a list where each element y of the list equals (x-y)*(x-y). 

replace :: Int -> [Int] -> [Int]
replace x [] = []
replace x (y:ys) = f y : replace x ys where f y = (x-y)*(x-y)

--5. Define a function addthemup that takes a list of lists of integers, sums the numbers in each of the lists within the list,
-- then multiplies the resulting sums with each other.

addthemup :: [[Int]] -> Int

addthemup list = foldr (*) 1 (map sum list)

