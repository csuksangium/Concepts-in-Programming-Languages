---Chanan Suksangium CS320 HW4

import Data.List

type Str a = [a]

---Question 1
overlap :: Eq a => (Str a, Str a) -> Int
overlap ((x:xs), s)
 |isPrefixOf (x:xs) s == True = length(x:xs)
 |isPrefixOf (xs) s == True = length(xs)
 |otherwise = overlap (xs, s)
 
naive :: Eq a => [Str a] -> Str a
naive (x:xs)
 |xs == [] = x
 |otherwise = x `o` naive(xs)
 
contains :: Eq a => Str a -> Str a -> Bool
contains s s' = contains' s s' True
 where
  contains' _ [] h = True
  contains' [] _ h = False
  contains' (x:xs) (y:ys) t = (y == x && contains' xs ys False) || (t && contains' xs (y:ys) t)
 
o :: Eq a => Str a -> Str a -> Str a
o s s'
 |over == 0 = s ++ s'
 |otherwise = s ++ (drop over s')
 where
 over = overlap (s, s')

---Question 2

minimize :: Eq a => (a -> Int) -> a -> a -> a
minimize f x y
 |f x > f y = y
 |otherwise = x

maximize :: Eq a => (a -> Int) -> a -> a -> a
maximize f x y
 |f x > f y = x
 |otherwise = y

---Question 3

update :: Eq a => [Str a] -> (Str a,Str a) -> [Str a]
update (x:xs) (s, s') = s'' : filter (temp s'') xs
 where
 s'' = s `o` s'
 temp s'' x = not(contains s'' x)

allPairs :: Eq a => [Str a] -> [(Str a,Str a)]
allPairs [] = []
allPairs (x:xs) = filter (k) z ++ allPairs xs
 where
 z = map (f x) xs
 k i = fst i /= snd i
 f x y = (x, y)

allPairs' :: Eq a => [Str a] -> [(Str a,Str a)]
allPairs' xs = [(x, y) | y <- xs, x <- xs, y /= x]

superstring :: Eq a => ([Str a] -> [(Str a,Str a)]) -> [Str a] -> Str a
superstring f xs
 |xs == [] = []
 |length xs == 1 = head(xs)
 |otherwise = foldr compared base (map f' next)
  where
  next = f xs
  combined p xs = [fst p `o` snd p] ++ [x | x <- xs, x /= (fst p), x /= (snd p)]
  f' (y, z) = superstring f (combined (y, z) xs) 
  compared s s' = minimize length s s'
  base = naive xs

optimal :: Eq a => [Str a] -> Str a
optimal xs = superstring allPairs' xs

---Question 4
firstPair :: Eq a => [Str a] -> [(Str a,Str a)] 
firstPair (x:xs) = [(x, head(xs))]

bestWithFirst :: Eq a => [Str a] -> [(Str a,Str a)] 
bestWithFirst (x:xs) = [(x, snd temp)]
 where
 temp = foldr (temp2) (x, last xs) lst'
 lst' = [(p, y) | let p = x, y <- xs]
 temp2 s s' = maximize overlap s s'

bestPair :: Eq a => [Str a] -> [(Str a,Str a)]
bestPair xs = [temp]
 where
 lst' = allPairs' xs
 temp = foldr (temp2) (head xs, last xs) lst'
 temp2 s s' = maximize overlap s s'

greedy :: Eq a => [Str a] -> Str a
greedy xs = superstring firstPair xs

---Question 5
compare' :: Eq a => ([Str a] -> Str a) -> ([Str a] -> Str a) -> [Str a] -> Double
compare' f f' xs = (fromIntegral fx) / (fromIntegral f'x)
 where
 fx = length(f xs)
 f'x = length(f' xs)
 
---main = do
---let test1 = ["ctagcgacat", "aagatagtta", "gctactaaga", "gacatattgt", "tagttactag"]
---let test2 = ["101001","010100010100", "100101", "001010", "11010", "100", "11010"]
---let test3 = [x++y | x<-["aab","dcc","aaa"], y<-["dcc", "aab"]]
---print(compare' optimal greedy test1)
