qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort ( x : xs ) = qsort smaller ++ [x] ++ qsort larger
 where smaller = [ a | a <- xs, a <= x ]
       larger = [ a | a <- xs, a > x ]


{- Exercice 3
-}

ones :: [Int]
ones = 1 : ones


egaux4 :: Int -> Int -> Int -> Int -> Bool
egaux4 a b c d = if (a == b && b == c && c == d)
                   then True
                   else False



max4 :: Ord a => a -> a -> a -> a -> a
max4 a b c d = max (max a b) (max c d)



{-
Exercice n°4 : listes par compréhension
-}

sumcarre :: Int
sumcarre = sum [x ^ x | x <- [1..100]]

replic :: Int -> a -> [a]
replic a b = [b | a <- [1..a]]



pyths :: Int -> [(Int,Int,Int)]
pyths h = [(a,b,c) | b <- [1..h], c <- [1..h], a <- [1..h], a^2 == b^2 + c^2 || b^2 == a^2 + c^2 || c^2 == a^2 + b^2, a == h || b == h || c == h]


{-
Exercice n°5 : le palindrome et ma première fonction récursive
-}
{-
inverse :: [a] -> [a]
inverse x:xs = inverse xs
-}


{-
Exercice n°6 : type de données algébriques simple
-}


data Parfum = Chocolat | Vanille | Framboise
prixParfum :: Parfum -> Num
prixParfum Chocolat = 1.5
prixParfum Vanille = 1.2
prixParfum Framboise = 1.4    



