-- Mier. 14-09-22 - Labo Algebra I

-- SUMATORIAS
sumatoria :: Int -> Int
sumatoria 1 = 1
sumatoria n = n + sumatoria(n-1)

sumatoria_cerrada :: Int -> Int 
sumatoria_cerrada n = div (n * (n + 1)) 2

-- Ejercicios (Parte 1)
f1 0 = 1
f1 n = f1(n-1) + 2^n

formula_cerrada1 n = (2^(n+1) - 1) / (2-1)

f2 0 q = 0
f2 1 q = q
f2 n q = f2 (n-1) q + q^n

formula_cerrada2 n q = (q^(n+1) - 1) / (q - 1) - 1 

f3 :: Int -> Int -> Int
f3 0 q = 0
f3 n q = f2 (2*n) q 

formula_cerrada3 n q = (q^(2*n+1) -1) / (q-1) -1

f4 :: Int -> Int -> Int
f4 0 q = 1
f4 n q = f3 n q - f2 (n-1) q 

-- Ejercicios (Parte 2)
fact 0 = 1
fact n = fact (n-1) * n

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox j = eAprox(j-1) + 1 / fromIntegral (fact(j))

-- Ejercicio 3
funcion 1 1 = 1
funcion 0 0 = 0
funcion _ 0 = 0
funcion 0 _ = 0
funcion n m = f2 n m + f2 n (m-1)

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m = f2 n q * f2 m q 

auxQ 1 = 1
auxQ m = auxQ (m-1) + 1/ m
sumaRacionales n m = (n * (n+1) / 2) * auxQ m

-- Tarea
g1 i n = f2 i n - f2 i i 
g2 n = funcion n n 

auxx :: Int -> Int
auxx i | mod i 2 == 0 = 2^i
       | otherwise = 0
g3 n = auxx (n-1) + auxx n
