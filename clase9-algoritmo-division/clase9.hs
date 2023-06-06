-- Mie 02-11-22
-- Algoritmo de la Division

type Set a = [a]

-- Alg. Division             
division :: Int -> Int -> (Int, Int)
division a d | a < d = (0, a)
             | otherwise = (fst (division (a-d) d)+1, snd (division (a-d) d))

-- Escrito mas simple
_division :: Int -> Int -> (Int, Int)
_division a d | a < d = (0, a)
              | otherwise = (fst qr_ + 1 , d + snd qr_)
              where qr_ = _division (a-d) d

-- Ejercicio: Extender division para Z
divisionExtendida :: Int -> Int -> (Int, Int)
divisionExtendida a d | a < 0 = (-1 * fst (division (-a) d) - 1, d - snd (division (-a) d)) 
                      | otherwise = division a d

-- Alg. Euclides :: Usando division ya programada, obtener el maximo comun divisor-
-- Obs. Si b > a se revierte solo :: mcd (a, b) -> mcd (b, a)
mcd :: Int -> Int -> Int
mcd a b | b == 0 = a     
        | otherwise = mcd b r
        where r = snd (division a b)

-- Otras formas de encontrar el maximo comun divisor
-- Por ejemplo: Armar las listas de divisores de A, B, intersectarlos y multiplicar todo

divisores :: Int -> Set Int 
divisores a = divisoresHasta a a

divisoresHasta :: Int -> Int -> Set Int
divisoresHasta a h | h == 1 = [1]
                   | mod a h == 0 = h : (divisoresHasta a (h-1))
                   | otherwise = divisoresHasta a (h-1)

mcd_vers2 :: Int -> Int -> Int 
mcd_vers2 a b = mayor ( interseccion (divisores a) (divisores b) )

interseccion [x] (y:ys) | elem x (y:ys) == True = [x]
                        | otherwise = []
interseccion (x:xs) (y:ys) | (elem x (y:ys) == True) = x : interseccion xs (y:ys)
                           | otherwise = interseccion xs (y:ys)

mayor :: Set Int -> Int
mayor [x] = x
mayor (x:xs) | x >= mayor (xs) = x 
             | otherwise = mayor (xs)

-- Aux :: No usados
productoria :: Set Int -> Int
productoria [] = 1
productoria (x:xs) | xs == [] = x
                   | otherwise = x * productoria xs

{-- Ideas Raras:: 

divisores a = a : anterioresDivisores a (a-1)

anterioresDivisores :: Int -> Int -> Set Int 
anterioresDivisores n k = anterioresDivisoresAC n k []

anterioresDivisoresAC :: Int -> Int -> Set Int -> Set Int
anterioresDivisoresAC n k (x:xs) | mod n k == 0 = k : (x:xs) ++ anterioresDivisoresAC n (k-10) (x:xs)
                                 | otherwise = (x:xs) ++ anterioresDivisoresAC n (k-1) (x:xs)
--}

----------------------------------------------------------------------------------
-- Ej: MCD Extendido --> (g, s, t)

trd3 :: (Int, Int, Int) -> Int
trd3 (_, _, z) = z

emcd :: Int -> Int -> (Int, Int, Int)
emcd a 0 = (a, 1, 0)  
emcd a b = (g, t_, s_ -t_*q)
         where (q,r) = division a b
               (g, s_, t_) = emcd b r        
