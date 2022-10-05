-- Clase 6 - Listas - 05-10-22 - Sebastian Andres

-- Las listas tienen tamaño dinamico
-- head :: [a, b, c] -> a -- devuelve el primer elto
-- tail :: [a, b, c] -> [b, c] -- devuelve la lista sin el primer elto
-- (:) a -> [b, c, d] -> [a, b, c, d] -- devuelve la lista pasada con el elto dado como cabeza

-- head [] -> Error
-- head [a,b,c] : [d,e] = (:) a [d, e] = [a, d, e]
-- head ([1,2,3] : [4,5]) -> Error no se puede armar la lista argumento, != tipo
-- head ([1,2,3] : [4,5] : [])

-- tail [a] = [] 
-- tail [] -> Error

-- (:) 1 [2, 3] = [1, 2, 3] = 1 : [2, 3]
-- (:) [2] [] = [[2]] = [2] : []
-- (:) [3, 2] [[2]] = [[3,2], [2]] = [3, 2] : [[2]]
-- [1,2,3] : [4,5] : [] = [1,2,3] : [4,5] : [] // ejecuta de derecha a izquierda (?)

-- [] lista vacia
-- [x] tiene 1 elemento
-- (x:xs) tiene al menos 1 elemento
-- (x:y:xs) tiene almenos 2 elementos
-- [x:y] = (x : y : []) tiene dos elementos

-- [a..b] = [a, a+1, a+2, ..., b-1, b] // b >= 1

-- [1..100] = [1, 2, 3, 4, 5, ..., 99, 100]
-- [1,3..100] = [1,3,4,5,6,..,100] -- NO DEVUELVE EL DOS
-- [100,99..1] = [100, 99, 98, ..., 1]

-- [1..] = [1, 2, 3, ...] nunca termina

-- [1, 2] ++ [3, 4] = [1, 2, 3, 4] // ++ agrega al final

-- Ejercicio 1: [1,0..(-100)] // devuelve los numeros enteros desde el 1 al -100
-- Ejercicio 2: [-19,(-15)..20] // devuelve los numeros n mod 4 = 0 desde -20 a 20

-- Ejercicio 3:
sumatoria :: [Int] -> Int
sumatoria a | tail a == [] = head a
            | otherwise = head a + sumatoria (tail a)

longitud :: [Int] -> Int
longitud a | tail a == [] = 1
           | otherwise = longitud (tail a) + 1

pertenece :: Int -> [Int] -> Bool
pertenece elto a | head a == elto = True
                 | tail a == [] = False 
                 | otherwise = pertenece elto (tail a)

pertenece2 :: Int -> [Int] -> Bool
pertenece2 n [] = False
pertenece2 n (x : xs) = (n == x) || (pertenece2 n xs)

-- Ejercicios con Pattern Matching
-- [1, 2, 3] = 1 : [2, 3] = 1 : 2 : [3] = 1 : 2 : 3 : []
-- En Haskell hay dos tipos de listas internamente -> [], x : [...]
-- Por convencion lista es xs

sumatoria2 :: [Int] -> Int
sumatoria2 [] = 0
sumatoria2 ( x : xs ) = sumatoria xs + x

longitud2 :: [ a ] -> Int
longitud2 [] = 0
longitud2 ( _ : xs ) = 1 + longitud2 xs

-- Ejercicios: Sin y con pattern matching

productoria :: [Int] -> Int
productoria [] = 1
productoria l | tail l == [] = head l
              | otherwise = head l * productoria (tail l)

productoriaPM :: [Int] -> Int
productoriaPM [] = 1
productoriaPM [a] = a
productoriaPM (a : xs) = a * productoriaPM xs

sumarN :: Int -> [Int] -> [Int]
sumarN n lista | lista == [] = [] 
               | otherwise = (head lista) + n : sumarN n (tail lista)

sumarNPM :: Int -> [Int] -> [Int]
sumarNPM n [] = []
sumarNPM n (x: xs) = x + n : sumarN n (xs)

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs | xs == [] = []
                  | otherwise = sumarN (head xs) xs

sumarElPrimeroPM :: [Int] -> [Int]
sumarElPrimeroPM [] = []reverso :: [Int] -> [Int] 
reverso [] = []
reverso (a:[]) = [a]
reverso (x:xs) = ultimo xs : reverso (x : sinUltimo (tail xs))

sinUltimo :: [Int] -> [Int]
sinUltimo [] = []
sinUltimo xs = listaHasta (longitud xs) xs

listaHasta :: Int -> [Int] -> [Int]
listaHasta h lista = listaHastaDesde 1 h lista

listaHastaDesde :: Int -> Int -> [Int] reverso :: [Int] -> [Int] 
reverso [] = []
reverso (a:[]) = [a]
reverso (x:xs) = ultimo xs : reverso (x : sinUltimo (tail xs))

sinUltimo :: [Int] -> [Int]
sinUltimo [] = []
sinUltimo xs = listaHasta (longitud xs) xs

listaHasta :: Int -> [Int] -> [Int]
listaHasta h lista = listaHastaDesde 1 h lista

listaHastaDesde :: Int -> Int -> [Int] -> [Int]
listaHastaDesde d h (x:xs) | d-1 == h = (x:xs)  
                           | otherwise = listaHastaDesde (d+1) h (x:xs) 

sumarElUltimoPM :: [Int] -> [Int]
sumarElUltimoPM [] = []
sumarElUltimoPM xs = sumarN (ultimo xs) xs

pares :: [Int] -> [Int]
pares [] = []
pares (x: xs) | mod x 2 == 0 = x : pares xs
              | otherwise = pares xs

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs  

-- Sale solo con Pattern Matching PM (??)
-- Intento
{-
reverso :: [Int] -> [Int] 
reverso [] = []
reverso (a:[]) = [a]
reverso (x:xs) = ultimo xs : reverso (x : sinUltimo (tail xs))

sinUltimo :: [Int] -> [Int]
sinUltimo [] = []
sinUltimo xs = listaHasta (longitud xs) xs

listaHasta :: Int -> [Int] -> [Int]
listaHasta h lista = listaHastaDesde 1 h lista

listaHastaDesde :: Int -> Int -> [Int] -> [Int]
listaHastaDesde d h (x:xs) | d-1 == h = (x:xs)  
                           | otherwise = listaHastaDesde (d+1) h (x:xs) 

-}

reverso :: [Int] -> [Int] 
reverso [] = []
reverso (x:xs) = (reverso xs) ++ [x]

max :: Int -> Int -> Int 
max a b | a >= b = a 
        | otherwise = b 

maximo :: [Int] -> Int
maximo x [] = x
maximo (x:xs) = max x (maximo xs)

min :: Int -> Int -> Int 
min a b | a <= b = a 
        | otherwise = b 

minimo :: Int -> Int -> Int 
minimo x [] = x
minimo (x:xs) = min x (maximo xs)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar [x] = [x]
ordenar xs = minimo xs 