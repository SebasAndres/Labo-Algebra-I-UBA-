-- Mie 26-10-22
-- Combinatoria

-----------------------------------------------------------------------------------------------------------------------------
-- Ejercicios 1: Numero Combinatorio

-- Definicion alternativa
combinatorio :: Integer -> Integer -> Integer
combinatorio n m | m == 0 = 1
                 | n == m = 1
                 | otherwise = combinatorio (n-1) m + combinatorio (n-1) (m-1) 

-- Definicion normal (no recursiva)
combinatorio_ :: Integer -> Integer -> Integer
combinatorio_ n m = div (fact (n)) (fact (m) * fact (n-m))

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

-----------------------------------------------------------------------------------------------------------------------------
-- Ejercicios 2: 
-- Variaciones :: Set Int -> Int -> Set [Int] que dado un conjunto c y una longitud l genere todas las posibles listas de longitud l a partir de elementos de c

type Set a = [a]

agregarElemEnCadaLista :: Integer -> Set [Integer] -> Set [Integer]
agregarElemEnCadaLista x [] = []
agregarElemEnCadaLista x (ls:lss) = (x:ls) : agregarElemEnCadaLista x lss                                

agregarTodosEnCadaLista :: Set Integer -> Set [Integer] -> Set [Integer]
agregarTodosEnCadaLista [] listas = [] -- Porque sino me agrega la lista al final // Si lo dejo como listas me queda que variaciones es partes
agregarTodosEnCadaLista (x:xs) listas = agregarElemEnCadaLista x listas ++ agregarTodosEnCadaLista xs listas

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones xs 0 = [[]]
variaciones xs l = agregarTodosEnCadaLista xs (variaciones xs (l-1))

-- [1,2] : [3, 4] = ERROR
-- [1,2] ++ [3, 4] = [1,2,3,4]                                      

-----------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 3: Por Divide & Conqueer : Divido el problema en tras funciones mas sencillas.
-- Encuentro patrones por ejemplos, por tests, y aplico mi idea como hipotesis inductiva (podria probarlo por induccion qe vale siempre).

-- Precondicion: 1 <= i <= longitud (xs) + 1
insertarEn :: [Integer] -> Integer -> Integer -> [Integer] 
insertarEn xs n 1 = n : xs
insertarEn (x:xs) n index = x : (insertarEn xs n (index-1))

insertarEnTodosLados :: [Integer] -> Integer -> Set [Integer]
insertarEnTodosLados xs n = insertarEnTodosLadosHasta xs n (longitud(xs)+1)

--                              lista       elem       index 
insertarEnTodosLadosHasta :: [Integer] -> Integer -> Integer -> Set [Integer]
insertarEnTodosLadosHasta xs n 1 = [n:xs]
insertarEnTodosLadosHasta xs n idx = (insertarEn xs n idx) : (insertarEnTodosLadosHasta xs n (idx-1))

insertarEnTodosLadosEnTodasListas :: Set [Integer] -> Integer -> Set[Integer]
insertarEnTodosLadosEnTodasListas [] n = []
insertarEnTodosLadosEnTodasListas (l:ls) n = (insertarEnTodosLados l n) ++ (insertarEnTodosLadosEnTodasListas ls n) 

-- Precondicion: n >= 1
permutaciones :: Integer -> Set [Integer]
permutaciones 1 = [[1]] 
permutaciones n = insertarEnTodosLadosEnTodasListas (permutaciones (n-1)) n  

-- aux
longitud :: [Integer] -> Integer
longitud a | tail a == [] = 1
           | otherwise = longitud (tail a) + 1

-----------------------------------------------------------------------------------------------------------------------------
-- Ejercicios Tarea:

-- 1) Ubicar n bolitas en k cajas: Hay varias formas de hacer soluciones (idea de como representar un problema)
-- Idea de representacion: {cajaDeBolita1, cajaDeBolita2, ..., cajaDeBolitaN}
-- En difinitiva es variaciones

bolitasEnCajas :: Integer -> Integer -> Set [Integer]
bolitasEnCajas n k = variaciones [1..k] n
