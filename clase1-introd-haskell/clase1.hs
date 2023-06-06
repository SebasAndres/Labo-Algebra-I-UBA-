-- Mie 17.08.2022 
-- Introduccion a Haskell

-- funcion 1
doble x = 2 * x

-- funcion 2
-- f x y = x * x + y * y            // no se puede reescribir funciones 

-- funcion 3    
g x y z = x + y + z * z

-- ejercicios
suma x y = x + y
norma x1 x2 = sqrt(x1**2 + x2**2)
funcionConstante8 x = 8

-- div es division entera & mod su resto.
-- las otras operaciones son +- iguales a otros lenguajes.
-- a |= b significa "a != b"

-- funciones partidas: recorre de arriba a abajo
fp0 n | n == 0 = 1
      | n /= 0 = 0

-- o de otra forma
fp1 n | n == 0 = 1
      | otherwise = 0

signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1


prueba1 n | n >= 3 = 5 -- throws Exception: Non-exhaustive ... 

-- Pattern Matching: otra forma de definir fp0 --> fpo
fpo 0 = 1
fpo n = 0

-- Cantidad de soluciones de una cuadratica
-- * Sea X**2 + bX + c = 0. Segun el valor del discriminante delta, hay tantas soluciones
-- ** si delta > 0 hay dos soluciones, si delta == 0 hay 1, sino hay 0

cantidadDeSoluciones b c | delta > 0 = 2
                         | delta == 0 = 1
                         | otherwise = 0
                         where delta = b**2 - 4*c

-- Tipos
-- Leer tipo que haskell asume de tu funcion
-- cmd: ":t nombrefuncion"
-- Para definir tipo escribir:
-- nombreFuncion :: Type -> Type 
-- nombreFuncion x = 2 * x
-- Types = {Int, Float, Bool, ...} (?)

-- Se puedem definir funciones con el interprete usando "let" antes.

-- Ejemplos de tipos:
--        var1   var2   rdo
maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y -- ~ funcion raratherwise = y

esPar :: Int -> Bool
esPar n | (mod n 2 == 0) = True
        | otherwise = False

-- otra forma de escribir "esPar"
esParOptimo :: Int -> Bool
esParOptimo n = mod n 2 == 0 -- Devuelve True o False segun la condicion

esImpar :: Int -> Bool
esImpar n = not (esPar n)

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y z = (x >= y) || z -- si z==True devuelve z o bool(x >= y), lee de izq a der.
-- otra forma de escribirla
funcionRara _ _ True = True
funcionRara x y False = x >= y

-- ejercicio final de la clase 1:
absoluto :: Int -> Int
absoluto n | n >= 0 = n
           | otherwise = -1 * n

maximoabsoluto :: Int -> Int -> Int  
maximoabsoluto n1 n2 | absoluto(n1) > absoluto(n2) = absoluto(n1)
                     | otherwise = absoluto(n2)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = maximo (maximo x y) z 

algunoEs0 :: Int -> Int -> Bool
algunoEs0 a b = (a==0) || (b==0)

ambosSon0 :: Int -> Int -> Bool
ambosSon0 a b = (a==0) && (b==0)

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = (mod a b == 0) 

digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10

digitoDecenas :: Int -> Int
digitoDecenas n = mod n 12 
