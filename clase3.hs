{-

Mie 07.09.22 - Recursion

-} 

-- factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1) 

-- o por Pattern Matching:

-- _factorial :: Int -> Int
_factorial 0 = 1
_factorial n = factorial(n-1) * n

-- Al asignarle un tipo, toma valores limitados -> no muy grandes.
-- Teoricamente x! = inf para x < 0 

-- Funcion problematica ejemplo :: esta mal
mal_esPar :: Int -> Bool
mal_esPar n | n == 0 = True
            | otherwise = mal_esPar(n-2)
-- Pero nunca puede dar Falso,
--  falta agregar un caso base

esPar :: Int -> Bool
esPar n | n == 0 = True
        | n == 1 = False
        | otherwise = esPar(n-2)

-- esPar2 computa la mitad de numeros que la de arriba
esPar2 :: Int -> Bool
esPar2 n | n == 0 = True
         | otherwise = not ( esPar (n -1) )

-- Ejercicios: 

-- 1. Sucesion de Fibonacci
-- fib :: (Int t) => t -> t
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)

-- 2. Calcular parte entera de ...
parteEntera :: Float -> Int -- Sirve solo en Z >= 0
parteEntera n | (0 < n) && (n < 1) = 0
              | otherwise = parteEntera(n-1) + 1   

parteEnteraNeg :: Float -> Int
parteEnteraNeg n | (0 < abs(n)) && (abs(n) < 1) = 0
                 | otherwise = parteEnteraNeg(n+1) - 1                 

-- 3. n es multiplo de 3?
mult3 :: Int -> Bool
mult3 n | n == 3 = True
        | n == 2 = False
        | n == 1 = False
        | n == 0 = False
        | otherwise = mult3 (n-3)

-- 4. Suma impares
-- impares = {1, 3, 5, 7, ...} 
-- impar n devuelve el impar de en la posicion n (index desde 1)
impar :: Int -> Int
impar n | n == 1 = 1
        | n <= 0 = 0
        | otherwise = impar(n-1) + 2 

-- sumaImpares suma los primeros n impares
sumaImpares :: Int -> Int
sumaImpares n | impar(n) == 1 = 1
              | otherwise = impar(n) + sumaImpares(n - 1)
              
              -- ejemplo visual para sumaImpares(3)
              -- 5 + sumaImpares(2) 

--5. Medio Fact n!= n*(n-2)*(n-4)
medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = n * medioFact(n-2)

-- ejemplo de uso
-- medioFact 5 = 5 * mediofact(5-2) * mediofact(5-4) 
--                         3        *    1

--6 Escribir una func... suma de digitos
sumDigitos :: Int -> Int
sumDigitos n | n < 10 = n
             | otherwise = sumDigitos (div n 10) + mod n 10

--7. Digitos Iguales
digitosIguales :: Int -> Bool
digitosIguales n | n < 10 = True
                 | otherwise = digitosIguales(div n 10) && (mod (div n 10) 10 == mod n 10)

{-
    Idea: 
    n < 10 : True
    digitosIguales(numeroSinUltimaCifra) && ultimaCifra(n) = ultimaCifra(numeroSinUltimaCifra)

    Ejemplo:
    caso n = 111
    1. digIguales(11) and ultdig == ultDig(11)
-s}
