-- Tp 1

-- Ejercicio 1
-- Idea: Dos numeros a, b son coprimos si su mcd es 1 -> (a:b) = 1
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | mcd a b == 1 = True
                | otherwise = False

-- Utilizamos el Algoritmo de Euclides para calcular el mcd.
-- (a : b) = (b : a mod b) = ... = (n : 0) = n || (n : 1) = 1
mcd :: Integer -> Integer -> Integer 
mcd a b | b == 0 = abs(a)
        | b == 1 = 1
        | otherwise = mcd (b) (mod a b)

-- Ejercicio 2
-- Idea: Resolvemos el problema para un A generico (a-pseudoprimos) y luego lo aplicamos para 2-pseudoprimos.
a_pseudoprimo :: Integer -> Integer -> Bool
a_pseudoprimo a n | (esPrimo (n) == False) && (mod (a^(n-1)-1) n == 0) && (n /= 1) = True
                  | otherwise = False 

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = a_pseudoprimo 2 n

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo n = a_pseudoprimo 3 n

-- Ejercicio 3
-- Idea: Iteramos desde i=1 hasta m+1, con un contador que incrementa cuando i es 3Pseudoprimo.
-- Cuando i > m, devolvemos el contador (valido para i-1, o sea i=m).  
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = cantidad3PseudoprimosDesde 1 m 0

cantidad3PseudoprimosDesde :: Integer -> Integer -> Integer -> Integer
cantidad3PseudoprimosDesde i m c | i > m = c 
                                 | es3Pseudoprimo (i) = cantidad3PseudoprimosDesde (i+1) m (c+1) 
                                 | otherwise = cantidad3PseudoprimosDesde (i+1) m c

-- Ejercicio 4
-- Idea: Recorremos desde i=2 hasta que el contador c llegue a cero (c=0 -> fin),
-- Le restamos uno a c cuando i cumple propiedad de ser 2 y 3 pseudoprimo.
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo n = kesimo2y3PseudoprimoDesde 2 n

kesimo2y3PseudoprimoDesde :: Integer -> Integer -> Integer
kesimo2y3PseudoprimoDesde i c | (c == 0) = i-1
                              | (es2Pseudoprimo (i) && es3Pseudoprimo(i)) = kesimo2y3PseudoprimoDesde (i+1) (c-1)
                              | otherwise = kesimo2y3PseudoprimoDesde (i+1) (c)

-- Ejercicio 5
-- Idea: Recorrer desde d=1 hasta d=n-1 y verificar que cuando d sea coprimo con n, n sea d-pseudoprimo :: Definicion de Carmichael.
esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelDesde 1 (n)

esCarmichaelDesde :: Integer -> Integer -> Bool
esCarmichaelDesde d n | (sonCoprimos d n) && (a_pseudoprimo d n == False) = False
                      | (d == n-1) = (sonCoprimos d n) && (a_pseudoprimo d n) || (sonCoprimos d n == False)
                      | otherwise = esCarmichaelDesde (d+1) n

-- Funciones auxiliares (Implementadas en clase)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k 
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo x = (menorDivisor x == x)