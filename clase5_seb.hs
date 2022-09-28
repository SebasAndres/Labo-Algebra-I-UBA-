-- clase 5 Labo Algebra I - 28-09-22 - Sebastian Andres

-- suma todos los divisores de x hasta y
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta x 1 = 1
sumaDivisoresHasta x y | (mod x y == 0) = sumaDivisoresHasta x (y-1) + y
                       | otherwise = sumaDivisoresHasta x (y-1)

-- para crear la funcion sumaDivisores, resolvi el problema general y lo lleve a lo particular
sumaDivisores :: Int -> Int 
sumaDivisores a = sumaDivisoresHasta a a

-- Ejercicios

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | mod n k == 0 = k -- porque como recorremos de men -> may, k es menor
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo x = (menorDivisor x == x)

nEsimoPrimo :: Int -> Int 
nEsimoPrimo n = nEsimoPrimoHasta n 2 -- p es un natural mayor a 2, min(p) = 2, recorre todos los N

nEsimoPrimoHasta :: Int -> Int -> Int
nEsimoPrimoHasta n p | n == 0 = p-1 -- porque cuando n = 1 -> entra en la segunda rama
                     | esPrimo p = nEsimoPrimoHasta (n-1) (p+1)
                     | otherwise = nEsimoPrimoHasta n (p+1)

-- Ejercicios 

-- Me pide el menor numero 1 <= m <= n tal que n es un k!
menorFactDesde :: Int -> Int
menorFactDesde m = kFactorialDesde m 1

-- Devuelvo el primer k! mayor igual a m
kFactorialDesde :: Int -> Int -> Int
kFactorialDesde m k | (factorial(k) >= m) = factorial(k)
                    | otherwise = kFactorialDesde m (k+1)

mayorFactHasta :: Int -> Int 
mayorFactHasta m = kFactorialHasta m 1

-- Me Paso y devuelvo el mayor k! menor a m
kFactorialHasta :: Int -> Int -> Int
kFactorialHasta m k | (factorial (k) > m) = factorial (k-1)
                    | otherwise = kFactorialHasta m (k+1)

esFact :: Int -> Bool
esFact m = (mayorFactHasta m == m)
-- Tambien vale, esFact m == (menorFactDesde m == m)

-- aux tipica
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Ejercicios

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciDesde n 1

-- Recorro la suc de fibonacci hasta que fib(k) >= n 
esFibonacciDesde :: Int -> Int -> Bool
esFibonacciDesde n k | fib(k) > n = False
                     | fib(k) < n = esFibonacciDesde n (k+1)
                     | otherwise = True -- fib(k) == n

-- aux tipica
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-2) + fib (n-1)

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde n 0

-- sumo todos los primos desde h=1 hasta sumaPrimos(h) > n o sumaPrimos(h) == n
-- sumaPrimos (h) = nEsimoPrimo(h) + nEsimoPrimo(h-1) + ... + 3 + 2

esSumaInicialDePrimosDesde :: Int -> Int -> Bool
esSumaInicialDePrimosDesde n h | sumaPrimos(h) > n = False
                               | sumaPrimos(h) < n = esSumaInicialDePrimosDesde n (h+1)
                               | otherwise = True -- caso sumaPrimos(h) == n

sumaPrimos :: Int -> Int
sumaPrimos n | n == 0 = 0
             | nEsimoPrimo(n) + nEsimoPrimo(n-1)

-- Ejercicios Tarea
-- La idea es recorrer todos los numeros desde n1 a n2 (o viceversa) y dar el sumaDivisores(i) maximo
tomaValorMax :: Int -> Int -> Int 
tomaValorMax n1 n2 = tomaValorMaxDesde n1 n2 n1

tomaValorMaxDesde :: Int -> Int -> Int -> Int
tomaValorMaxDesde n1 n2 m | sumaDivisores (m) > ... = m 
                          |    

{- Apuntes

Intentos raros
---------------
(recorrido de atras para adelante, de n a 2)

menorDivisorHasta :: Int -> Int -> Int
menorDivisorHasta n q | mod n (q) == 0 = q
                      | otherwise = menorDivisor n (q+1)
-- para q > 2

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorHasta n n 

-----------------------------------
Ejemplo implementacion nEsimoPrimo

nEsimoPrimo 3
nEsimoPrimoHasta 3 2
nEsimoPrimoHasta (3-1) (2+1)
nEsimoPrimoHasta 2 3
nEsimoPrimoHasta (2-1) (3+1)
nEsimoPrimoHasta 1 4
nEsimoPrimoHasta 1 (4+1)
nEsimoPrimoHasta 1 5
nEsimoPrimoHasta (1-1) (5+1)
nEsimoPrimoHasta 0 6
p - 1 => 6 - 1
5

-}