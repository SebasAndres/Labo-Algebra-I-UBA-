-- EJERCICIO 1. sonCoprimos
-- Idea: Evaluar si el mcd (a, b) == 1 con el Algoritmo de Euclides

mcd :: Integer -> Integer -> Integer 
mcd a b | b == 0 = abs(a)
        | b == 1 = 1
        | otherwise = mcd (b) (mod a b)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | (mcd a b == 1) = True
                | otherwise = False

-- EJE3RCICO 2: es2Pseudoprimo
-- Idea: Resolvemos el problema para a_pseudoprimo y lo llevamos al caso particular

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = a_pseudoprimo n 2

a_pseudoprimo :: Integer -> Integer -> Bool 
a_pseudoprimo n a | (esPrimo(n) == False) && (mod (a^(n-1)-1) n == 0) = True
                  | otherwise = False

-- EJERCICIO 3: cantidad3Pseudoprimos
-- Idea: recorrer todos los numeros de i hasta M y sumar uno cuando hay un pseudoprimo, terminar cuando i == M
-- Alternativa: recorrer los a_pseudoprimos[i] y parar cuando a[i] >= M. devolver i

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = -1 + cantidad3PseudoprimosDesde 1 m 0

cantidad3PseudoprimosDesde :: Integer -> Integer -> Integer -> Integer
cantidad3PseudoprimosDesde i m c | i == m = c 
                                 | es3Pseudoprimo (i) = cantidad3PseudoprimosDesde (i+1) m (c+1)   
                                 | otherwise = cantidad3PseudoprimosDesde (i+1) m (c) 

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo n = a_pseudoprimo n 3

-- EJERCICIO 4: kesimo2y3Pseudoprimo
-- Idea: Iterar hasta reducir k=0, incrementando el punto de partida d en cada vuelta d++

kesimo2y3Pseudoprimo :: Integer -> Integer -- devuelve el k-esimo numero que es 2-pseudoprimo y 3-pseudoprimo
kesimo2y3Pseudoprimo n = kesimo2y3PseudoprimoDesde 2 n

kesimo2y3PseudoprimoDesde :: Integer -> Integer -> Integer 
kesimo2y3PseudoprimoDesde d k | (k == 0) = d-1 -- porque k llego a cero en el paso anterior
                              | (es2Pseudoprimo d && es3Pseudoprimo d) = kesimo2y3PseudoprimoDesde (d+1) (k-1)
                              | otherwise = kesimo2y3PseudoprimoDesde (d+1) (k)

-- EJERCICIO 5: esCarmichael
-- Idea: Recorro los numeros desde i=1 a n, y evaluo que la condicion se cumpla en los i coprimos con n.
-- Si llega a i=n-1 y nunca incumplio la condicion (i,n coprimos -> n es i-pseudoprimo), devuelve True 

esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelDesde 1 n

esCarmichaelDesde :: Integer -> Integer -> Bool
esCarmichaelDesde d n | (sonCoprimos d n == True) && (a_pseudoprimo n d == False) = False -- coprimo pero no apseudoprimo  
                      | (d == n-1) = (sonCoprimos d n == True) && (a_pseudoprimo n d == True) 
                      | otherwise = esCarmichaelDesde (d+1) n

-------------------------------------------------------
--- Funciones Auxiliares (Implementadas en Clase) -----

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k 
                      | otherwise = menorDivisorDesde n (k+1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo x = (menorDivisor x == x)
