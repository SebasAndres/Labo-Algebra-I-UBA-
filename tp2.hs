-- Tp 2: Laboratorio - Algebra I

-- Andres Sebastian Ignacio
-- Geslin Ariel Nicolas
-- Gonzalez Dardik Micaela Natali

-- Definimos el tipo Complejo:
type Complejo = (Float, Float)

-- Ejercicio 1: 

-- [1.1]
re :: Complejo -> Float
re a = fst a

-- [1.2]
im :: Complejo -> Float
im a = snd a

-- [1.3]
suma ::  Complejo -> Complejo -> Complejo
suma a b = ((re a + re b), (im a + im b))

-- [1.4]
producto ::  Complejo -> Complejo -> Complejo
producto a b = ((re a * re b) - (im a * im b), (re a * im b) + (im a * re b))

-- [1.5]
conjugado :: Complejo -> Complejo
conjugado a = (re a, - (im a))

norma :: Complejo -> Float
norma a = sqrt ((re a)^2 + (im a)^2)

-- verificar // mejorar
angulo :: Complejo -> Float
angulo (a,b) | a > 0 && b > 0 = acos (a / norma(a,b))
             | a < 0 && b > 0 = acos (a / norma(a,b))
             | a < 0 && b < 0 = acos (a / norma(a,b)) -- atan (a / b) + pi   
             | a > 0 && b < 0 = atan (a / b) + 2*pi
             | b == 0 && a > 0 = 0
             | b == 0 && a < 0 = pi 
             | a == 0 && b < 0 = 3/2 * pi
             | a == 0 && b > 0 = pi / 2
-- [1.6]
inverso :: Complejo -> Complejo
inverso a = ((re a) / (norma a)^2 , (- im a) / (norma a)^2)

-- [1.7]
cociente :: Complejo -> Complejo -> Complejo
cociente z w = ((norma(z)/norma(w))*cos(angulo(z)-angulo(w)),
                (norma(z)/norma(w))*sin(angulo(z)-angulo(w)))

-- [1.8]
-- fijarse si hay otra forma de cambiar el tipo
potencia :: Complejo -> Integer -> Complejo
potencia z k = ((norma(z)^k)*cos(toFloat(k)*angulo z), (norma(z)^k)*sin(toFloat(k)*angulo z))

toFloat :: Integer -> Float
toFloat n = fromIntegral n 

-- [1.9]
raicesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
raicesCuadratica a b c | disc >=0 = (((-b+sqrt(disc))/(2*a), 0), ((-b-sqrt(disc))/(2*a), 0))
                       | otherwise = ((-b/(2*a), -sqrt(-disc)/(2*a)), (-b/(2*a), sqrt(-disc)/(2*a)))
                       where disc = b**2-4*a*c

-- Ejercicio 2:
-- [2.1]
modulo :: Complejo -> Float
modulo z = norma z

-- [2.2]
distancia :: Complejo -> Complejo -> Float
distancia z w = modulo ((re z - re w), (im z - im w))

-- [2.3]
-- Chequear!
argumento :: Complejo -> Float 
argumento z = angulo z

-- reducir por 2*pi da error por redondear
reducir :: Float -> Float
reducir n | n >= 2*pi = reducir (n-2*pi)
          | otherwise = n

-- [2.4]
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r t = (r*cos(t), r*sin(t))

-- [2.5]
raizCuadrada :: Complejo -> (Complejo, Complejo)
raizCuadrada z = (((modulo(z)**(1/n)*cos(argumento(z)/n)), modulo(z)**(1/n)*sin(argumento(z)/n)),
                  ((modulo(z)**(1/n)*cos((argumento(z)+2*pi)/n)), modulo(z)**(1/n)*sin((argumento(z)+2*pi)/n)))
                  where n = 2

raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (cociente (suma b (fst (raizCuadrada w))) (producto (2, 0) a),
                                  cociente (suma b (snd (raizCuadrada w))) (producto (2, 0) a))
                                where w = resta (potencia b 2) (producto (producto (4, 0) a) c) 

resta :: Complejo -> Complejo -> Complejo
resta z w = (fst z - fst w, snd z - snd w)

-- Ejercicio 3: 
-- [3.1]
raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasHasta n (n-1)

raicesNEsimasHasta :: Integer -> Integer -> [Complejo]
raicesNEsimasHasta n 0 = [raizNKesima n 0]
raicesNEsimasHasta n fin = raizNKesima n fin : raicesNEsimasHasta n (fin-1)

raizNKesima :: Integer -> Integer -> Complejo
raizNKesima n k = (cos(theta), sin(theta)) where theta = 2*pi*toFloat(k)/toFloat(n)

-- [3.2]
sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] error = True
sonRaicesNEsimas n (c:cs) error | tiendeAUno c n error = sonRaicesNEsimas n cs error -- abs (z^n - 1) < error
                                | otherwise = False

tiendeAUno :: Complejo -> Integer -> Float -> Bool 
tiendeAUno z n error | esDivisor (2*pi/n**2) (argumento z) n && abs ((modulo z)^n - 1) < error = True
                     | otherwise = False

-- z = r*e^(theta*i) 
-- z^n = r^n * e^(n*theta*i)
-- 1 = 1^n * e^(2*pi*k/n)

-- chequear que 
--     r^n == 1
--     n*theta = 2*pi*k/n <--> theta = (2*pi/n^2) * k, k = {0, .., n-1} 

-- Existe k = {0, .. , n-1} tal que theta = k * arg?

esDivisor :: Float -> Float -> Integer -> Bool
esDivisor theta arg n = esDivisorDesde theta arg n 0

esDivisorDesde :: Float -> Float -> Integer -> Integer -> Bool
esDivisorDesde theta arg n k | theta == arg * toFloat(k) = True
                             | k == n = False
                             | otherwise = esDivisorDesde theta arg n (k+1)

-- Dudas: 
-- 1) Como soluciono el problema de tipos (multiplicar float con int, redondeos en trigonometricas, etc..)
--    Ejemplo: Ver funcion potencia [1.9]
-- 2) SonRaicesNEsimas? [3.2]
-- 3) Argumento

