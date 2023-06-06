-- Tp 2: Laboratorio - Algebra I

-- Andres Sebastian Ignacio | sebastian.ignacio.andres@gmail.com  
-- Geslin Ariel Nicolas | arielgeslin@gmail.com
-- Gonzalez Dardik Micaela Natali | micagonzdark@gmail.com

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

-- [1.6]
inverso :: Complejo -> Complejo
inverso a = ((re a) / (norma a)^2 , (- im a) / (norma a)^2)

-- [1.7]
cociente :: Complejo -> Complejo -> Complejo
cociente z (0, 0) = undefined
cociente (0,0) w = (0, 0) 
cociente z w = ((norma(z)/norma(w))*cos(angulo(z)-angulo(w)),
                (norma(z)/norma(w))*sin(angulo(z)-angulo(w)))

-- [1.8]
potencia :: Complejo -> Integer -> Complejo
potencia z 0 = (1.0, 0.0)
potencia z k = producto z (potencia (z) (k-1))

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
angulo :: Complejo -> Float
angulo (a,b) | cuadrante (a,b) == 1 = atan (b/a) -- Cuadrante 1
             | cuadrante (a,b) == 2 = pi + (atan (b/a)) -- Cuadrante 2
             | cuadrante (a,b) == 3 = pi + (atan (b/a)) -- Cuadrante 3
             | otherwise = 2*pi + (atan (b/a)) -- Cuadrante 4

cuadrante :: Complejo -> Int
cuadrante (a,b) | a >= 0 && b >= 0 = 1
                | a <= 0 && b >= 0 = 2
                | a < 0  && b <= 0 = 3
                | a >= 0 && b <= 0 = 4

argumento :: Complejo -> Float 
argumento z = angulo z

-- [2.4]
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r t = (r*cos(t), r*sin(t))

-- [2.5]
-- Devuelve los w tal que w^2 = z input
raizCuadrada :: Complejo -> (Complejo, Complejo)
raizCuadrada (0.0, 0.0) = ((0.0, 0.0), (0.0, 0.0))
raizCuadrada z = (((modulo(z)**(1/n)*cos(argumento(z)/n)), modulo(z)**(1/n)*sin(argumento(z)/n)),
                  ((modulo(z)**(1/n)*cos((argumento(z)+2*pi)/n)), modulo(z)**(1/n)*sin((argumento(z)+2*pi)/n)))
                  where n = 2

-- [2.6]
-- Resuelve el sist para a,b,c complejos, usando la func ya implementada raizCuadrada sobre el discriminante w
raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (cociente (suma (producto (-1,0) b) (fst (raizCuadrada w))) (producto (2, 0) a),
                                  cociente (suma (producto (-1,0) b) (snd (raizCuadrada w))) (producto (2, 0) a))
                                where w = resta (potencia b 2) (producto (producto (4, 0) a) c) 

resta :: Complejo -> Complejo -> Complejo
resta z w = (fst z - fst w, snd z - snd w)

-- Ejercicio 3: 
-- [3.1]
-- Crea todas las raicesNEsimas por recursion hasta n-1
raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasHasta n (n-1)

raicesNEsimasHasta :: Integer -> Integer -> [Complejo]
raicesNEsimasHasta n 0 = [raizNKesima n 0] -- Si es  la primera entre corchetes para concatenar con el resto
raicesNEsimasHasta n fin = raizNKesima n fin : raicesNEsimasHasta n (fin-1)
-- En este caso va desde k=n-1 a k=0

-- Devuelve la k raizNEsima con k entre 0 y n-1
raizNKesima :: Integer -> Integer -> Complejo
raizNKesima n k = (cos(theta), sin(theta)) where theta = 2*pi*toFloat(k)/toFloat(n)

-- [3.2]
-- Chequeo por recursion, corta el circuito cuando alguna raiz falla (no tiende a 1)
sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] error = True
sonRaicesNEsimas n (c:cs) error | tiendeAUno c n error = sonRaicesNEsimas n cs error -- abs (z^n - 1) < error
                                | otherwise = False

tiendeAUno :: Complejo -> Integer -> Float -> Bool 
tiendeAUno z n error | modulo ( resta zn (1, 0) ) < error = True
                     | otherwise = False
                      where zn = potencia z n 



