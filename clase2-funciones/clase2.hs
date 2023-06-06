-- Mier. 31-08-22 - Labo Algebra I

{-

los types quedan restringidos por las operaciones de la funcion
p indica variable libre
existen clases de tipos que limitan las operaciones que se pueden hacer con ellos

:t nombrefuncion para ver la clase de tipos que Haskell le asigna a la funcion
triple x = x * x
:t triple (en terminal)

clases de tipos = { Integral, Fractional, Floating, Num, Ord, Eq }
cada una integrada por subconjuntos

nombre :: (Num t, Ord t) => t -> -> t -> Imt
significa que t debe pertenecer en Num y en Ord

Obs(##) El retorno de las funciones SIEMPRE DEBE TENER EL MISMO TIPO.

-}

-- Ejercicio 1: Averiguar el tipado que asigna Haskell en las funciones dadas.

f1 x y z = x**y + z <= x+y**2 -- return Bool
-- f1 :: (Ord a, Floating a) => a -> a -> a -> Bool

f2 x y = (sqrt x) / (sqrt y)
-- f2 :: Floating a => a -> a -> a

f3 x y = div (sqrt x) (sqrt y)
-- f3 :: (Integral a, Floating a) => a -> a -> a

f4 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y
-- f4 :: (Eq p, Floating p) => p -> p -> p -> p
-- Obs(##) El retorno de las funciones SIEMPRE DEBE TENER EL MISMO TIPO.

f5 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z
--f5 :: (Eq a, Floating a) => a -> a -> p -> p

{- .

K tupla como (Float, Float, ...)

En pattern matching no funciona repetir variables en la declaracion

-- Diferentes formas de asignar funciones :: Funciones Binarias
-- 3 + 5    // es operador infijo
-- (+) 3 5  // es operador prefijo ?

-- puedo pedir :t (**)    // sirve para float
-- y lo comparo con :t (^) // sirve para int

Puedo pasar de infijo a prefijo de la forma:
3 ´mod´ 5

Pattern Matching: 
esOrigen :: ( Float , Float ) -> Bool
esOrigen (0 , 0) = True
esOrigen (_ , _ ) = False
angulo0 :: ( Float , Float ) -> Bool
angulo0 (_ , 0) = True
angulo0 (_ , _ ) = False
{ -
No podemos usar dos veces la misma variable
angulo45 :: ( Float , Float ) -> Bool
angulo45 (x , x ) = True
angulo45 (_ , _ ) = False

angulo45 :: ( Float , Float ) -> Bool
angulo45 (x , y ) = x == y

-}

-- EJERCICIOS: IMPLEMENTAR LAS FUNCIONES

{- Ejercicio 1: estanRelacionados: dados dos n ́umeros reales, decide si est ́an relacionados considerando
la relaci ́on de equivalencia en R cuyas clases de equivalencia son:
(−∞, 3], (3, 7] y (7, ∞)
-}

estanRelacionados :: (Ord t, Num t) => t -> t -> Bool

estanRelacionados x y | (x <= 3) && (y <= 3) = True -- conj 1
                      | ((x > 3) && (x <= 7)) && (y > 3) && (y <= 7) = True -- conj 2 
                      | (x > 7) && (y>7) = True -- conj 3
                      | otherwise = False

_estanRelacionados x y = ((x <= 3) && (y <= 3)) || ((x > 3) && (x <= 7)) && (y > 3) && (y <= 7) || (x > 7) && (y>7) 

{-Ejercicio 2: prodInt: calcula el producto interno entre dos vectores de R2. -}

prodInt :: (Floating t) => (t, t) -> (t, t) -> t
prodInt (vx, vy) (wx, wy) = vx * vy + wx * wy

{-Ejercicio 3: todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer
vector es menor a la coordenada correspondiente del segundo vector. -}

todoMenor :: (Ord t, Num t) => (t, t) -> (t, t) -> Bool
todoMenor (vx, vy) (ux, uy) = (vx < ux) && (vy < uy) 

{-Ejercicio 4: distanciaPuntos: calcula la distancia entre dos puntos de R2 -}

distanciaPuntos :: (Floating t) => (t, t) -> (t, t) -> t
distanciaPuntos (px, py) (qx, qy) = sqrt ((px - qx)**2 + (py - qy)**2) 
                          
{- Ejercicio 5: sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos -}

sumaTerna :: (Integral t) => (t, t, t) -> t
sumaTerna (a, b, c) = a + b + c

{- Ejercicio 6: posicPrimerPar: dada una terna de enteros, devuelve la posici ́on del primer n ́umero par si
es que hay alguno, y devuelve 4 si son todos impares -}

posicPrimerPar :: (Integral t) => (t, t, t) -> t
posicPrimerPar (a, b, c) | mod a 2 == 0 = a
                         | mod b 2 == 0 = b 
                         | mod c 2 == 0 = c     
                         | otherwise = 4

{- Ejercicio 7: crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por
separado (debe funcionar para elementos de cualquier tipo) -}

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

{- Ejercicio 8: invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como par ́ametro
(debe funcionar para elementos de cualquier tipo) -}

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

-- Para ser modulo, el archivo debe empezar con mayuscula

