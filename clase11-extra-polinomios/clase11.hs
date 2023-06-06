-- clase extra

type Polinomio = [Float]

limpiar :: [Float] -> Polinomio
limpiar [t] | t == 0 = []
            | otherwise = [t]
limpiar (x:xs) | x == 0 = limpiar(xs)
               | otherwise = x : xs

grado :: Polinomio -> Int
grado [] = undefined
grado p = length (limpiar (p))

evaluar :: Polinomio -> Float -> Float
evaluar [p] a = p
evaluar (p:ps) a = p * a^(length (p:ps) -1) + evaluar ps a

suma :: Polinomio -> Polinomio -> Polinomio
suma p q | grado(p) >= grado(q) = sumaMismoGrado p (agregarNCeros (grado(p)-grado(q)) q)
         | grado(p) < grado(q) = sumaMismoGrado q (agregarNCeros (grado(q)-grado(p)) p)

agregarNCeros :: Int -> Polinomio -> Polinomio
agregarNCeros n p = listaN0 n ++ p 

listaN0 :: Int -> [Float]
listaN0 0 = []
listaN0 n = 0 : listaN0 (n-1)

sumaMismoGrado :: Polinomio -> Polinomio -> Polinomio
sumaMismoGrado [a] [b] = [a+b]
sumaMismoGrado (a:as) (b:bs) = a+b : sumaMismoGrado as bs

type Monomio = (Float, Int)

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar a [p] = [a*p]
productoPorEscalar a (p:ps) = a*p : productoPorEscalar a ps

resta :: Polinomio -> Polinomio -> Polinomio
resta p q = suma p (cambiarSigno q)

cambiarSigno :: Polinomio -> Polinomio
cambiarSigno [p] = [-p]
cambiarSigno (p:ps) = -p : cambiarSigno ps