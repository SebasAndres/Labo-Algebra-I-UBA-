-- 09-11-22

-- Recuerdo Alg. Euclides Extendido:
--                               g=mcd       s       t
emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd a 0 = (a, 1, 0)
emcd a b = (g, s, t)
        where (g, s', t') = emcd b (mod a b)
              s = t'
              t = s' - t' * q
              q = div a b

mcd :: Integer -> Integer -> Integer 
mcd a b | b == 0 = abs(a)
        | otherwise = mcd (b) (mod a b)

------------------------------------------------------------------------
-- Ecuaciones Diofanticas Ax + By = C
-- Una diofantica tiene solucion sii (A : B) | C

-- Tiene solucion AX = B (mod M)??
tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m | mod b (mcd a m) == 0 = True
                    | otherwise = False

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m | tieneSolucion a b m = s * d
                           where (g, s, t) = emcd a m
                                 d = div b g

--                                                   sol part, congruencia 
solucionGeneral :: Integer -> Integer -> Integer -> (Integer, Integer)
solucionGeneral a b m | tieneSolucion a b m = (s*d, div m g)
                        where (g, s, t) = emcd a m
                              d = div b g  