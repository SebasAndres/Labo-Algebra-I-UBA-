-- Clase 7 - Sebastian Andres - 12/10/22

-- Conjuntos
-- El orden de los elementos es relevante para las listas, pero no para conjuntos.

type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar n c | elem n c = c -- elem(n, c) = (n in c?)
            | otherwise = n : c  

-- incluido: Usar recursion para achicar cjts y comparar
incluido :: Set Int -> Set Int -> Bool
incluido [] b = True
incluido (x:xs) b | (elem x b) && (incluido xs b) = True
                  | otherwise = False

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = (incluido c1 c2) && (incluido c2 c1)

-- Partes (n) = Partes (n-1) + [C+n for C in Partes(n-1)] 
partes :: Int -> Set (Set Int)
partes 0 = [[]] 
partes n = partes (n-1) ++ agregarATodos n (partes (n-1)) 

-- Idea: [C+n for C in Partes(n-1)] 
agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos n [] = []
agregarATodos n (ls:lss) = (agregar n ls) : (agregarATodos n lss)

-- A x B = {(a,b) for a,b in A, B}
productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] b = []
productoCartesiano (ax : axs) b = armarTuplas ax b ++ productoCartesiano axs b 

armarTuplas :: Int -> Set Int -> Set (Int, Int)
armarTuplas n [] = []
armarTuplas n (x:xs) = (n, x) : armarTuplas n xs
