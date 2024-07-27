module Utilidades.Utiles
  ( indiceLista,
    cabeza,
    posicion,
    binario,
    esInt,
    esReal,
    stringToInt,
    stringToDouble,
    stringToFloat,
    listasDePares,
    distanciaEuclidea,
    intercambia,
    introduce,
    elimina,
    eliminaElemento,
    agruparListaPorTamaño,
    normaliza,
    listaIOs2IOlista,
  )
where

import Data.Char

indiceLista :: [a] -> Int -> String -> a
indiceLista lista indice funcion
  | indice < 0 || indice >= longitud = error ("Indice fuera de los límites en una lista. Funcion: " ++ funcion ++ " Indice: " ++ show indice)
  | otherwise = lista !! indice
  where
    longitud = length lista

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones normales en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

cabeza :: String -> [a] -> a
cabeza funcion lista
  | null lista = error $ "La lista pasada en la funcion " ++ funcion ++ " esta vacia."
  | otherwise = head lista

posicion :: String -> Int -> [a] -> a
posicion funcion pos lista = indiceLista lista pos funcion

binario :: Int -> Int
binario n
  | odd n = 1
  | otherwise = 0

esInt :: String -> Bool
esInt = foldr ((&&) . isDigit) True

esReal :: String -> Bool
esReal s
  | esInt entera && esInt fraccionaria = True
  | otherwise = False
  where
    entera = takeWhile (/= '.') s
    f = dropWhile (/= '.') s
    fraccionaria = if null f then "" else tail f

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt c@(s : ss)
  | s == '-' = (-1) * stringToInt ss
  | otherwise = (digitToInt s * 10 ^ (length c - 1)) + stringToInt ss

stringToDouble :: String -> Double
stringToDouble [] = 0.0
stringToDouble s = entera + fraccionaria
  where
    e = stringToInt $ takeWhile (/= '.') s
    entera = fromInteger $ toInteger e
    f = tail $ dropWhile (/= '.') s
    fracEnt = fromInteger $ toInteger $ stringToInt f
    fraccionaria = fracEnt / (10 ^ length f)

stringToFloat :: String -> Float
stringToFloat [] = 0.0
stringToFloat s = entera + fraccionaria
  where
    e = stringToInt $ takeWhile (/= '.') s
    entera = fromInteger $ toInteger e
    f = tail $ dropWhile (/= '.') s
    fracEnt = fromInteger $ toInteger $ stringToInt f
    fraccionaria = fracEnt / (10 ^ length f)

listasDePares :: [a] -> [[a]]
listasDePares [] = []
listasDePares lista = par : listasDePares resto
  where
    par = take 2 lista
    resto = drop 2 lista

distanciaEuclidea :: Float -> Float -> Float
distanciaEuclidea a b = sqrt $ (a - b) ** 2

intercambia :: [a] -> a -> Int -> [a]
intercambia [] a _ = [a]
intercambia (x : xs) a p
  | p == 0 = a : xs
  | null xs = [a]
  | otherwise = x : intercambia xs a (p - 1)

introduce :: [a] -> a -> Int -> [a]
introduce (x : xs) a p
  | p == 0 = x : a : xs
  | null xs = x : [a]
  | otherwise = x : introduce xs a (p - 1)

elimina :: [a] -> Int -> [a]
elimina (x : xs) p
  | p == 0 = xs
  | null xs = [x]
  | otherwise = x : elimina xs (p - 1)

eliminaElemento :: (Eq a) => [a] -> a -> [a]
eliminaElemento (x : xs) a
  | x == a = xs
  | null xs = [x]
  | otherwise = x : eliminaElemento xs a

agruparListaPorTamaño :: [Int] -> Int -> [[Int]]
agruparListaPorTamaño lista tamañoPorFila
  | null resto = [fila]
  | otherwise = fila : agruparListaPorTamaño resto tamañoPorFila
  where
    fila = take tamañoPorFila lista
    resto = drop tamañoPorFila lista

normaliza :: Double -> Double
normaliza d = d - fromIntegral (floor d)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones IO en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

listaIOs2IOlista :: [IO a] -> IO [a]
listaIOs2IOlista [] = do return []
listaIOs2IOlista (x : xs) = do
  elemento <- x
  resto <- listaIOs2IOlista xs
  return $ elemento : resto