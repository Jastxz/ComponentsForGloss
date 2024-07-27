{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Tipos.TipoPosicion
  ( Pos,
    PosF,
    Esquinas,
    Malla,
    Alineamiento,
    alineamientoInicio,
    alineamientoCentrado,
    alineamientoFinal,
    parteEsquinas,
    moduloVector,
    moduloVectorEsquinas,
    calculaPuntoSegunPosiciones,
    tamañoVentanaAesquinasDeCuadrado,
    listaAesquinas,
    esquinasAlista,
    primEsquina,
    secEsquina,
    tercEsquina,
    cuartaEsquina,
    creaMalla,
    creaMallaConPosicion,
    pintaPunto,
    pintaEsquinas,
    posicionDentroDeEsquinas,
  )
where

import qualified Data.Bifunctor
import Data.List (zip4)
import GHC.Float (int2Float)
import Graphics.Gloss (Picture, circleSolid, color, pictures, red, translate)
import Utilidades.Utiles (cabeza)

type Pos = (Int, Int)

type PosF = (Float, Float)

type Esquinas = (PosF, PosF, PosF, PosF)

type Malla = [Esquinas]

data Alineamiento = Inicio | Centrado | Final deriving (Show, Eq)

alineamientoInicio :: Alineamiento
alineamientoInicio = Inicio

alineamientoCentrado :: Alineamiento
alineamientoCentrado = Centrado

alineamientoFinal :: Alineamiento
alineamientoFinal = Final

parteEsquinas :: Esquinas -> Int -> PosF
parteEsquinas (a, b, c, d) p
  | p == 1 = a
  | p == 2 = b
  | p == 3 = c
  | p == 4 = d
  | otherwise = error "No existen más de 4 esquinas. Las partes elegibles son: 1, 2, 3, 4."

moduloVector :: PosF -> PosF -> Float
moduloVector (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer)

moduloVectorEsquinas :: Esquinas -> (Float, Float)
moduloVectorEsquinas (a, b, c, _) = (moduloVector a b, moduloVector a c)

calculaPuntoSegunPosiciones :: Float -> Float -> PosF -> PosF -> PosF
calculaPuntoSegunPosiciones moduloX moduloY p1 (porcentajeX, porcentajeY) = Data.Bifunctor.bimap (fst p1 +) (snd p1 -) desfases
  where
    desfases = (moduloX * porcentajeX, moduloY * porcentajeY)

{- Recibe el tamaño de una ventana (o una posición de prueba), el tamaño del cuadrado que forman las esquinas en forma de porcentaje
en relación con el tamaño total de la ventana, y la posición central donde desees poner el cuadrado. Devuelve las esquinas del cuadrado indicado.

Nota: Hay que tener en cuenta que el tamaño de la ventana siempre va a venir en positivo pero que el centrado es según ejes cartesianos. -}
tamañoVentanaAesquinasDeCuadrado :: Pos -> PosF -> PosF -> Esquinas
tamañoVentanaAesquinasDeCuadrado (anchoVentana, altoVentana) (porcentajeAncho, porcentajeAlto) centroEsquinas@(posX, posY) = esquinasCuadrado
  where
    (anchoV, altoV) = (int2Float anchoVentana, int2Float altoVentana)
    (modX, modY) = (anchoV * porcentajeAncho, altoV * porcentajeAlto)
    distanciaAlCentro@(disAncho, disAlto) = (modX / 2, modY / 2)
    e1 = (posX - disAncho, posY + disAlto)
    e2 = (posX + disAncho, posY + disAlto)
    e3 = (posX - disAncho, posY - disAlto)
    e4 = (posX + disAncho, posY - disAlto)
    esquinasCuadrado = (e1, e2, e3, e4)

listaAesquinas :: [PosF] -> Esquinas
listaAesquinas ps
  | length ps < 4 = error "No se puede convertir una lista con menos de 4 elementos a esquinas"
  | otherwise = (cabeza "listaAesquinas" ps, ps !! 1, ps !! 2, ps !! 3)

esquinasAlista :: Esquinas -> [PosF]
esquinasAlista (a, b, c, d) = [a, b, c, d, b]

primEsquina :: Esquinas -> PosF
primEsquina (a, _, _, _) = a

secEsquina :: Esquinas -> PosF
secEsquina (_, a, _, _) = a

tercEsquina :: Esquinas -> PosF
tercEsquina (_, _, a, _) = a

cuartaEsquina :: Esquinas -> PosF
cuartaEsquina (_, _, _, a) = a

creaMalla :: Esquinas -> Malla
creaMalla esqs
  | x1 == x3 && y1 == y2 && x2 == x4 && y3 == y4 = zip4 ps1 ps2 ps3 ps4
  | otherwise = error $ "No se puede crear la malla porque las esquinas no concuerdan. Esquinas: " ++ show esqs
  where
    (p1@(x1, y1), p2@(x2, y2), p3@(x3, y3), (x4, y4)) = esqs
    sumaX = moduloVector p1 p2
    sumaY = moduloVector p1 p3
    ps1 = [(x1 + a * sumaX, y1 - b * sumaY) | b <- [0.0, 0.1 .. 0.9], a <- [0.0, 0.1 .. 0.9]]
    ps2 = [(x2 - a * sumaX, y2 - b * sumaY) | b <- [0.0, 0.1 .. 0.9], a <- [0.9, 0.8 .. 0.0]]
    ps3 = [(x3 + a * sumaX, y3 + b * sumaY) | b <- [0.9, 0.8 .. 0.0], a <- [0.0, 0.1 .. 0.9]]
    ps4 = [(x4 - a * sumaX, y4 + b * sumaY) | b <- [0.9, 0.8 .. 0.0], a <- [0.9, 0.8 .. 0.0]]

creaMallaConPosicion :: Pos -> Malla
creaMallaConPosicion tamVentana = creaMalla $ tamañoVentanaAesquinasDeCuadrado tamVentana (1.0, 1.0) (0.0, 0.0)

pintaPunto :: Float -> PosF -> Picture
pintaPunto radio (x, y) = translate x y $ color red $ circleSolid radio

pintaEsquinas :: Float -> Esquinas -> Picture
pintaEsquinas radio (p1, p2, p3, p4) = pictures [pintaPunto radio p1, pintaPunto radio p2, pintaPunto radio p3, pintaPunto radio p4]

posicionDentroDeEsquinas :: Esquinas -> PosF -> Bool
posicionDentroDeEsquinas (e1, e2, e3, e4) (x, y) = dentroAncho && dentroAlto
  where
    (x1, y1) = e1
    (x2, _) = e2
    (_, y3) = e3
    dentroAncho = x >= x1 && x <= x2
    dentroAlto = y <= y1 && y >= y3