{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tipos.TipoSaltoDeLinea
  ( SaltoDeLinea (..),
    DatosSaltoDeLinea (..),
    construyeSaltoDeLinea,
    construyeSaltosDeLinea,
    dibujaSaltoDeLinea,
    metadatosSaltoDeLineaVacio,
    pulsaElemento,
  )
where

import GHC.Float (int2Float)
import Graphics.Gloss (Picture, color, rectangleSolid, translate)
import Tipos.TipoElemento (Elemento (colorFondo, colorFondoPulsado, esquinas, identificador, pulsado), ListaIdentificadores, MetaDatos, construyeElemento, metaDatosElementoVacioFondoBlanco)
import Tipos.TipoPosicion (calculaPuntoSegunPosiciones, moduloVector)

{- SaltoDeLinea:
    datosElemento: Datos internos del elemento
    numeroDeLineas: Número de líneas que añade en blanco
    tamañoEnPixeles: Tamaño en píxeles de la línea
    vertical: Especificación para saber si el salto de línea debe extenderse horizontal ó verticalmente-}
data SaltoDeLinea = SaltoDeLinea
  { datosElemento :: Elemento,
    numeroDeLineas :: Int,
    tamañoEnPixeles :: Int,
    vertical :: Bool
  }
  deriving (Show)

data DatosSaltoDeLinea = DatosSaltoDeLinea
  { metadatosElemento :: MetaDatos,
    datosNumeroDeLineas :: Int,
    datosTamañoEnPixeles :: Int,
    datosVertical :: Bool
  }
  deriving (Show)

construyeSaltoDeLinea :: ListaIdentificadores -> DatosSaltoDeLinea -> (ListaIdentificadores, SaltoDeLinea)
construyeSaltoDeLinea ids metadatos =
  ( idsActualizados,
    SaltoDeLinea
      { datosElemento = datosElemento,
        numeroDeLineas = datosNumeroDeLineas metadatos,
        tamañoEnPixeles = datosTamañoEnPixeles metadatos,
        vertical = datosVertical metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeSaltosDeLinea :: ListaIdentificadores -> [DatosSaltoDeLinea] -> (ListaIdentificadores, [SaltoDeLinea])
construyeSaltosDeLinea ids [] = (ids, [])
construyeSaltosDeLinea ids (d : datosSaltosDeLinea) = (idsTerminados, salto : saltos)
  where
    (idsActualizados, salto) = construyeSaltoDeLinea ids d
    (idsTerminados, saltos) = construyeSaltosDeLinea idsActualizados datosSaltosDeLinea

dibujaSaltoDeLinea :: SaltoDeLinea -> IO Picture
dibujaSaltoDeLinea saltoDeLinea = do
  let datos = datosElemento saltoDeLinea
  let tamañoFinal = int2Float $ numeroDeLineas saltoDeLinea * tamañoEnPixeles saltoDeLinea
  let espacio@(p1, p2, p3, p4) = esquinas datos
  let colorLineas | pulsado datos = colorFondoPulsado datos | otherwise = colorFondo datos
  let moduloX = moduloVector p1 p2
  let moduloY = moduloVector p1 p3
  let alturaFinal | not (vertical saltoDeLinea) && tamañoFinal <= moduloY = tamañoFinal | otherwise = moduloY
  let anchuraFinal | vertical saltoDeLinea && tamañoFinal <= moduloX = tamañoFinal | otherwise = moduloX
  let puntoCentral = calculaPuntoSegunPosiciones moduloX moduloY p1 (0.5, 0.5)
  let saltoTerminado = uncurry translate puntoCentral $ color colorLineas $ rectangleSolid anchuraFinal alturaFinal
  return saltoTerminado

metadatosSaltoDeLineaVacio :: DatosSaltoDeLinea
metadatosSaltoDeLineaVacio =
  DatosSaltoDeLinea
    { metadatosElemento = metaDatosElementoVacioFondoBlanco,
      datosNumeroDeLineas = 1,
      datosTamañoEnPixeles = 10,
      datosVertical = False
    }

pulsaElemento :: String -> SaltoDeLinea -> SaltoDeLinea
pulsaElemento idElemento tipo
  | idElemento == idTipo = tipo {datosElemento = datosPulsados}
  | otherwise = tipo {datosElemento = datosNoPulsados}
  where
    datosTipo = datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo