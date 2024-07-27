{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tipos.TipoTitulo
  ( Titulo (..),
    DatosTitulo (..),
    construyeTitulo,
    construyeTitulos,
    dibujaTitulo,
    metadatosTituloVacio,
    pulsaElemento,
  )
where

import Graphics.Gloss (Picture, color, pictures, rectangleSolid, scale, text, translate)
import Tipos.TipoElemento (Elemento (colorFondo, colorFondoPulsado, colorTexto, colorTextoPulsado, esquinas, identificador, pulsado), ListaIdentificadores, MetaDatos, construyeElemento, metaDatosElementoVacioFondoTransparente)
import Tipos.TipoPosicion (Alineamiento, alineamientoCentrado, alineamientoInicio, calculaPuntoSegunPosiciones, moduloVector)

{- Titulo:
    datosElemento: Datos internos del elemento
    tamañoTitulo: Número que simboliza el tamaño de título elegido entre un posible de 6 distintos, de menor a mayor, más un séptimo por defecto
    texto: Texto actual del elemento
    alineacion: A escoger entre al principio ('Inicio'), centrado ('Centrado') o al final ('Final')-}
data Titulo = Titulo
  { datosElemento :: Elemento,
    tamañoTitulo :: Int,
    texto :: String,
    alineacion :: Alineamiento
  }
  deriving (Show)

data DatosTitulo = DatosTitulo
  { metadatosElemento :: MetaDatos,
    datosTamañoTitulo :: Int,
    datosTexto :: String,
    datosAlineacion :: Alineamiento
  }
  deriving (Show)

construyeTitulo :: ListaIdentificadores -> DatosTitulo -> (ListaIdentificadores, Titulo)
construyeTitulo ids metadatos =
  ( idsActualizados,
    Titulo
      { datosElemento = datosElemento,
        tamañoTitulo = datosTamañoTitulo metadatos,
        texto = datosTexto metadatos,
        alineacion = datosAlineacion metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeTitulos :: ListaIdentificadores -> [DatosTitulo] -> (ListaIdentificadores, [Titulo])
construyeTitulos ids [] = (ids, [])
construyeTitulos ids (d : datosTitulos) = (idsTerminados, titulo : titulos)
  where
    (idsActualizados, titulo) = construyeTitulo ids d
    (idsTerminados, titulos) = construyeTitulos idsActualizados datosTitulos

dibujaTitulo :: Titulo -> IO Picture
dibujaTitulo titulo = do
  let datos = datosElemento titulo
  let alineamiento = alineacion titulo
  let (escalaX, escalaY) = tamañoTituloAescala $ tamañoTitulo titulo
  let espacio@(p1, p2, p3, p4) = esquinas datos
  let moduloX = moduloVector p1 p2
  let moduloY = moduloVector p1 p3
  let puntoCentral = calculaPuntoSegunPosiciones moduloX moduloY p1 (0.5, 0.5)
  let puntoTexto
        | alineamiento == alineamientoInicio = (fst p1, snd p1 - 0.5 * moduloY)
        | alineamiento == alineamientoCentrado = (fst p1 + 0.5 * moduloX, snd p1 - 0.5 * moduloY)
        | otherwise = (fst p1 + moduloX, snd p1 - 0.5 * moduloY)
  let colFondo | pulsado datos = colorFondoPulsado datos | otherwise = colorFondo datos
  let colTexto | pulsado datos = colorTextoPulsado datos | otherwise = colorTexto datos
  let formaTerminada = uncurry translate puntoCentral $ color colFondo $ rectangleSolid moduloX moduloY
  let tituloTerminado = uncurry translate puntoTexto $ color colTexto $ scale escalaX escalaY $ text $ texto titulo
  return $ pictures [formaTerminada, tituloTerminado]

metadatosTituloVacio :: DatosTitulo
metadatosTituloVacio =
  DatosTitulo
    { metadatosElemento = metaDatosElementoVacioFondoTransparente,
      datosTamañoTitulo = 1,
      datosTexto = "",
      datosAlineacion = alineamientoInicio
    }

tamañoTituloAescala :: Int -> (Float, Float)
tamañoTituloAescala tam = case tam of
  1 -> (0.1, 0.1)
  2 -> (0.17, 0.17)
  3 -> (0.24, 0.24)
  4 -> (0.31, 0.31)
  5 -> (0.37, 0.37)
  6 -> (0.44, 0.44)
  _ -> (0.5, 0.5)

pulsaElemento :: String -> Titulo -> Titulo
pulsaElemento idElemento tipo
  | idElemento == idTipo = tipo {datosElemento = datosPulsados}
  | otherwise = tipo {datosElemento = datosNoPulsados}
  where
    datosTipo = datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo