{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tipos.TipoLista
  ( Lista (..),
    DatosLista (..),
    construyeLista,
    construyeListas,
    dibujaLista,
    metadatosListaVacia,
    pulsaElemento,
  )
where

import GHC.Float (int2Float)
import Graphics.Gloss (Picture, color, pictures, scale, text, translate)
import Tipos.TipoElemento (Elemento (colorTexto, colorTextoPulsado, esquinas, identificador, pulsado), ListaIdentificadores, MetaDatos, construyeElemento, metaDatosElementoVacioFondoTransparente)
import Tipos.TipoPosicion (Alineamiento, alineamientoCentrado, alineamientoInicio, moduloVector)

{- Lista:
    datosElemento: Datos internos del elemento
    encabezado: Texto que sirve como título para la lista
    alineacionEncabezado: A escoger entre al principio ('Inicio'), centrado ('Centrado') o al final ('Final')
    escalasEncabezado: Tamaño del texto del encabezado
    textos: Lista con los textos que debe mostrar la lista
    escalasTexto: Tamaño del texto-}
data Lista = Lista
  { datosElemento :: Elemento,
    encabezado :: String,
    alineacionEncabezado :: Alineamiento,
    escalasEncabezado :: (Float, Float),
    textos :: [String],
    escalasTexto :: (Float, Float)
  }
  deriving (Show)

data DatosLista = DatosLista
  { metadatosElemento :: MetaDatos,
    datosEncabezado :: String,
    datosAlineacionEncabezado :: Alineamiento,
    datosEscalaEncabezado :: (Float, Float),
    datosTextos :: [String],
    datosEscalasTexto :: (Float, Float)
  }
  deriving (Show)

construyeLista :: ListaIdentificadores -> DatosLista -> (ListaIdentificadores, Lista)
construyeLista ids metadatos =
  ( idsActualizados,
    Lista
      { datosElemento = datosElemento,
        encabezado = datosEncabezado metadatos,
        alineacionEncabezado = datosAlineacionEncabezado metadatos,
        escalasEncabezado = datosEscalaEncabezado metadatos,
        textos = datosTextos metadatos,
        escalasTexto = datosEscalasTexto metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeListas :: ListaIdentificadores -> [DatosLista] -> (ListaIdentificadores, [Lista])
construyeListas ids [] = (ids, [])
construyeListas ids (d : datosListas) = (idsTerminados, lista : listas)
  where
    (idsActualizados, lista) = construyeLista ids d
    (idsTerminados, listas) = construyeListas idsActualizados datosListas

dibujaLista :: Lista -> IO Picture
dibujaLista lista = do
  let datos = datosElemento lista
  let alineamientoEncabezado = alineacionEncabezado lista
  let (escalaX1, escalaY1) = escalasEncabezado lista
  let (escalaX2, escalaY2) = escalasTexto lista
  let espacio@(p1, p2, p3, p4) = esquinas datos
  let moduloX = moduloVector p1 p2
  let moduloY = moduloVector p1 p3
  let puntoEncabezado
        | alineamientoEncabezado == alineamientoInicio = (fst p1, snd p1 - 0.1 * moduloY)
        | alineamientoEncabezado == alineamientoCentrado = (fst p1 + 0.5 * moduloX, snd p1 - 0.1 * moduloY)
        | otherwise = (fst p1 + moduloX, snd p1 - 0.1 * moduloY)
  let separacionTextos = (moduloY * 0.9) / int2Float (length (textos lista))
  let puntosTextos = [(fst p1 + 0.2 * moduloX, snd p1 - int2Float i * separacionTextos) | i <- [1 .. length (textos lista)]]
  let textosPreparados = map ("- " ++) $ textos lista
  let colTexto | pulsado datos = colorTextoPulsado datos | otherwise = colorTexto datos
  let encabezadoTerminado = uncurry translate puntoEncabezado $ color colTexto $ scale escalaX1 escalaY1 $ text $ encabezado lista
  let textosTerminados = pictures [uncurry translate p (color colTexto (scale escalaX2 escalaY2 (text t))) | (p, t) <- zip puntosTextos textosPreparados]
  return $ pictures [encabezadoTerminado, textosTerminados]

metadatosListaVacia :: DatosLista
metadatosListaVacia =
  DatosLista
    { metadatosElemento = metaDatosElementoVacioFondoTransparente,
      datosEncabezado = "",
      datosAlineacionEncabezado = alineamientoInicio,
      datosEscalaEncabezado = (0.1, 0.1),
      datosTextos = [""],
      datosEscalasTexto = (0.1, 0.1)
    }

pulsaElemento :: String -> Lista -> Lista
pulsaElemento idElemento tipo
  | idElemento == idTipo = tipo {datosElemento = datosPulsados}
  | otherwise = tipo {datosElemento = datosNoPulsados}
  where
    datosTipo = datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo