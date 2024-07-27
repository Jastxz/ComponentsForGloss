{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tipos.TipoEtiqueta
  ( Etiqueta (..),
    DatosEtiqueta (..),
    construyeEtiqueta,
    construyeEtiquetas,
    dibujaEtiqueta,
    metadatosEtiquetaVacia,
    pulsaElemento,
  )
where

import Graphics.Gloss (Picture, color, scale, text, translate)
import Tipos.TipoElemento
import Tipos.TipoPosicion (Alineamiento, Esquinas, alineamientoCentrado, alineamientoInicio, moduloVector)

{- Etiqueta:
    datosElemento: Datos internos del elemento
    texto: Texto actual del elemento
    porcentajesTexto: Medidas para situar el texto dentro del botón en X e Y. Hay que tener en cuenta que el texto empieza en la posición 1 de las esquinas
    escalasTexto: Tamaño del texto
    alineacion: A escoger entre al principio ('Inicio'), centrado ('Centrado') o al final ('Final')
    nombreElementoAsociado: Nombre del elemento al que está asociado esta etiqueta-}
data Etiqueta = Etiqueta
  { datosElemento :: Elemento,
    texto :: String,
    porcentajesTexto :: (Float, Float),
    escalasTexto :: (Float, Float),
    alineacion :: Alineamiento,
    nombreElementoAsociado :: String
  }
  deriving (Show)

data DatosEtiqueta = DatosEtiqueta
  { metadatosElemento :: MetaDatos,
    datosTexto :: String,
    datosPorcentajesTexto :: (Float, Float),
    datosEscalasTexto :: (Float, Float),
    datosAlineacion :: Alineamiento,
    datosNombreElementoAsociado :: String
  }
  deriving (Show)

construyeEtiqueta :: ListaIdentificadores -> DatosEtiqueta -> (ListaIdentificadores, Etiqueta)
construyeEtiqueta ids metadatos =
  ( idsActualizados,
    Etiqueta
      { datosElemento = datosElemento,
        texto = datosTexto metadatos,
        porcentajesTexto = datosPorcentajesTexto metadatos,
        escalasTexto = datosEscalasTexto metadatos,
        alineacion = datosAlineacion metadatos,
        nombreElementoAsociado = datosNombreElementoAsociado metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeEtiquetas :: ListaIdentificadores -> [DatosEtiqueta] -> (ListaIdentificadores, [Etiqueta])
construyeEtiquetas ids [] = (ids, [])
construyeEtiquetas ids (d : datosEtiquetas) = (idsTerminados, etiqueta : etiquetas)
  where
    (idsActualizados, etiqueta) = construyeEtiqueta ids d
    (idsTerminados, etiquetas) = construyeEtiquetas idsActualizados datosEtiquetas

dibujaEtiqueta :: Esquinas -> Etiqueta -> IO Picture
dibujaEtiqueta esquinasReferido etiqueta = do
  let datos = datosElemento etiqueta
  let alineamiento = alineacion etiqueta
  let (escalaX, escalaY) = escalasTexto etiqueta
  let espacio@(p1, p2, p3, p4) = esquinasReferido
  let moduloX = moduloVector p1 p2
  let moduloY = moduloVector p1 p3
  let puntoTexto
        | alineamiento == alineamientoInicio = (fst p1, snd p1 + 0.1 * moduloY)
        | alineamiento == alineamientoCentrado = (fst p1 + 0.5 * moduloX, snd p1 + 0.1 * moduloY)
        | otherwise = (fst p1 + moduloX, snd p1 + 0.1 * moduloY)
  let colTexto | pulsado datos = colorTextoPulsado datos | otherwise = colorTexto datos
  let textoTerminado = uncurry translate puntoTexto $ color colTexto $ scale escalaX escalaY $ text $ texto etiqueta
  return textoTerminado

metadatosEtiquetaVacia :: DatosEtiqueta
metadatosEtiquetaVacia =
  DatosEtiqueta
    { metadatosElemento = metaDatosElementoVacioFondoTransparente,
      datosTexto = "",
      datosPorcentajesTexto = (0.3, 0.5),
      datosEscalasTexto = (0.1, 0.1),
      datosAlineacion = alineamientoInicio,
      datosNombreElementoAsociado = "1"
    }

pulsaElemento :: String -> Etiqueta -> Etiqueta
pulsaElemento idElemento tipo
  | idElemento == idTipo = tipo {datosElemento = datosPulsados}
  | otherwise = tipo {datosElemento = datosNoPulsados}
  where
    datosTipo = datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo