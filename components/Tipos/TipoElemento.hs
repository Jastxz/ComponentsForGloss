{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tipos.TipoElemento
  ( Elemento (..),
    MetaDatos (..),
    ListaIdentificadores,
    construyeElemento,
    metaDatosElementoVacioFondoBlanco,
    metaDatosElementoVacioFondoNegro,
    metaDatosElementoVacioFondoTransparente,
  )
where

import Graphics.Gloss
import Tipos.TipoPosicion (Esquinas)
import Utilidades.Colores (transparente)

{- Elemento:
    identificador: Id específico del elemento
    nombre: Identificador adicional del elemento para facilitar su enlace con otros
    idPadre: Identificador del padre
    idHijos: Lista de identificadores de los hijos
    esquinas: Posiciones de las esquinas acotadas de la base en concreto con el siguiente orden -> (1,2,3,4) =>
        1 ---- 2
        |      |
        3 ---- 4
    pulsado: Bandera que puede usarse para saber si el elemento ha sido pulsado o lo está siendo
    colorFondo: Color del fondo del elemento
    colorFondoPulsado: Color del fondo del elemento cuando pulsado es verdadero
    colorTexto: Color del texto del elemento
    colorTextoPulsado: Color del texto del elemento cuando pulsado es verdadero-}
data Elemento = Elemento
  { identificador :: String,
    nombre :: String,
    idPadre :: String,
    idsHijos :: [String],
    esquinas :: Esquinas,
    pulsado :: Bool,
    colorFondo :: Color,
    colorFondoPulsado :: Color,
    colorTexto :: Color,
    colorTextoPulsado :: Color
  }
  deriving (Show)

data MetaDatos = MetaDatos
  { datosNombre :: String,
    datosIdPadre :: String,
    datosIdHijos :: [String],
    datosEsquinas :: Esquinas,
    datosPulsado :: Bool,
    datosColorFondo :: Color,
    datosColorFondoPulsado :: Color,
    datosColorTexto :: Color,
    datosColorTextoPulsado :: Color
  }
  deriving (Show)

type ListaIdentificadores = [String]

construyeElemento :: ListaIdentificadores -> MetaDatos -> (ListaIdentificadores, Elemento)
construyeElemento ids metaDatos =
  ( id : ids,
    Elemento
      { identificador = id,
        nombre = datosNombre metaDatos,
        idPadre = datosIdPadre metaDatos,
        idsHijos = datosIdHijos metaDatos,
        esquinas = datosEsquinas metaDatos,
        pulsado = datosPulsado metaDatos,
        colorFondo = datosColorFondo metaDatos,
        colorFondoPulsado = datosColorFondoPulsado metaDatos,
        colorTexto = datosColorTexto metaDatos,
        colorTextoPulsado = datosColorTextoPulsado metaDatos
      }
  )
  where
    id = show $ length ids

metaDatosElementoVacioFondoBlanco :: MetaDatos
metaDatosElementoVacioFondoBlanco =
  MetaDatos
    { datosNombre = "",
      datosIdPadre = "",
      datosIdHijos = [""],
      datosEsquinas = ((0, 0), (0, 0), (0, 0), (0, 0)),
      datosPulsado = False,
      datosColorFondo = white,
      datosColorFondoPulsado = black,
      datosColorTexto = black,
      datosColorTextoPulsado = white
    }

metaDatosElementoVacioFondoNegro :: MetaDatos
metaDatosElementoVacioFondoNegro =
  MetaDatos
    { datosNombre = "",
      datosIdPadre = "",
      datosIdHijos = [""],
      datosEsquinas = ((0, 0), (0, 0), (0, 0), (0, 0)),
      datosPulsado = False,
      datosColorFondo = black,
      datosColorFondoPulsado = white,
      datosColorTexto = white,
      datosColorTextoPulsado = black
    }

metaDatosElementoVacioFondoTransparente :: MetaDatos
metaDatosElementoVacioFondoTransparente =
  MetaDatos
    { datosNombre = "",
      datosIdPadre = "",
      datosIdHijos = [""],
      datosEsquinas = ((0, 0), (0, 0), (0, 0), (0, 0)),
      datosPulsado = False,
      datosColorFondo = transparente,
      datosColorFondoPulsado = white,
      datosColorTexto = black,
      datosColorTextoPulsado = magenta
    }
