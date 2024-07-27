{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tipos.TipoParrafo
  ( Parrafo (..),
    DatosParrafo (..),
    construyeParrafo,
    construyeParrafos,
    dibujaParrafo,
    metadatosParrafoVacio,
    pulsaElemento,
  )
where

import GHC.Float (int2Float)
import Graphics.Gloss (Picture, color, pictures, scale, text, translate)
import Tipos.TipoElemento (Elemento (colorTexto, colorTextoPulsado, esquinas, identificador, pulsado), ListaIdentificadores, MetaDatos, construyeElemento, metaDatosElementoVacioFondoTransparente)
import Tipos.TipoPosicion (moduloVector)

{- Parrafo:
    datosElemento: Datos internos del elemento
    texto: Texto actual del elemento
    escalasTexto: Tamaño del texto
    margenes: Margen de separación en el comienzo del párrafo respecto de su esquina 1
    encajado: Especificación para saber si no nos importa que el texto se salga por los bordes-}
data Parrafo = Parrafo
  { datosElemento :: Elemento,
    texto :: String,
    escalasTexto :: (Float, Float),
    margenes :: (Float, Float),
    encajado :: Bool
  }
  deriving (Show)

data DatosParrafo = DatosParrafo
  { metadatosElemento :: MetaDatos,
    datosTexto :: String,
    datosEscalasTexto :: (Float, Float),
    datosMargenes :: (Float, Float),
    datosEncajado :: Bool
  }
  deriving (Show)

construyeParrafo :: ListaIdentificadores -> DatosParrafo -> (ListaIdentificadores, Parrafo)
construyeParrafo ids metadatos =
  ( idsActualizados,
    Parrafo
      { datosElemento = datosElemento,
        texto = datosTexto metadatos,
        escalasTexto = datosEscalasTexto metadatos,
        margenes = datosMargenes metadatos,
        encajado = datosEncajado metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeParrafos :: ListaIdentificadores -> [DatosParrafo] -> (ListaIdentificadores, [Parrafo])
construyeParrafos ids [] = (ids, [])
construyeParrafos ids (d : datosParrafos) = (idsTerminados, parrafo : parrafos)
  where
    (idsActualizados, parrafo) = construyeParrafo ids d
    (idsTerminados, parrafos) = construyeParrafos idsActualizados datosParrafos

dibujaParrafo :: Parrafo -> IO Picture
dibujaParrafo parrafo = do
  let datos = datosElemento parrafo
  let (escalaX, escalaY) = escalasTexto parrafo
  let (margenX, margenY) = margenes parrafo
  let espacio@(p1, p2, p3, p4) = esquinas datos
  let moduloX = moduloVector p1 p2
  let moduloY = moduloVector p1 p3
  -- Cada caracter ocupa algo un aproximado de 8 píxeles, por lo que es lo que queda reflejado
  let caracteresPorLinea = round $ (moduloX - 2 * moduloX * margenX) / 8
  let pixelesPorColumna = moduloY - 2 * moduloY * margenY
  let textoPartido = parteTextoSegunModulo caracteresPorLinea $ texto parrafo
  let separacionEntreLineas = pixelesPorColumna / int2Float (length textoPartido)
  let puntoComienzoX = fst p1 + margenX * moduloX
  let puntoComienzoY = snd p1 - margenY * moduloY
  let puntosLineas = [(puntoComienzoX, puntoComienzoY - int2Float multiplicador * separacionEntreLineas) | multiplicador <- [0 .. length textoPartido - 1]]
  let colTexto | pulsado datos = colorTextoPulsado datos | otherwise = colorTexto datos
  let textoColoreado = map (color colTexto . scale escalaX escalaY . text) textoPartido
  let textoTerminado = zipWith (uncurry translate) puntosLineas textoColoreado
  return $ pictures [pictures textoTerminado]

metadatosParrafoVacio :: DatosParrafo
metadatosParrafoVacio =
  DatosParrafo
    { metadatosElemento = metaDatosElementoVacioFondoTransparente,
      datosTexto = "",
      datosEscalasTexto = (0.1, 0.1),
      datosMargenes = (0.1, 0.1),
      datosEncajado = True
    }

parteTextoSegunModulo :: Int -> String -> [String]
parteTextoSegunModulo _ [] = []
parteTextoSegunModulo caracteresPorLinea texto = take caracteresPorLinea texto : parteTextoSegunModulo caracteresPorLinea (drop caracteresPorLinea texto)

pulsaElemento :: String -> Parrafo -> Parrafo
pulsaElemento idElemento tipo
  | idElemento == idTipo = tipo {datosElemento = datosPulsados}
  | otherwise = tipo {datosElemento = datosNoPulsados}
  where
    datosTipo = datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo