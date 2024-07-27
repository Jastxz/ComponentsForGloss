{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tipos.TipoBarraNavegacion
  ( BarraNavegacion (..),
    DatosBarraNavegacion (..),
    construyeBarraNavegacion,
    construyeBarrasNavegacion,
    dibujaBarraNavegacion,
    metadatosBarraVacia,
    Tipos.TipoBarraNavegacion.pulsaElemento,
  )
where

import GHC.Float (int2Float)
import Graphics.Gloss
import Tipos.TipoElemento
import Tipos.TipoForma as F
import Tipos.TipoPosicion
import Utilidades.Utiles (listaIOs2IOlista)

{- BarraNavegación:
    datosElemento: Datos internos del elemento
    formas: Elementos del tipo de forma asociadas a esta barra
    margenes: Márgen de los botones en el siguiente orden -> Arriba, Derecha, Abajo, Izaquierda-}
data BarraNavegacion = BarraNavegacion
  { datosElemento :: Elemento,
    formas :: [Forma],
    margenes :: (Int, Int, Int, Int)
  }
  deriving (Show)

data DatosBarraNavegacion = DatosBarraNavegacion
  { metadatosElemento :: MetaDatos,
    datosFormas :: [Forma],
    datosMargenes :: (Int, Int, Int, Int)
  }
  deriving (Show)

construyeBarraNavegacion :: ListaIdentificadores -> DatosBarraNavegacion -> (ListaIdentificadores, BarraNavegacion)
construyeBarraNavegacion ids metadatos =
  ( idsActualizados,
    BarraNavegacion
      { Tipos.TipoBarraNavegacion.datosElemento = datosElemento,
        formas = datosFormas metadatos,
        margenes = datosMargenes metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (Tipos.TipoBarraNavegacion.metadatosElemento metadatos)

construyeBarrasNavegacion :: ListaIdentificadores -> [DatosBarraNavegacion] -> (ListaIdentificadores, [BarraNavegacion])
construyeBarrasNavegacion ids [] = (ids, [])
construyeBarrasNavegacion ids (d : datosBarras) = (idsTerminados, barra : barras)
  where
    (idsActualizados, barra) = construyeBarraNavegacion ids d
    (idsTerminados, barras) = construyeBarrasNavegacion idsActualizados datosBarras

dibujaBarraNavegacion :: BarraNavegacion -> IO Picture
dibujaBarraNavegacion barra = do
  let datos = Tipos.TipoBarraNavegacion.datosElemento barra
  let colorBarra | pulsado datos = colorFondoPulsado datos | otherwise = colorFondo datos
  let fondoBarra = color colorBarra $ polygon $ esquinasAlista $ esquinas datos
  let mallaFormas = map (añadeMargenes (margenes barra) . esquinas . F.datosElemento) $ formas barra
  formasDibujados <- listaIOs2IOlista [F.dibujaForma [mallaFormas !! indice] (formas barra !! indice) | indice <- [0 .. length (formas barra) - 1]]
  let fondoBarraConFormas = fondoBarra : formasDibujados
  return $ pictures fondoBarraConFormas

metadatosBarraVacia :: DatosBarraNavegacion
metadatosBarraVacia =
  DatosBarraNavegacion
    { Tipos.TipoBarraNavegacion.metadatosElemento = metaDatosElementoVacioFondoNegro,
      datosFormas = [],
      datosMargenes = (0, 0, 0, 0)
    }

-- Función auxiliar para el dibujado de la barra
añadeMargenes :: (Int, Int, Int, Int) -> Esquinas -> Esquinas
añadeMargenes (arr, der, ab, izq) ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) = (p1, p2, p3, p4)
  where
    arrF = int2Float arr
    derF = int2Float der
    abF = int2Float ab
    izqF = int2Float izq
    p1 = (x1 + izqF, y1 - arrF)
    p2 = (x2 - derF, y2 - arrF)
    p3 = (x3 + izqF, y3 + abF)
    p4 = (x4 - izqF, y4 + abF)

pulsaElemento :: String -> BarraNavegacion -> BarraNavegacion
pulsaElemento idElemento tipo
  | idElemento == idTipo = tipo {Tipos.TipoBarraNavegacion.datosElemento = datosPulsados}
  | otherwise = tipo {Tipos.TipoBarraNavegacion.datosElemento = datosNoPulsados}
  where
    datosTipo = Tipos.TipoBarraNavegacion.datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo