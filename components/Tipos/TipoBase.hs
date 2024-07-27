{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tipos.TipoBase
  ( Base (..),
    DatosBase (..),
    creaMallaBase,
    construyeBase,
    construyeBases,
    metadatosBaseVacia,
  )
where

import Tipos.TipoElemento (Elemento (esquinas), ListaIdentificadores, MetaDatos, construyeElemento, metaDatosElementoVacioFondoTransparente)
import Tipos.TipoPosicion (Malla, creaMalla)

{- Base:
    datosElemento: Datos internos del elemento
    idsElementosAlojados: Identificadores de los elementos internos de la base, ordenados-}
data Base = Base
  { datosElemento :: Elemento,
    idsElementosAlojados :: [String]
  }
  deriving (Show)

data DatosBase = DatosBase
  { metadatosElemento :: MetaDatos,
    datosIdsElementosAlojados :: [String]
  }
  deriving (Show)

creaMallaBase :: Base -> Malla
creaMallaBase base = creaMalla $ esquinas $ datosElemento base

construyeBase :: ListaIdentificadores -> DatosBase -> (ListaIdentificadores, Base)
construyeBase ids metadatos =
  ( idsActualizados,
    Base
      { datosElemento = datosElemento,
        idsElementosAlojados = datosIdsElementosAlojados metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeBases :: ListaIdentificadores -> [DatosBase] -> (ListaIdentificadores, [Base])
construyeBases ids [] = (ids, [])
construyeBases ids (d : datosBases) = (idsTerminados, base : bases)
  where
    (idsActualizados, base) = construyeBase ids d
    (idsTerminados, bases) = construyeBases idsActualizados datosBases

metadatosBaseVacia :: DatosBase
metadatosBaseVacia = DatosBase {metadatosElemento = metaDatosElementoVacioFondoTransparente, datosIdsElementosAlojados = [""]}