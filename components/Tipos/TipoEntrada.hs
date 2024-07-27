{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tipos.TipoEntrada
  ( Entrada (..),
    DatosEntrada (..),
    construyeEntrada,
    construyeEntradas,
    dibujaEntrada,
    metadatosEntradaVacia,
    pulsaElemento,
    actualizaEntrada,
    pulsaTeclaChar,
    pulsaTeclaEspecial,
  )
where

import GHC.Float (int2Float)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (SpecialKey (KeyBackspace, KeyDelete, KeyEnter, KeySpace, KeyTab))
import Tipos.TipoElemento
import Tipos.TipoForma (bordeRectangulo)
import Tipos.TipoPosicion (calculaPuntoSegunPosiciones, moduloVector)
import Utilidades.Colores (transparente)

{- Entrada:
    datosElemento: Datos internos del elemento
    texto: Texto actual del elemento
    fondoTransparente: Bandera para saber si el elemento tiene fondo sólido o transparente
    altura: Altura del cuadro del elemento
    anchura: Anchura del cuadro del elemento
    porcentajesTexto: Medidas para situar el texto dentro del botón en X e Y. Hay que tener en cuenta que el texto empieza en la posición 1 de las esquinas
    escalasTexto: Tamaño del texto
    banderaRaya: Bandera para pintar o no la ralla que cambia según el estado anterior-}
data Entrada = Entrada
  { datosElemento :: Elemento,
    texto :: String,
    fondoTransparente :: Bool,
    altura :: Float,
    anchura :: Float,
    porcentajesTexto :: (Float, Float),
    escalasTexto :: (Float, Float),
    banderaRaya :: Bool
  }
  deriving (Show)

data DatosEntrada = DatosEntrada
  { metadatosElemento :: MetaDatos,
    datosTexto :: String,
    datosFondoTransparente :: Bool,
    datosAltura :: Float,
    datosAnchura :: Float,
    datosPorcentajesTexto :: (Float, Float),
    datosEscalasTexto :: (Float, Float),
    datosBanderaRaya :: Bool
  }
  deriving (Show)

construyeEntrada :: ListaIdentificadores -> DatosEntrada -> (ListaIdentificadores, Entrada)
construyeEntrada ids metadatos =
  ( idsActualizados,
    Entrada
      { datosElemento = datosElemento,
        texto = datosTexto metadatos,
        fondoTransparente = datosFondoTransparente metadatos,
        altura = datosAltura metadatos,
        anchura = datosAnchura metadatos,
        porcentajesTexto = datosPorcentajesTexto metadatos,
        escalasTexto = datosEscalasTexto metadatos,
        banderaRaya = datosBanderaRaya metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeEntradas :: ListaIdentificadores -> [DatosEntrada] -> (ListaIdentificadores, [Entrada])
construyeEntradas ids [] = (ids, [])
construyeEntradas ids (d : datosEntradas) = (idsTerminados, entrada : entradas)
  where
    (idsActualizados, entrada) = construyeEntrada ids d
    (idsTerminados, entradas) = construyeEntradas idsActualizados datosEntradas

dibujaEntrada :: Entrada -> IO Picture
dibujaEntrada entrada = do
  let datos = datosElemento entrada
  let (escalaX, escalaY) = escalasTexto entrada
  let espacio@(p1, p2, p3, p4) = esquinas datos
  let moduloX = moduloVector p1 p2
  let moduloY = moduloVector p1 p3
  let alturaFinal | altura entrada > moduloY = moduloY | otherwise = altura entrada
  let anchuraFinal | anchura entrada > moduloX = moduloX | otherwise = anchura entrada
  let puntoCentral = calculaPuntoSegunPosiciones anchuraFinal alturaFinal p1 (0.5, 0.5)
  let porcTexto@(xP, yP) = porcentajesTexto entrada
  let puntoTexto = calculaPuntoSegunPosiciones anchuraFinal alturaFinal p1 porcTexto
  -- En este caso parece que cada carácter puede medir 6.5 píxeles
  let desfaseRaya = int2Float (length (texto entrada)) * 6.5
  let puntoSuperiorInicialTexto@(xSI, ySI) = calculaPuntoSegunPosiciones anchuraFinal alturaFinal p1 (xP, 0.2)
  let puntoInferiorInicialTexto@(xII, yII) = calculaPuntoSegunPosiciones anchuraFinal alturaFinal p1 (xP, 0.8)
  let formaDibujada = rectangleSolid anchuraFinal alturaFinal
  let bordeDibujado = bordeRectangulo 0.5 anchuraFinal alturaFinal
  let colFondo | fondoTransparente entrada = transparente | otherwise = colorFondo datos
  let colTexto = colorTexto datos
  let formaTerminada = uncurry translate puntoCentral $ color colFondo formaDibujada
  let bordeTerminado = uncurry translate puntoCentral $ color colTexto bordeDibujado
  let textoTerminado = uncurry translate puntoTexto $ color colTexto $ scale escalaX escalaY $ text $ texto entrada
  let dibujoRaya = line [(xSI + desfaseRaya, ySI), (xII + desfaseRaya, yII)]
  let estadoRaya = if banderaRaya entrada && pulsado datos then color colTexto dibujoRaya else color colFondo dibujoRaya
  return $ pictures [formaTerminada, bordeTerminado, textoTerminado, estadoRaya]

metadatosEntradaVacia :: DatosEntrada
metadatosEntradaVacia =
  DatosEntrada
    { metadatosElemento = metaDatosElementoVacioFondoBlanco,
      datosTexto = "",
      datosFondoTransparente = False,
      datosAltura = 50,
      datosAnchura = 200,
      datosPorcentajesTexto = (0.3, 0.5),
      datosEscalasTexto = (0.1, 0.1),
      datosBanderaRaya = True
    }

pulsaElemento :: String -> Entrada -> Entrada
pulsaElemento idElemento tipo
  | idElemento == idTipo = tipo {datosElemento = datosPulsados}
  | otherwise = tipo {datosElemento = datosNoPulsados}
  where
    datosTipo = datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo

actualizaEntrada :: Entrada -> Entrada
actualizaEntrada entrada
  | pulsado datos = entrada {banderaRaya = not (banderaRaya entrada)}
  | otherwise = entrada
  where
    datos = datosElemento entrada

pulsaTeclaChar :: Char -> Entrada -> Entrada
pulsaTeclaChar tecla entrada
  | pulsado (datosElemento entrada) = entradaActualizada
  | otherwise = entrada
  where
    datosTipo = datosElemento entrada
    datosNoPulsados = datosTipo {pulsado = False}
    entradaActualizada = case tecla of
      '\b' -> if null (texto entrada) then entrada else entrada {texto = init (texto entrada)}
      _ -> entrada {texto = (texto entrada) ++ [tecla]}

pulsaTeclaEspecial :: SpecialKey -> Entrada -> Entrada
pulsaTeclaEspecial tecla entrada
  | pulsado (datosElemento entrada) = entradaActualizada
  | otherwise = entrada
  where
    datosTipo = datosElemento entrada
    datosNoPulsados = datosTipo {pulsado = False}
    entradaActualizada = case tecla of
      KeySpace -> entrada {texto = (texto entrada) ++ " "}
      KeyBackspace -> if null (texto entrada) then entrada else entrada {texto = init (texto entrada)}
      KeyTab -> entrada {datosElemento = datosNoPulsados}
      KeyEnter -> entrada {datosElemento = datosNoPulsados}
      KeyDelete -> if null (texto entrada) then entrada else entrada {texto = tail (texto entrada)}
      _ -> entrada