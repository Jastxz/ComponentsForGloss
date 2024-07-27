module PruebasEntrada
  ( testAlmacen,
  )
where

import AlmacenElementos as Al
import DatosElementosPreconstruidos as Dep
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, MouseButton, SpecialKey), KeyState (Up), MouseButton (LeftButton), playIO)
import Graphics.Gloss.Interface.Pure.Display (Display (InWindow), Picture, white)
import Tipos.TipoBase as B
import Utilidades.Constantes (posicionVentana, tamañoVentana)

-- Definiciones básicas

ventana :: Display
ventana = InWindow "Automata" tamañoVentana posicionVentana

almacenConstruido :: ([String], Al.AlmacenElementos)
almacenConstruido = Al.construyeAlmacen [] almacenInput

almacen :: Al.AlmacenElementos
almacen = snd almacenConstruido

base :: B.Base
base = extraeRelaciones almacen

-- Definiciones para las entradas

manejaEntrada :: Event -> AlmacenElementos -> IO AlmacenElementos
manejaEntrada evento mundo
  | EventKey (MouseButton LeftButton) Up _ raton <- evento = gestionaPulsacionElemento mundo raton
  | EventKey (Char tecla) Up _ _ <- evento = gestionaTeclaChar tecla mundo
  | EventKey (SpecialKey tecla) Up _ _ <- evento = gestionaTeclaEspecial tecla mundo
  | otherwise = return mundo

actualiza :: Float -> AlmacenElementos -> IO AlmacenElementos
actualiza _ mundo = gestionaActualizacionesPasivasElementos mundo

-- Tests

dibujaAlmacenAux :: Al.AlmacenElementos -> IO Picture
dibujaAlmacenAux almacenEl = dibujaAlmacen almacenEl base

testAlmacen :: IO ()
testAlmacen = do
  playIO ventana white 15 almacen dibujaAlmacenAux manejaEntrada actualiza