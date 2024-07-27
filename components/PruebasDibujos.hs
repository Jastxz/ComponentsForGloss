module PruebasDibujos
  ( testElemento,
    testBase,
    testBarraBasica,
    testBarraBasicaPulsada,
    testBarra1Boton,
    testBotonBasico,
    testBotonPulsado,
    testEntradaBasica,
    testEntradaTransparente,
    testEtiqueta,
    testLista,
    testParrafo,
    testSaltoDeLineaBasico,
    testSaltoDeLineaVertical,
    testTituloBasico,
    testTituloBasico2,
    testTituloBasico3,
    testTituloBasico4,
    testTituloBasico5,
    testTituloBasico6,
    testTituloBasico7,
  )
where

import AlmacenElementos as Al
import DatosElementosPreconstruidos as Dep
import Graphics.Gloss.Interface.Pure.Display (Display (InWindow), blue, display, white)
import Tipos.TipoBarraNavegacion as Barra
import Tipos.TipoBase as B
import Tipos.TipoElemento as E
import Tipos.TipoEntrada as E
import Tipos.TipoEtiqueta as Et
import Tipos.TipoForma as F
import Tipos.TipoLista as L
import Tipos.TipoParrafo as P
import Tipos.TipoPosicion (Malla)
import Tipos.TipoSaltoDeLinea as S
import Tipos.TipoTitulo as T
import Utilidades.Constantes (posicionVentana, tamañoVentana)
import Utilidades.Utiles (cabeza)

-- Definiciones básicas

ventana :: Display
ventana = InWindow "Automata" tamañoVentana posicionVentana

almacen :: Al.AlmacenElementos
almacen = snd $ Al.construyeAlmacen [] almacenBasico

mallaBasica :: Malla
mallaBasica = B.creaMallaBase $ cabeza "mallaBasica" $ Al.bases almacen

elemento :: E.Elemento
elemento = snd $ E.construyeElemento [] Dep.elementoBasico

base :: B.Base
base = cabeza "base" $ Al.bases almacen

barra :: Barra.BarraNavegacion
barra = cabeza "barra" $ Al.barrasNavegacion almacen

boton :: F.Forma
boton = cabeza "boton" $ Al.formas almacen

entrada :: E.Entrada
entrada = cabeza "entrada" $ Al.entradas almacen

etiqueta :: Et.Etiqueta
etiqueta = cabeza "etiqueta" $ Al.etiquetas almacen

lista :: L.Lista
lista = cabeza "lista" $ Al.listas almacen

parrafo :: P.Parrafo
parrafo = cabeza "parrafo" $ Al.parrafos almacen

saltoDeLinea :: S.SaltoDeLinea
saltoDeLinea = cabeza "saltoDeLinea" $ Al.saltosDeLinea almacen

titulo :: T.Titulo
titulo = cabeza "titulo" $ Al.titulos almacen

-- Tests

-- +++++++++++++ Elemento +++++++++++++++++++++++++++++++
testElemento :: IO ()
testElemento = do
  print $ "elementoBasico: " ++ show elemento

-- +++++++++++++ Base +++++++++++++++++++++++++++++++
testBase :: IO ()
testBase = do
  print $ "baseBasica: " ++ show base
  print "\nMalla basica de la baseBasica:"
  print mallaBasica

-- +++++++++++++ Barra +++++++++++++++++++++++++++++++
testBarraBasica :: IO ()
testBarraBasica = do
  barraDibujada <- Barra.dibujaBarraNavegacion barra
  print $ "\nBarra basica: " ++ show barra
  display ventana white barraDibujada

testBarraBasicaPulsada :: IO ()
testBarraBasicaPulsada = do
  barraDibujada <- Barra.dibujaBarraNavegacion $ barra {Barra.datosElemento = elemento {E.pulsado = True}}
  print $ "\nBarra basica: " ++ show barra
  display ventana white barraDibujada

testBarra1Boton :: IO ()
testBarra1Boton = do
  barraDibujada <- Barra.dibujaBarraNavegacion $ barra {Barra.formas = [PruebasDibujos.boton]}
  print $ "\nBarra con un boton: " ++ show barra {Barra.formas = [PruebasDibujos.boton]}
  display ventana white barraDibujada

-- +++++++++++++ Botón +++++++++++++++++++++++++++++++
testBotonBasico :: IO ()
testBotonBasico = do
  botonDibujado <- F.dibujaForma [] PruebasDibujos.boton
  print $ "Boton basico: " ++ show PruebasDibujos.boton
  display ventana white botonDibujado

testBotonPulsado :: IO ()
testBotonPulsado = do
  botonDibujado <- F.dibujaForma [] PruebasDibujos.boton
  print $ "Boton basico: " ++ show PruebasDibujos.boton {F.datosElemento = elemento {E.pulsado = True}}
  display ventana white botonDibujado

-- +++++++++++++ Entrada +++++++++++++++++++++++++++++++
testEntradaBasica :: IO ()
testEntradaBasica = do
  print $ "entradaBasica: " ++ show entrada
  entradaDibujada <- E.dibujaEntrada entrada
  print "\nEntrada basica:"
  display ventana white entradaDibujada

testEntradaTransparente :: IO ()
testEntradaTransparente = do
  print $ "entradaTransparente: " ++ show entrada {E.fondoTransparente = True}
  entradaDibujada <- E.dibujaEntrada $ entrada {E.fondoTransparente = True}
  print "\nEntrada transparente:"
  display ventana blue entradaDibujada

-- +++++++++++++ Etiqueta +++++++++++++++++++++++++++++++
testEtiqueta :: IO ()
testEtiqueta = do
  let esquinasPrueba = esquinas $ Et.datosElemento etiqueta
  print $ "etiquetaBasica: " ++ show etiqueta
  etiquetaDibujada <- Et.dibujaEtiqueta esquinasPrueba etiqueta
  display ventana white etiquetaDibujada

-- +++++++++++++ Lista +++++++++++++++++++++++++++++++
testLista :: IO ()
testLista = do
  print $ "listaBasica: " ++ show lista
  listaDibujada <- L.dibujaLista lista
  display ventana white listaDibujada

-- +++++++++++++ Párrafos +++++++++++++++++++++++++++++++
testParrafo :: IO ()
testParrafo = do
  print $ "parrafoBasico: " ++ show parrafo
  parrafoDibujado <- P.dibujaParrafo parrafo
  display ventana white parrafoDibujado

-- +++++++++++++ Saltos de línea +++++++++++++++++++++++++++++++
testSaltoDeLineaBasico :: IO ()
testSaltoDeLineaBasico = do
  print $ "saltoDeLineaBasico: " ++ show saltoDeLinea
  saltoDeLineaDibujado <- S.dibujaSaltoDeLinea saltoDeLinea
  display ventana white saltoDeLineaDibujado

testSaltoDeLineaVertical :: IO ()
testSaltoDeLineaVertical = do
  print $ "saltoDeLineaVertical: " ++ show saltoDeLinea {S.vertical = True}
  saltoDeLineaDibujado <- S.dibujaSaltoDeLinea $ saltoDeLinea {S.vertical = True}
  display ventana white saltoDeLineaDibujado

-- +++++++++++++ Títulos +++++++++++++++++++++++++++++++
testTituloBasico :: IO ()
testTituloBasico = do
  print $ "tituloBasico: " ++ show titulo
  tituloDibujado <- T.dibujaTitulo titulo
  display ventana white tituloDibujado

testTituloBasico2 :: IO ()
testTituloBasico2 = do
  print $ "tituloBasico2: " ++ show titulo {T.tamañoTitulo = 2, T.texto = "Texto 2"}
  tituloDibujado <- T.dibujaTitulo $ titulo {T.tamañoTitulo = 2, T.texto = "Texto 2"}
  display ventana white tituloDibujado

testTituloBasico3 :: IO ()
testTituloBasico3 = do
  print $ "tituloBasico3: " ++ show titulo {T.tamañoTitulo = 3, T.texto = "Texto 3"}
  tituloDibujado <- T.dibujaTitulo $ titulo {T.tamañoTitulo = 3, T.texto = "Texto 3"}
  display ventana white tituloDibujado

testTituloBasico4 :: IO ()
testTituloBasico4 = do
  print $ "tituloBasico4: " ++ show titulo {T.tamañoTitulo = 4, T.texto = "Texto 4"}
  tituloDibujado <- T.dibujaTitulo $ titulo {T.tamañoTitulo = 4, T.texto = "Texto 4"}
  display ventana white tituloDibujado

testTituloBasico5 :: IO ()
testTituloBasico5 = do
  print $ "tituloBasico5: " ++ show titulo {T.tamañoTitulo = 5, T.texto = "Texto 5"}
  tituloDibujado <- T.dibujaTitulo $ titulo {T.tamañoTitulo = 5, T.texto = "Texto 5"}
  display ventana white tituloDibujado

testTituloBasico6 :: IO ()
testTituloBasico6 = do
  print $ "tituloBasico6: " ++ show titulo {T.tamañoTitulo = 6, T.texto = "Texto 6"}
  tituloDibujado <- T.dibujaTitulo $ titulo {T.tamañoTitulo = 6, T.texto = "Texto 6"}
  display ventana white tituloDibujado

testTituloBasico7 :: IO ()
testTituloBasico7 = do
  print $ "tituloBasico7: " ++ show titulo {T.tamañoTitulo = 7, T.texto = "Texto 7"}
  tituloDibujado <- T.dibujaTitulo $ titulo {T.tamañoTitulo = 7, T.texto = "Texto 7"}
  display ventana white tituloDibujado
