{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tipos.TipoForma
  ( Forma (..),
    DatosForma (..),
    boton,
    checkbox,
    radioButton,
    ninguno,
    construyeForma,
    construyeFormas,
    dibujaForma,
    bordeRectangulo,
    metadatosFormaBoton,
    metadatosFormaCheckbox,
    metadatosFormaRadioButton,
    metadatosFormaRectangulo,
    metadatosFormaSemicirculo,
    metadatosFormaCirculo,
    metadatosFormaPoligono,
    pulsaElemento,
  )
where

import Graphics.Gloss
import Tipos.TipoElemento
import Tipos.TipoPosicion (Malla, calculaPuntoSegunPosiciones, moduloVector)
import Utilidades.Colores (transparente)
import Utilidades.Utiles (cabeza)

{- Forma:
    datosElemento: Datos internos del elemento
    texto: Texto asociado a la forma
    porcentajesTexto: Medidas para situar el texto dentro de la forma en X e Y. Hay que tener en cuenta que el texto empieza en la posición 1 de las esquinas
    escalasTexto: Tamaño del texto
    rol: Especifíca el papel que interpretará la forma. A escoger entre boton, checbox, radio button o ninguno
    marcar: Especifica en caso de rol radio button o checkbox, si debe aparecer marcado o relleno
    tipo: A escoger una forma entre círculo, semicírculo, rectángulo, polígono o matriz
    listaDePuntos: Lista de puntos necesarios en caso de referir una forma poligonal
    altura: Altura del cuadro del elemento en caso de referir una forma rectangular
    anchura: Anchura del cuadro del elemento en caso de referir una forma rectangular
    angulos: Ángulos del semicírculo del elemento en caso de referir una forma semicircular
    radio: Radio del círculo en caso de referir una forma círcular o semicircular
    borde: Ancho del borde-}
data Forma = Forma
  { datosElemento :: Elemento,
    texto :: String,
    porcentajesTexto :: (Float, Float),
    escalasTexto :: (Float, Float),
    rol :: String,
    marcar :: Bool,
    tipo :: String,
    listaDePuntos :: Path,
    altura :: Float,
    anchura :: Float,
    angulos :: (Float, Float),
    radio :: Float,
    borde :: Float
  }
  deriving (Show)

data DatosForma = DatosForma
  { metadatosElemento :: MetaDatos,
    datosTexto :: String,
    datosPorcentajesTexto :: (Float, Float),
    datosEscalasTexto :: (Float, Float),
    datosRol :: String,
    datosMarcar :: Bool,
    datosTipo :: String,
    datosListaDePuntos :: Path,
    datosAltura :: Float,
    datosAnchura :: Float,
    datosAngulos :: (Float, Float),
    datosRadio :: Float,
    datosBorde :: Float
  }
  deriving (Show)

-- Constantes
rectangulo :: String
rectangulo = "rectangulo"

semicirculo :: String
semicirculo = "semicirculo"

circulo :: String
circulo = "circulo"

poligono :: String
poligono = "poligono"

boton :: String
boton = "boton"

checkbox :: String
checkbox = "checkbox"

radioButton :: String
radioButton = "radio button"

ninguno :: String
ninguno = "ninguno"

construyeForma :: ListaIdentificadores -> DatosForma -> (ListaIdentificadores, Forma)
construyeForma ids metadatos =
  ( idsActualizados,
    Forma
      { datosElemento = datosElemento,
        texto = datosTexto metadatos,
        porcentajesTexto = datosPorcentajesTexto metadatos,
        escalasTexto = datosEscalasTexto metadatos,
        rol = datosRol metadatos,
        marcar = datosMarcar metadatos,
        tipo = datosTipo metadatos,
        listaDePuntos = datosListaDePuntos metadatos,
        altura = datosAltura metadatos,
        anchura = datosAnchura metadatos,
        angulos = datosAngulos metadatos,
        radio = datosRadio metadatos,
        borde = datosBorde metadatos
      }
  )
  where
    (idsActualizados, datosElemento) = construyeElemento ids (metadatosElemento metadatos)

construyeFormas :: ListaIdentificadores -> [DatosForma] -> (ListaIdentificadores, [Forma])
construyeFormas ids [] = (ids, [])
construyeFormas ids (d : datosFormas) = (idsTerminados, forma : formas)
  where
    (idsActualizados, forma) = construyeForma ids d
    (idsTerminados, formas) = construyeFormas idsActualizados datosFormas

dibujaForma :: Malla -> Forma -> IO Picture
dibujaForma malla forma = do
  let datos = datosElemento forma
  let (porcentajeX, porcentajeY) = porcentajesTexto forma
  let (escalaX, escalaY) = escalasTexto forma
  let espacioForma@(p1, p2, p3, p4) | Prelude.length malla == 1 = cabeza "dibujaForma" malla | otherwise = esquinas datos
  let moduloX = moduloVector p1 p2
  let moduloY = moduloVector p1 p3
  let alturaFinal | altura forma > moduloY = moduloY | otherwise = altura forma
  let anchuraFinal | anchura forma > moduloX = moduloX | otherwise = anchura forma
  let puntoCentral = calculaPuntoSegunPosiciones moduloX moduloY p1 (0.5, 0.5)
  let puntoTexto = calculaPuntoSegunPosiciones moduloX moduloY p1 $ porcentajesTexto forma
  let formaDibujada = case tipo forma of
        "rectangulo" -> rectangleSolid anchuraFinal alturaFinal
        "semicirculo" -> uncurry arcSolid (angulos forma) (radio forma)
        "circulo" -> circleSolid $ radio forma
        "poligono" -> polygon $ listaDePuntos forma
        _ -> error "Tipo de forma especificada no existente o incorrecta"
  let radioForma = radio forma
  let marcaForma = case rol forma of
        "radio button" -> circleSolid (radioForma * 0.5)
        _ -> rectangleSolid (anchuraFinal * 0.5) (alturaFinal * 0.5)
  let angulosForma@(a1, a2) = angulos forma
  let bordeForma = case tipo forma of
        "rectangulo" -> bordeRectangulo (borde forma) anchuraFinal alturaFinal
        "semicirculo" -> bordeSemicirculo (borde forma) a1 a2 radioForma
        "circulo" -> bordeCirculo (borde forma) radioForma
        "poligono" -> lineLoop $ listaDePuntos forma
        _ -> error "Tipo de forma especificada no existente o incorrecta"
  let colFondo | pulsado datos && not (marcar forma) = colorFondoPulsado datos | otherwise = colorFondo datos
  let colTexto | pulsado datos && not (marcar forma) = colorTextoPulsado datos | otherwise = colorTexto datos
  let formaTerminada = uncurry translate puntoCentral $ color colFondo formaDibujada
  let marcaTerminada = uncurry translate puntoCentral $ color (if marcar forma then (colorTexto datos) else transparente) marcaForma
  let bordeTerminado = uncurry translate puntoCentral $ color colTexto bordeForma
  let textoTerminado = uncurry translate puntoTexto $ color colTexto $ scale escalaX escalaY $ text $ texto forma
  let dibujoBasico = case rol forma of
        "boton" -> pictures [formaTerminada, textoTerminado]
        "checkbox" -> formaTerminada
        "radio button" -> formaTerminada
        "ninguno" -> formaTerminada
        _ -> error "Rol especificado no existente o incorrecto"
  let dibujoMarcado = case rol forma of
        "boton" -> dibujoBasico
        "checkbox" -> pictures [dibujoBasico, marcaTerminada]
        "radio button" -> pictures [dibujoBasico, marcaTerminada]
        "ninguno" -> dibujoBasico
        _ -> error "Rol especificado no existente o incorrecto"
  let dibujoConBorde = pictures [dibujoBasico, bordeTerminado]
  let dibujoCompleto = pictures [dibujoMarcado, bordeTerminado]
  let dibujoFinal | marcar forma && borde forma > 0.0 = dibujoCompleto | marcar forma = dibujoMarcado | borde forma > 0.0 = dibujoConBorde | otherwise = dibujoBasico
  return dibujoFinal

bordeRectangulo :: Float -> Float -> Float -> Picture
bordeRectangulo borde ancho alto
  | borde <= 0 = rect
  | otherwise = pictures [bordeRectangulo thck wd hg, rect]
  where
    thck = borde - 0.1
    wd = ancho - ancho * 0.001
    hg = alto - alto * 0.001
    rect = rectangleWire ancho alto

bordeSemicirculo :: Float -> Float -> Float -> Float -> Picture
bordeSemicirculo borde a1 a2 radio
  | borde <= 0 = sem
  | otherwise = pictures [bordeSemicirculo thck a1 a2 rad, sem]
  where
    thck = borde - 0.1
    rad = radio - radio * 0.001
    sem = arc a1 a2 radio

bordeCirculo :: Float -> Float -> Picture
bordeCirculo borde radio
  | borde <= 0 = circ
  | otherwise = pictures [bordeCirculo thck rad, circ]
  where
    thck = borde - 0.1
    rad = radio - radio * 0.001
    circ = circle radio

metadatosFormaBoton :: DatosForma
metadatosFormaBoton =
  DatosForma
    { metadatosElemento = metaDatosElementoVacioFondoNegro,
      datosTexto = "",
      datosPorcentajesTexto = (0.4, 0.5),
      datosEscalasTexto = (0.1, 0.1),
      datosRol = "boton",
      datosMarcar = False,
      datosTipo = "rectangulo",
      datosListaDePuntos = [],
      datosAltura = 100,
      datosAnchura = 200,
      datosAngulos = (0, 0),
      datosRadio = 0,
      datosBorde = 0.5
    }

metadatosFormaCheckbox :: DatosForma
metadatosFormaCheckbox =
  DatosForma
    { metadatosElemento = metaDatosElementoVacioFondoTransparente,
      datosTexto = "",
      datosPorcentajesTexto = (0, 0),
      datosEscalasTexto = (0, 0),
      datosRol = "checkbox",
      datosMarcar = False,
      datosTipo = "rectangulo",
      datosListaDePuntos = [],
      datosAltura = 10,
      datosAnchura = 10,
      datosAngulos = (0, 0),
      datosRadio = 0,
      datosBorde = 0.2
    }

metadatosFormaRadioButton :: DatosForma
metadatosFormaRadioButton =
  DatosForma
    { metadatosElemento = metaDatosElementoVacioFondoTransparente,
      datosTexto = "",
      datosPorcentajesTexto = (0, 0),
      datosEscalasTexto = (0, 0),
      datosRol = "radio button",
      datosMarcar = False,
      datosTipo = "circulo",
      datosListaDePuntos = [],
      datosAltura = 0,
      datosAnchura = 0,
      datosAngulos = (0, 0),
      datosRadio = 10,
      datosBorde = 1.0
    }

metadatosFormaRectangulo :: DatosForma
metadatosFormaRectangulo =
  DatosForma
    { metadatosElemento = metaDatosElementoVacioFondoNegro,
      datosTexto = "",
      datosPorcentajesTexto = (0, 0),
      datosEscalasTexto = (0, 0),
      datosRol = "ninguno",
      datosMarcar = False,
      datosTipo = "rectangulo",
      datosListaDePuntos = [],
      datosAltura = 100,
      datosAnchura = 200,
      datosAngulos = (0, 0),
      datosRadio = 0,
      datosBorde = 0.0
    }

metadatosFormaSemicirculo :: DatosForma
metadatosFormaSemicirculo =
  DatosForma
    { metadatosElemento = metaDatosElementoVacioFondoNegro,
      datosTexto = "",
      datosPorcentajesTexto = (0, 0),
      datosEscalasTexto = (0, 0),
      datosRol = "ninguno",
      datosMarcar = False,
      datosTipo = "semicirculo",
      datosListaDePuntos = [],
      datosAltura = 0,
      datosAnchura = 0,
      datosAngulos = (10, 20),
      datosRadio = 0,
      datosBorde = 0.0
    }

metadatosFormaCirculo :: DatosForma
metadatosFormaCirculo =
  DatosForma
    { metadatosElemento = metaDatosElementoVacioFondoNegro,
      datosTexto = "",
      datosPorcentajesTexto = (0, 0),
      datosEscalasTexto = (0, 0),
      datosRol = "ninguno",
      datosMarcar = False,
      datosTipo = "circulo",
      datosListaDePuntos = [],
      datosAltura = 0,
      datosAnchura = 0,
      datosAngulos = (0, 0),
      datosRadio = 10,
      datosBorde = 0.0
    }

metadatosFormaPoligono :: DatosForma
metadatosFormaPoligono =
  DatosForma
    { metadatosElemento = metaDatosElementoVacioFondoNegro,
      datosTexto = "",
      datosPorcentajesTexto = (0, 0),
      datosEscalasTexto = (0, 0),
      datosRol = "ninguno",
      datosMarcar = False,
      datosTipo = "poligono",
      datosListaDePuntos = [(0, 15), (15, -15), (-15, -15)],
      datosAltura = 0,
      datosAnchura = 0,
      datosAngulos = (0, 0),
      datosRadio = 0,
      datosBorde = 0.0
    }

pulsaElemento :: String -> Bool -> Forma -> Forma
pulsaElemento idElemento banderaReset tipo
  | idElemento == idTipo = tipo {datosElemento = datosPulsados, marcar = not (marcar tipo)}
  | banderaReset = tipo {datosElemento = datosNoPulsados, marcar = False}
  | otherwise = tipo {datosElemento = datosNoPulsados}
  where
    datosTipo = datosElemento tipo
    datosPulsados = datosTipo {pulsado = not (pulsado datosTipo)}
    datosNoPulsados = datosTipo {pulsado = False}
    idTipo = identificador datosTipo