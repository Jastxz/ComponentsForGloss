module DatosElementosPreconstruidos
  ( elementoBasico,
    almacenBasico,
    almacenInput,
  )
where

import AlmacenElementos as A
import Graphics.Gloss (black, blue, magenta)
import Tipos.TipoBarraNavegacion as B
import Tipos.TipoBase as Base
import Tipos.TipoElemento
import Tipos.TipoEntrada as En
import Tipos.TipoEtiqueta as Et
import Tipos.TipoForma as F
import Tipos.TipoLista as L
import Tipos.TipoParrafo as P
import Tipos.TipoPosicion (Esquinas)
import Tipos.TipoSaltoDeLinea as S
import Tipos.TipoTitulo as T
import Utilidades.Colores (marron)

-- Definición de esquinas
-- ------------------------------------------------------------------------
esquinasBasica :: Esquinas
esquinasBasica = ((-150, 150), (150, 150), (-150, -150), (150, -150))

esquinasSupIzq :: Esquinas
esquinasSupIzq = ((-750, 400), (-250, 400), (-750, 100), (-250, 100))

esquinasSupCent :: Esquinas
esquinasSupCent = ((-200, 400), (250, 400), (-200, 100), (250, 100))

esquinasSupDer :: Esquinas
esquinasSupDer = ((300, 400), (750, 400), (300, 100), (750, 100))

esquinasCentIzq :: Esquinas
esquinasCentIzq = ((-500, 50), (-50, 50), (-500, -50), (50, -50))

esquinasCentDer :: Esquinas
esquinasCentDer = ((50, 50), (500, 50), (50, -50), (500, -50))

esquinasInfIzq :: Esquinas
esquinasInfIzq = ((-750, -100), (-250, -100), (-750, -400), (-250, -400))

esquinasInfCent :: Esquinas
esquinasInfCent = ((-200, -100), (250, -100), (-200, -400), (250, -400))

esquinasInfDer :: Esquinas
esquinasInfDer = ((300, -100), (750, -100), (300, -400), (750, -400))

-- ------------------------------------------------------------------------

-- Definición de elementos
-- ------------------------------------------------------------------------
elementoBasico :: MetaDatos
elementoBasico = metaDatosElementoVacioFondoBlanco {datosEsquinas = esquinasBasica, datosColorFondo = marron}

elementoFondoAzul :: MetaDatos
elementoFondoAzul = elementoBasico {datosColorFondo = blue}

elementoEsquinasTitulos :: MetaDatos
elementoEsquinasTitulos = elementoFondoAzul {datosEsquinas = ((-75, 15), (75, 15), (-75, -15), (75, -15))}

elementoSupIzq :: MetaDatos
elementoSupIzq = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasSupIzq, datosColorFondo = magenta}

elementoSupCent :: MetaDatos
elementoSupCent = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasSupCent, datosColorFondo = blue}

elementoSupDer :: MetaDatos
elementoSupDer = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasSupDer, datosColorFondo = marron, datosNombre = "1"}

elementoCentIzq :: MetaDatos
elementoCentIzq = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasCentIzq, datosColorTexto = black}

elementoCentDer :: MetaDatos
elementoCentDer = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasCentDer, datosColorTexto = black}

elementoInfIzq :: MetaDatos
elementoInfIzq = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasInfIzq, datosColorTexto = black}

elementoInfCent :: MetaDatos
elementoInfCent = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasInfCent, datosColorFondo = blue}

elementoInfDer :: MetaDatos
elementoInfDer = metaDatosElementoVacioFondoNegro {datosEsquinas = esquinasInfDer, datosColorFondo = marron}

-- ------------------------------------------------------------------------

baseBasica :: DatosBase
baseBasica = metadatosBaseVacia

barraBasica :: DatosBarraNavegacion
barraBasica = metadatosBarraVacia {B.metadatosElemento = elementoBasico}

barraInput :: DatosBarraNavegacion
barraInput = metadatosBarraVacia {B.metadatosElemento = elementoSupIzq}

botonBasico :: DatosForma
botonBasico = metadatosFormaBoton {F.metadatosElemento = elementoBasico, F.datosTexto = "Boton", F.datosBorde = 1.0}

botonInput :: DatosForma
botonInput = metadatosFormaBoton {F.metadatosElemento = elementoSupCent, F.datosTexto = "Boton", F.datosBorde = 1.0}

entradaBasica :: DatosEntrada
entradaBasica = metadatosEntradaVacia {En.metadatosElemento = elementoBasico, En.datosTexto = "Entrada"}

entradaInput :: DatosEntrada
entradaInput = metadatosEntradaVacia {En.metadatosElemento = elementoSupDer, En.datosTexto = "Entrada"}

etiquetaBase :: DatosEtiqueta
etiquetaBase = metadatosEtiquetaVacia {Et.metadatosElemento = elementoBasico, Et.datosTexto = "Etiqueta"}

etiquetaInput :: DatosEtiqueta
etiquetaInput = metadatosEtiquetaVacia {Et.metadatosElemento = elementoCentIzq, Et.datosTexto = "Etiqueta"}

listaBasica :: DatosLista
listaBasica =
  metadatosListaVacia
    { L.metadatosElemento = elementoBasico,
      L.datosEncabezado = "Encabezado lista",
      L.datosTextos = ["Item 1", "Item 2"]
    }

listaInput :: DatosLista
listaInput =
  metadatosListaVacia
    { L.metadatosElemento = elementoCentDer,
      L.datosEncabezado = "Encabezado lista",
      L.datosTextos = ["Item 1", "Item 2"]
    }

textoDelParrafo :: String
textoDelParrafo = "En la vastedad del cosmos, las estrellas titilan como destellos fugaces en la inmensidad del espacio. La galaxia gira con gracia, llevando consigo secretos ancestrales y misterios cósmicos. En un rincón remoto, un planeta azul y sereno se mece en la órbita, albergando la asombrosa diversidad de la vida. Bajo el manto estrellado, la naturaleza sigue su curso, tejida con la danza eterna de la luz y la sombra."

parrafoBasico :: DatosParrafo
parrafoBasico = metadatosParrafoVacio {P.metadatosElemento = elementoBasico, P.datosTexto = textoDelParrafo}

parrafoInput :: DatosParrafo
parrafoInput = metadatosParrafoVacio {P.metadatosElemento = elementoInfIzq, P.datosTexto = textoDelParrafo}

saltoDeLineaBasico :: DatosSaltoDeLinea
saltoDeLineaBasico = metadatosSaltoDeLineaVacio {S.metadatosElemento = elementoFondoAzul}

saltoDeLineaInput :: DatosSaltoDeLinea
saltoDeLineaInput = metadatosSaltoDeLineaVacio {S.metadatosElemento = elementoInfCent}

tituloBasico :: DatosTitulo
tituloBasico = metadatosTituloVacio {T.metadatosElemento = elementoEsquinasTitulos, T.datosTexto = "Titulo 1"}

tituloInput :: DatosTitulo
tituloInput = metadatosTituloVacio {T.metadatosElemento = elementoInfDer, T.datosTexto = "Titulo 1"}

almacenBasico :: DatosAlmacen
almacenBasico =
  DatosAlmacen
    { datosBarras = [barraBasica],
      datosBases = [baseBasica],
      datosEntradas = [entradaBasica],
      datosEtiquetas = [etiquetaBase],
      A.datosFormas = [botonBasico],
      datosListas = [listaBasica],
      datosParrafos = [parrafoBasico],
      datosSaltosDeLinea = [saltoDeLineaBasico],
      datosTitulos = [tituloBasico]
    }

almacenInput :: DatosAlmacen
almacenInput =
  DatosAlmacen
    { datosBarras = [barraInput],
      datosBases = [baseBasica],
      datosEntradas = [entradaInput],
      datosEtiquetas = [etiquetaInput],
      A.datosFormas = [botonInput],
      datosListas = [listaInput],
      datosParrafos = [parrafoInput],
      datosSaltosDeLinea = [saltoDeLineaInput],
      datosTitulos = [tituloInput]
    }
