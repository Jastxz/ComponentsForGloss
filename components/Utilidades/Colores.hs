module Utilidades.Colores
  ( transparente,
    gris,
    marron,
  )
where

import Graphics.Gloss

transparente :: Color
transparente = makeColor 0 0 0 0

gris :: Color
gris = makeColor 0.4 0.4 0.4 0.8

marron :: Color
marron = makeColorI 140 76 0 255