module Lib () where

import Text.Show.Functions ()

type Poder = Personaje->Personaje

data Personaje = UnPersonaje{
  nombre :: String,
  poderBasico :: Poder,
  superPoder :: Poder,
  superPoderActivo :: Bool,
  cantidadVida :: Int,
  radioGranada :: Int,
  tipoTuercas :: String
}

esVidaNegativa :: Int -> Bool
esVidaNegativa vida = vida < 0

ponerACeroVidaNegativa :: Int -> Int
ponerACeroVidaNegativa vida
  | esVidaNegativa vida = 0
  | otherwise = vida 

bolaEspinosa :: Poder
bolaEspinosa atacado = atacado{cantidadVida = ponerACeroVidaNegativa (cantidadVida atacado - 1000)}

lluviaDeTuercas :: Personaje->Poder
lluviaDeTuercas  atacante afectado
  |tipoTuercas atacante=="Sanadoras" = afectado{cantidadVida = cantidadVida afectado + 800}
  |tipoTuercas atacante=="Daninas" = afectado{cantidadVida = cantidadVida afectado `div` 2}
  |otherwise = afectado

granadaDeEspinas :: Personaje->Poder
granadaDeEspinas atacante atacado
  |radioGranada atacante > 3 && atacado vida < 800 = atacado{nombre = nombre atacado ++ " Espina estuvo aqui", superPoderActivo = False, cantidadVida = 0}
  |radioGranada atacante > 3 = atacado{nombre = nombre atacado ++ " Espina estuvo aqui"}
  |atacado vida < 800 = atacado{superPoderActivo = False, cantidadVida = 0}
  |otherwise = bolaEspinosa atacado

torretaCurativa :: Poder
torretaCurativa ayudado = ayudado{superPoderActivo = True, cantidadVida = cantidadVida atacado * 2}
