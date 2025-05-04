module Lib () where

import Text.Show.Functions ()

type Poder = Personaje->Personaje->Personaje

data Personaje = UnPersonaje{
  nombre :: String,
  poderBasico :: Poder,
  superPoder :: Poder,
  superPoderActivo :: Bool,
  cantidadVida :: Int,
  radioGranada :: Int,
  tipoTuercas :: String
} deriving(Show)

esVidaNegativa :: Int -> Bool
esVidaNegativa vida = vida < 0

ponerACeroVidaNegativa :: Int -> Int
ponerACeroVidaNegativa vida
  | esVidaNegativa vida = 0
  | otherwise = vida 

bolaEspinosa :: Poder
bolaEspinosa atacante atacado = atacado{cantidadVida = ponerACeroVidaNegativa (cantidadVida atacado - 1000)}

lluviaDeTuercas :: Poder
lluviaDeTuercas  atacante afectado
  |tipoTuercas atacante=="Sanadoras" = afectado{cantidadVida = cantidadVida afectado + 800}
  |tipoTuercas atacante=="Daninas" = afectado{cantidadVida = cantidadVida afectado `div` 2}
  |otherwise = afectado

granadaDeEspinas :: Poder
granadaDeEspinas atacante atacado
  |radioGranada atacante > 3 && cantidadVida atacado < 800 = atacado{nombre = nombre atacado ++ " Espina estuvo aqui", superPoderActivo = False, cantidadVida = 0}
  |radioGranada atacante > 3 = atacado{nombre = nombre atacado ++ " Espina estuvo aqui"}
  |cantidadVida atacado < 800 = atacado{superPoderActivo = False, cantidadVida = 0}
  |otherwise = bolaEspinosa atacante atacado

torretaCurativa :: Poder
torretaCurativa atacante ayudado = ayudado{superPoderActivo = True, cantidadVida = cantidadVida ayudado * 2}

atacarPoderEspecial :: Personaje -> Personaje -> Personaje
atacarPoderEspecial atacante atacado = (poderBasico atacante) atacante ((superPoder atacante) atacante atacado)

espina :: Personaje
espina = UnPersonaje{nombre="Espina", poderBasico=bolaEspinosa, superPoder=granadaDeEspinas, superPoderActivo=True, cantidadVida=4800, radioGranada=5, tipoTuercas=""}

pamela :: Personaje
pamela = UnPersonaje{nombre="Pamela", poderBasico=lluviaDeTuercas, superPoder=torretaCurativa,superPoderActivo=False, cantidadVida=9600, radioGranada=0, tipoTuercas="Sanadoras"}

personajes :: [Personaje]
personajes = [espina, pamela]

enLasUltimas :: Personaje -> Bool
enLasUltimas personaje = cantidadVida personaje < 800

quienesEnLasUltimas :: [Personaje]
quienesEnLasUltimas = filter enLasUltimas personajes