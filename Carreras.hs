import Text.Show.Functions

data Auto = Auto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int
} deriving (Show, Eq)

type Color = String
type Carrera = [Auto]

estanCerca :: Auto -> Auto -> Bool
estanCerca unAuto otroAuto = unAuto /= otroAuto && ((<10) . distanciaEntre unAuto $ otroAuto)

distanciaEntre :: Auto -> Auto -> Int
distanciaEntre unAuto otroAuto = abs (distancia unAuto - distancia otroAuto)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera =  tieneAutosCerca unAuto unaCarrera && (null . autosPorDelante unAuto $ unaCarrera)

tieneAutosCerca :: Auto -> Carrera -> Bool
tieneAutosCerca unAuto = any (estanCerca unAuto) 

autosPorDelante :: Auto -> Carrera -> Carrera
autosPorDelante unAuto = filter (leVanGanando unAuto)

leVanGanando :: Auto -> Auto -> Bool
leVanGanando unAuto = (< distancia unAuto) . distancia 

puesto :: Auto -> Carrera -> Int
puesto unAuto = (+1) . length . autosPorDelante unAuto

--- Punto 2 ---

correrAuto :: Int -> Auto -> Auto
correrAuto tiempoARecorrer unAuto = cambiarDistanciaRecorrida (+ tiempoARecorrer * velocidad unAuto) unAuto

cambiarDistanciaRecorrida :: (Int -> Int) -> Auto -> Auto
cambiarDistanciaRecorrida modificadorDeDistancia unAuto = unAuto {distancia = modificadorDeDistancia . distancia $ unAuto}

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad modificadorDeVelocidad unAuto = unAuto {velocidad = max 0 (modificadorDeVelocidad . velocidad $ unAuto)}

bajarLaVelocidad :: Int -> Auto -> Auto
bajarLaVelocidad = alterarVelocidad . subtract 

--- Punto 3 ---

type PowerUp = Auto -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Carrera -> PowerUp
terremoto autos unAuto = afectarALosQueCumplen (estanCerca unAuto) (bajarLaVelocidad 50) autos

miguelitos :: Int -> Carrera -> PowerUp
miguelitos velocidadABajar autos unAuto = afectarALosQueCumplen (leVanGanando unAuto) (bajarLaVelocidad velocidadABajar) autos

jetpack :: Int -> Carrera -> PowerUp
jetpack tiempoARecorrer autos unAuto = afectarALosQueCumplen (== unAuto) (modificacionesPorJetPack tiempoARecorrer) autos

modificacionesPorJetPack :: Int -> Auto -> Auto
modificacionesPorJetPack tiempoARecorrer = alterarVelocidad id . correrAuto tiempoARecorrer . alterarVelocidad (2*)

--- Punto 4a ---

type Evento = Carrera -> Carrera

simularCarrera :: Carrera -> [Evento] -> [(Int, Color)] --- LO QUE ME COSTO ESTE PUNTO, NUNCA TUVE TANTOS PROBLEMAS DE TIPO
simularCarrera autos eventos = tablaDePosiciones (pasarPorEventos eventos autos)

pasarPorEventos :: [Evento] -> Evento
pasarPorEventos eventos autos = foldl aplicarEvento autos eventos

aplicarEvento :: Carrera -> Evento -> Carrera
aplicarEvento autos unEvento = unEvento autos

tablaDePosiciones :: Carrera -> [(Int, Color)]
tablaDePosiciones autos = map (hacerTabla autos) autos

hacerTabla :: Carrera -> Auto -> (Int, Color)
hacerTabla autos unAuto = (puesto unAuto autos, color unAuto)

--- Punto 4b ---

correnTodos :: Int -> Evento
correnTodos tiempoARecorrer = map (correrAuto tiempoARecorrer) 

usaPowerUp :: Color -> PowerUp -> Evento
usaPowerUp colorABuscar pasarPorPowerUp autos = pasarPorPowerUp (encontrar ((== colorABuscar) . color) autos)

encontrar :: (Auto -> Bool) -> Carrera -> Auto
encontrar hayAutoDeEsteColor = head . filter hayAutoDeEsteColor 


autoRojo = Auto "Rojo" 120 0
autoAzul = Auto "Azul" 120 0
autoNegro = Auto "Negro" 120 0
autoBlanco = Auto "Blanco" 120 0

autitosPedorros :: [Auto]
autitosPedorros = [autoAzul, autoBlanco, autoNegro, autoRojo]

-- carreraIncreiblementePedorra :: [(Int, Color)]
-- carreraIncreiblementePedorra = simularCarrera autitosPedorros [correnTodos 30, usaPowerUp "Azul" (flip jetpack 30), usaPowerUp "Blanco" terremoto,
--                                 correnTodos 40, usaPowerUp "Blanco" (flip miguelitos 20), usaPowerUp "Negro" (flip jetpack 6), correnTodos 10]
