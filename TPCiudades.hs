import Text.Show.Functions
import Data.List(genericLength)

data Ciudad = UnaCiudad {
    nombre :: String,
    anioFundacion :: Int,
    atracciones :: [Atraccion],
    costoDeVida :: Float
} deriving Show

type Atraccion = String

data Año = UnAño {
    anio :: Int,
    eventos :: [Evento]
}deriving Show

type Evento = (Ciudad -> Ciudad)

azul = UnaCiudad "Azul" 1832 ["Teatro Español" , "Parque Municipal Sarmiento" , "Cstanera Cacique Catriel"] 190
caletaOlivia = UnaCiudad "Caleta Olivia" 1901 ["El Gorosito" , "Faro Costanera"] 120
baradero = UnaCiudad "Baradero" 1615 ["Parque del Este" , "Museo Alejandro Barbich"] 150
nullish = UnaCiudad "Nullish" 1800 [] 140


año2015 = UnAño 2015 []
año2021 = UnAño 2021 [ciudadEnCrisis , agregarNuevaAtraccion"playa"]
año2022 = UnAño 2022 [ciudadEnCrisis , ciudadARemodelar 5, ciudadAReevaluar 7]
año2023 = UnAño 2023 [agregarNuevaAtraccion "Parque", ciudadARemodelar 20 ,  ciudadARemodelar 10]

-------------
-- PUNTO 1 --
-------------

valorDeCiudad :: Ciudad -> Float
valorDeCiudad (UnaCiudad nombre anioFundacion atracciones costoDeVida) 
    | anioFundacion < 1800 = fromIntegral((*5) . (1800-) $ anioFundacion)
    | null atracciones = costoDeVida * 2
    | otherwise = costoDeVida * 3

 -------------
-- PUNTO 2.1 --
 -------------

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

esAtraccionCopada :: Ciudad -> Bool
esAtraccionCopada = any primerLetraVocal . atracciones 

primerLetraVocal :: String -> Bool
primerLetraVocal = esVocal . head        


 -------------
-- PUNTO 2.2 --
 -------------

esCiudadSobria :: Int -> Ciudad -> Bool
esCiudadSobria cantidadLetras unaCiudad = all (tieneMasLetras cantidadLetras) (atracciones unaCiudad)

tieneMasLetras :: Int -> String -> Bool
tieneMasLetras cantidadLetras = (>cantidadLetras) . length 

 -------------
-- PUNTO 2.3 --
 -------------

esCiudadRara :: Ciudad -> Bool
esCiudadRara = not . (tieneMasLetras 5) . nombre 

-------------
-- PUNTO 3 --
-------------

agregarNuevaAtraccion :: Atraccion -> Ciudad -> Ciudad
agregarNuevaAtraccion nuevaAtraccion = modificarCostoDeVida (*1.20) . modificarAtracciones (nuevaAtraccion :)

-- Usamos descartarUltimaAtraccion, que encapsula reverse's y un drop (antes habíamos usado un init, pero rompía con una lista de atracciones vacía)
ciudadEnCrisis :: Ciudad -> Ciudad
ciudadEnCrisis = modificarCostoDeVida (*0.90) . modificarAtracciones (descartarUltimaAtraccion) 


ciudadARemodelar :: Float -> Ciudad -> Ciudad
ciudadARemodelar porcentajeCostoDeVida = modificarCostoDeVida (* transformarPorcentaje porcentajeCostoDeVida) . modificarNombre 
    where
        transformarPorcentaje :: Float -> Float
        transformarPorcentaje porcentajeCostoDeVida = porcentajeCostoDeVida / 100 + 1


ciudadAReevaluar :: Int -> Ciudad -> Ciudad
ciudadAReevaluar cantidadLetras unaCiudad 
    | esCiudadSobria cantidadLetras unaCiudad = modificarCostoDeVida (*1.10) unaCiudad 
    | otherwise = modificarCostoDeVida (subtract 3) unaCiudad


--------------------------Funcion para no repetir lógica----------------------------------------
modificarCostoDeVida :: (Float -> Float) -> Ciudad -> Ciudad
modificarCostoDeVida modificador unaCiudad = unaCiudad {costoDeVida = modificador (costoDeVida unaCiudad)}

modificarNombre :: Ciudad -> Ciudad
modificarNombre unaCiudad = unaCiudad {nombre = "New " ++ (nombre unaCiudad)}

modificarAtracciones :: ([Atraccion] -> [Atraccion]) -> Ciudad -> Ciudad
modificarAtracciones modificador unaCiudad = unaCiudad {atracciones = modificador (atracciones unaCiudad)}

descartarUltimaAtraccion :: [Atraccion] -> [Atraccion]
descartarUltimaAtraccion = reverse . (drop 1) . reverse
--------------------------Funcion para no repetir lógica----------------------------------------


---------------
-- PUNTO 5.1 --
---------------

ciudadLuegoDeAños :: Año -> Ciudad -> Ciudad
ciudadLuegoDeAños unAño = pasarPorEventos (eventos unAño)

pasarPorEventos :: [Evento] -> Ciudad -> Ciudad
pasarPorEventos eventos unaCiudad = foldl aplicarEvento unaCiudad eventos

aplicarEvento :: Ciudad -> Evento -> Ciudad
aplicarEvento unaCiudad unEvento = unEvento unaCiudad


---------------
-- PUNTO 5.2 --
---------------

type Criterio a = Ciudad -> a

terminaMejorSegun :: (Ord a) => Criterio a -> Ciudad -> Evento -> Bool
terminaMejorSegun criterioComparacion unaCiudad unEvento = criterioComparacion (unEvento unaCiudad) > criterioComparacion unaCiudad


---------------
-- PUNTO 5.3 --
---------------

aSubirCostoDeVida :: Año -> Ciudad -> Ciudad
aSubirCostoDeVida unAño = transformarUnaCiudad unAño (terminaMejorSegun costoDeVida)  

---------------
-- PUNTO 5.4 --
---------------

aBajarCostoDeVida :: Año -> Ciudad -> Ciudad
aBajarCostoDeVida unAño = transformarUnaCiudad unAño hayQueBajar 

hayQueBajar :: Ciudad -> Evento -> Bool
hayQueBajar unaCiudad unEvento = not (terminaMejorSegun costoDeVida unaCiudad unEvento)

---------------
-- PUNTO 5.5 --  
---------------

aSubirValorDeCiudad :: Año -> Ciudad -> Ciudad
aSubirValorDeCiudad unAño = transformarUnaCiudad unAño (terminaMejorSegun valorDeCiudad) 

--------------------------Funcion para no repetir lógica----------------------------------------
filtrarSegun :: (Evento -> Bool) -> Año -> [Evento]
filtrarSegun condicionDeEvento unAño = filter condicionDeEvento (eventos unAño)

transformarUnaCiudad :: Año -> (Ciudad -> Evento -> Bool) -> Ciudad -> Ciudad 
transformarUnaCiudad unAño condicionDeEvento unaCiudad = pasarPorEventos (filtrarSegun (condicionDeEvento unaCiudad) unAño) unaCiudad 
--------------------------Funcion para no repetir lógica----------------------------------------


 -------------
-- PUNTO 6.1 --
 -------------

tieneEventosOrdenadosPorCostoDeVida :: Año -> Ciudad -> Bool
tieneEventosOrdenadosPorCostoDeVida unAño unaCiudad = incrementaPorEvento (evaluarCostosDeVida (aplicarEvento unaCiudad) (eventos unAño))

 -------------
-- PUNTO 6.2 --
 -------------

ciudadesAOrdenar :: [Ciudad] -> Evento -> Bool
ciudadesAOrdenar ciudades unEvento = incrementaPorEvento (evaluarCostosDeVida (flip aplicarEvento unEvento) ciudades)

 -------------
-- PUNTO 6.3 --
 -------------

añosAOrdenar :: [Año] -> Ciudad -> Bool
añosAOrdenar años unaCiudad = incrementaPorEvento (evaluarCostosDeVida (flip ciudadLuegoDeAños unaCiudad) años)


--------------------------Funcion para no repetir lógica----------------------------------------
incrementaPorEvento :: [Float] -> Bool
incrementaPorEvento [] = False  
incrementaPorEvento [_] = True
incrementaPorEvento (x1:x2:xs) = x1 < x2 && incrementaPorEvento (x2:xs) 

evaluarCostosDeVida :: (a -> Ciudad) -> [a] -> [Float]
evaluarCostosDeVida eventoModificador elementos = map (costoDeVida . eventoModificador) elementos
--------------------------Funcion para no repetir lógica----------------------------------------
                        -10%                +10% || -3          1%++
año2024 = UnAño 2024 (ciudadEnCrisis : ciudadAReevaluar 7 : eventosInfinitos)
eventosInfinitos :: [Evento]
eventosInfinitos = map ciudadARemodelar [1..]
                    29-2.9=26.1               29-3=26 ROMPE
                    31 - 3.1 = 27.9           31-3=28    INFINITO
                    
ciudadesInfinitas = (azul : nullish : discoRayado)
discoRayado :: [Ciudad]
discoRayado = cycle [caletaOlivia, baradero]


historiaSinFin = (año2021 : año2022 : infinitoAño2023)
infinitoAño2023 :: [Año]
infinitoAño2023 = repeat año2023