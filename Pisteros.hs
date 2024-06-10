import Text.Show.Functions

data Auto = UnAuto {
    marca :: String,
    modelo :: String,
    desgaste :: (Ruedas, Chasis),
    velocidadMaxima :: Float,
    tiempoDeCarrera :: Float
} deriving (Show)

type Ruedas = Float
type Chasis = Float
type Transformacion = Auto -> Auto
--- Punto 1 ---

ferrari :: Auto
ferrari = UnAuto "Ferrari" "F50" (0,0) 73 0

lambo :: Auto
lambo = UnAuto "Lamborghini" "Diablo" (7, 4) 73 0

fiat :: Auto
fiat = UnAuto "Fiat" "600" (27, 37) 44 0

--- Punto 2 ---

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado unAuto = estadoDelAuto (<50) (desgasteDeChasis unAuto) && estadoDelAuto (<60) (desgasteDeRueda unAuto)

elAutoEstaHechoBolsa :: Auto -> Bool
elAutoEstaHechoBolsa unAuto = estadoDelAuto (>80) (desgasteDeRueda unAuto) || estadoDelAuto (>80) (desgasteDeChasis unAuto)

desgasteDeRueda :: Auto -> Float
desgasteDeRueda = fst . desgaste 

desgasteDeChasis :: Auto -> Float
desgasteDeChasis = snd . desgaste

estadoDelAuto :: (Float -> Bool) -> Float -> Bool 
estadoDelAuto condicionDeEstado tipoDesgaste = condicionDeEstado tipoDesgaste   

--- Punto 3 ---

repararUnAuto :: Auto -> Auto
repararUnAuto = modificarDesgasteDeRueda (*0) . modificarDesgasteDeChasis (*0.15)

modificarDesgasteDeRueda :: (Float -> Float) -> Transformacion
modificarDesgasteDeRueda modificarRuedas unAuto = unAuto {desgaste = (modificarRuedas (desgasteDeRueda unAuto), desgasteDeChasis unAuto)}

modificarDesgasteDeChasis :: (Float -> Float) -> Transformacion
modificarDesgasteDeChasis modificarChasis unAuto = unAuto {desgaste = (desgasteDeRueda unAuto, modificarChasis (desgasteDeChasis unAuto))}

--- Punto 4.a ---

modificarTiempoEnCarrera :: Float -> Transformacion
modificarTiempoEnCarrera criterioASumar unAuto = unAuto {tiempoDeCarrera = tiempoDeCarrera unAuto + criterioASumar}

atravesarCurva :: Float -> Float -> Transformacion
atravesarCurva unAngulo unaLongitud unAuto = modificarDesgasteDeRueda (+ (3 * unAngulo / unaLongitud)) . 
                                            modificarTiempoEnCarrera (unaLongitud / (velocidadMaxima unAuto / 2)) $ unAuto
curvaPeligrosa :: Transformacion
curvaPeligrosa = atravesarCurva 60 300

curvaTranca :: Transformacion
curvaTranca = atravesarCurva 110 550

-- --- Punto 4.b ---

tramoRecto :: Float -> Transformacion
tramoRecto unaLongitud unAuto = modificarDesgasteDeChasis (/100) . modificarTiempoEnCarrera (unaLongitud / velocidadMaxima unAuto) $ unAuto

tramoRectoClassic :: Transformacion
tramoRectoClassic = tramoRecto 750

tramito :: Transformacion
tramito = tramoRecto 280

-- --- Punto 4.c ---

pasarPorBoxes :: Transformacion -> Transformacion
pasarPorBoxes luegoDeUnTramo unAuto
    | not . estaEnBuenEstado $ unAuto = modificarTiempoEnCarrera 10 unAuto
    | otherwise = luegoDeUnTramo unAuto

-- --- Punto 4.d ---

pasarPorUnaLimpiaditaOLluvia :: Transformacion -> Transformacion
pasarPorUnaLimpiaditaOLluvia luegoDeUnTramo unAuto = modificarTiempoEnCarrera (tiempoDeCarrera (luegoDeUnTramo unAuto) - (tiempoDeCarrera unAuto) / 2) unAuto

-- --- Punto 4.e ---

-- CAMBIAR AUTO -> AUTO POR TRANSOFMRACION
pasarPorRipio :: (Auto -> Auto) -> Transformacion
pasarPorRipio luegoDeUnTramo unAuto = luegoDeUnTramo . luegoDeUnTramo . modificarTiempoEnCarrera (2 * tiempoDeCarrera unAuto) $ unAuto

-- --- Punto 4.f ---

tramosConObstruccion :: Transformacion -> Float -> Transformacion
tramosConObstruccion luegoDeUnTramo unaLongitud unAuto = modificarDesgasteDeRueda (2 * unaLongitud *) (luegoDeUnTramo unAuto) 

-- --- Punto 5 ---

pasarPorTramo :: Transformacion -> Transformacion
pasarPorTramo luegoDelTramo unAuto
    | not . elAutoEstaHechoBolsa $ unAuto = luegoDelTramo unAuto
    | otherwise = unAuto

-- --- Punto 6 ---

superPista :: Transformacion
superPista = tramoRectoClassic . curvaTranca . tramito . pasarPorUnaLimpiaditaOLluvia tramito . 
                    tramosConObstruccion (atravesarCurva 80 400) 2 . atravesarCurva 115 650 . tramoRecto 970 . curvaPeligrosa .
                    pasarPorRipio tramito . pasarPorBoxes tramito

--- Punto 6b ---

pegarLaVuelta :: Pista -> [Auto] -> [Auto] 
pegarLaVuelta todosLosTramos autos = map (darVueltaUnoPorUno todosLosTramos) autos

darVueltaUnoPorUno :: Pista -> Auto -> Auto
darVueltaUnoPorUno luegoDeTodosLosTramos auto = foldl (flip pasarPorTramo) auto luegoDeTodosLosTramos

--- Punto 7 ---
data Carrera = UnaCarrera {
    numeroDeVueltas :: Int, 
    tramosDePista :: Pista
} deriving (Show)

type Pista = [Transformacion]

tourBuenosAires :: Carrera
tourBuenosAires = UnaCarrera 20 [superPista]

jugarLaCarrera :: [Auto] -> Carrera -> [[Auto]]
jugarLaCarrera autos unaCarrera = take (numeroDeVueltas unaCarrera) $ iterate (corredoresBuenos unaCarrera) autos

corredoresBuenos :: Carrera -> [Auto] -> [Auto]
corredoresBuenos unaCarrera autos = filter eliminarHechosBolsa (pegarLaVuelta (tramosDePista unaCarrera) autos)

eliminarHechosBolsa :: Auto -> Bool
eliminarHechosBolsa = not . elAutoEstaHechoBolsa
