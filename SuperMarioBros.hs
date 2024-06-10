import Data.Char (isUpper, isLower)
import Text.Show.Functions
import Data.List(genericLength)

data Plomero = UnPlomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historialDeReparaciones :: [Reparacion], -- Preguntarle a Joa si hace falta una tupla (dec, req)
    dinero :: Int
} deriving (Show, Eq)

data Herramienta = UnaHerramienta {
    nombreHerramienta :: String,
    precio :: Int,
    materialEmpuñadura :: Material
} deriving (Show, Eq)

data Reparacion = UnaReparacion {
    descripcion :: String,
    requerimiento :: Herramienta
} deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

llaveInglesa = UnaHerramienta "Llave Inglesa" 200 Hierro
llaveDelCorazonDeJoaco = UnaHerramienta "Corazon de Altalef" 1 Madera
martillo = UnaHerramienta "Martillo" 20 Madera
destornillador = UnaHerramienta "Destornillador" 0 Plastico

mario = UnPlomero "Mario" [UnaHerramienta "Llave Inglesa" 200 Hierro, UnaHerramienta "Martillo" 20 Madera] [] 1200
wario = UnPlomero "Wario" llavesDeWario [] 50

llavesDeWario :: [Herramienta]
llavesDeWario = repeat (UnaHerramienta "Llave Inglesa" 1 Hierro)

--- Punto 2 ---

tieneUnaHerramienta :: String -> Plomero -> Bool
tieneUnaHerramienta laHerramienta = any (nombreDeherramienta laHerramienta) . cajaDeHerramientas

nombreDeherramienta :: String -> Herramienta -> Bool
nombreDeherramienta nombreHerramientaBuscada = (== nombreHerramientaBuscada) . nombreHerramienta 

elPlomeroEsMalvado :: Plomero -> Bool
elPlomeroEsMalvado = (== "Wa") . take 2 . nombre 

puedeComprarHerramienta :: Herramienta -> Plomero -> Bool
puedeComprarHerramienta unaHerramienta = (<= (precio unaHerramienta)) . dinero

--- Punto 3 ---

esBuenaHerramienta :: Herramienta -> Bool
esBuenaHerramienta (UnaHerramienta "Martillo" _ Goma) = True
esBuenaHerramienta (UnaHerramienta "Martillo" _ Madera) = True
esBuenaHerramienta unaHerramienta = precio unaHerramienta > 10000 && materialEmpuñadura unaHerramienta == Hierro


--- Punto 4 ---

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    | puedeComprarHerramienta unaHerramienta unPlomero = actualizarDinero (subtract (precio unaHerramienta)) unPlomero
    | otherwise = unPlomero

actualizarDinero :: (Int -> Int) -> Plomero -> Plomero
actualizarDinero modificadorDeDinero unPlomero = unPlomero {dinero = modificadorDeDinero . dinero $ unPlomero}

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta unaHerramienta unPlomero = unPlomero {cajaDeHerramientas = unaHerramienta : cajaDeHerramientas unPlomero}

--- Punto 5 ---

filtracionDeAgua = UnaReparacion "Filtracion de Agua" llaveInglesa

esDificilLaReparacion :: Reparacion -> Bool
esDificilLaReparacion unaReparacion = ((>100) . length . descripcion $ unaReparacion) && all isUpper (descripcion unaReparacion)

presupuestoDeReparacion :: Reparacion -> Int
presupuestoDeReparacion = (*3) . length . descripcion

--- Punto 6 ---

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion unaReparacion unPlomero 
    | puedeReparar unaReparacion unPlomero = reparacionHecha unaReparacion . actualizarDinero (+ presupuestoDeReparacion unaReparacion) . visitarParaReparar unaReparacion $ unPlomero
    | otherwise = actualizarDinero (+100) unPlomero

visitarParaReparar :: Reparacion -> Plomero -> Plomero
visitarParaReparar unaReparacion unPlomero
    | elPlomeroEsMalvado unPlomero = agregarHerramienta destornillador unPlomero
    | esDificilLaReparacion unaReparacion = perderHerramientasBuenas unPlomero
    | otherwise = olvidarPrimerHerramienta unPlomero

puedeReparar :: Reparacion -> Plomero -> Bool
puedeReparar unaReparacion unPlomero = elem (requerimiento unaReparacion) (cajaDeHerramientas unPlomero) || 
                                      (elPlomeroEsMalvado unPlomero && elem martillo (cajaDeHerramientas unPlomero))

reparacionHecha :: Reparacion -> Plomero -> Plomero
reparacionHecha unaReparacion unPlomero = unPlomero {historialDeReparaciones = (unaReparacion :) . historialDeReparaciones $ unPlomero}

perderHerramientasBuenas :: Plomero -> Plomero
perderHerramientasBuenas = modificarCajaDeHerramientas (filter (not . esBuenaHerramienta))

olvidarPrimerHerramienta :: Plomero -> Plomero
olvidarPrimerHerramienta = modificarCajaDeHerramientas (drop 1)

modificarCajaDeHerramientas :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero 
modificarCajaDeHerramientas modificadorCajaHerramientas unPlomero = unPlomero {cajaDeHerramientas = modificadorCajaHerramientas (cajaDeHerramientas unPlomero)}

--- Punto 7 ---

plomeroLuegoDeLaburar :: [Reparacion] -> Plomero -> Plomero
plomeroLuegoDeLaburar reparaciones unPlomero = foldl (flip hacerReparacion) unPlomero reparaciones

--- Punto 8 ---

type CriterioComparacion a = Plomero -> a

plomerosLuegoDeReparar :: [Reparacion] -> [Plomero] -> [Plomero]
plomerosLuegoDeReparar reparaciones plomeros = map (plomeroLuegoDeLaburar reparaciones) plomeros

seleccionMejorSegun :: (Ord a) => CriterioComparacion a -> [Plomero] -> Plomero
seleccionMejorSegun _ [unPlomero] = unPlomero
seleccionMejorSegun condicionDeComparacion (x1:x2:xs)
    | condicionDeComparacion x1 > condicionDeComparacion x2 = seleccionMejorSegun condicionDeComparacion (x1:xs)
    | otherwise = seleccionMejorSegun condicionDeComparacion (x2:xs)

empleadoMasReparador :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasReparador = escogerMejorEmpleadoSegun (length . historialDeReparaciones) 

empleadoMasAdinerado :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasAdinerado = escogerMejorEmpleadoSegun dinero 

empleadoMasInversor :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasInversor = escogerMejorEmpleadoSegun sumaDeValoresDeHerramientas

sumaDeValoresDeHerramientas :: Plomero -> Int
sumaDeValoresDeHerramientas unPlomero = foldl (+) 0 (valorDeHerramientas unPlomero)

valorDeHerramientas :: Plomero -> [Int]
valorDeHerramientas = map precio . cajaDeHerramientas 

escogerMejorEmpleadoSegun :: (Ord a) => CriterioComparacion a -> [Reparacion] -> [Plomero] -> Plomero
escogerMejorEmpleadoSegun elegirSegunCondicion reparaciones = seleccionMejorSegun elegirSegunCondicion (plomerosLuegoDeReparar reparaciones plomeros)
