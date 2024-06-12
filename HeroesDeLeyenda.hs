import Text.Show.Functions

data Heroe = UnHeroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
} deriving (Show)

data Artefacto = UnArtefacto {
    nombre :: String,
    rareza :: Int
} deriving (Show)

type Tarea = Heroe -> Heroe 

xiphos :: Artefacto
xiphos = UnArtefacto "Xiphos" 50

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = UnArtefacto "Lanza Del Olimpo" 100

relampagoDeZeus :: Artefacto
relampagoDeZeus = UnArtefacto "Rempalago de Zeus" 500

epitetoSegunReconocimiento :: Heroe -> Heroe
epitetoSegunReconocimiento unHeroe
    | reconocimiento unHeroe > 1000 = cambiarEpiteto "El magico" unHeroe
    | reconocimiento unHeroe > 500 = cambiarEpiteto "El magnifico" . modificarArtefactos (lanzaDelOlimpo :) $ unHeroe
    | reconocimiento unHeroe > 100 = cambiarEpiteto "Hoplita" . modificarArtefactos (xiphos :) $ unHeroe 
    | otherwise = unHeroe

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto nombreNuevo unHeroe = unHeroe {epiteto = nombreNuevo}

-- agregarArtefacto :: Artefacto -> Heroe -> Heroe
-- agregarArtefacto unArtefacto unHeroe = unHeroe {artefactos = unArtefacto : artefactos unHeroe}

modificarArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
modificarArtefactos modificador unHeroe = unHeroe {artefactos = modificador (artefactos unHeroe)}

--- Punto 3 ---

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto unArtefacto = modificarArtefactos (unArtefacto :) . aumentarReconocimiento (rareza unArtefacto)

aumentarReconocimiento :: Int -> Heroe -> Heroe
aumentarReconocimiento cantidadDeReconocimiento unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + cantidadDeReconocimiento}

escalarElOlimpo :: Tarea
escalarElOlimpo = aumentarReconocimiento 500 . desecharArtefactosInutiles . triplicarRarezaDeArtefactos . modificarArtefactos (relampagoDeZeus :)

desecharArtefactosInutiles :: Heroe -> Heroe
desecharArtefactosInutiles = modificarArtefactos (filter (segunBuenosArtefactos))

segunBuenosArtefactos :: Artefacto -> Bool
segunBuenosArtefactos = (<1000) . rareza 

triplicarRarezaDeArtefactos :: Heroe -> Heroe
triplicarRarezaDeArtefactos = modificarArtefactos (map triplicadorRarezas)

triplicadorRarezas :: Artefacto -> Artefacto
triplicadorRarezas unArtefacto = UnArtefacto {rareza = (*3) . rareza $ unArtefacto}

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadDeCuadras = cambiarEpiteto ("Gros" ++ replicate cantidadDeCuadras 'O')

type Debilidad = Heroe -> Bool

data Bestia = UnaBestia {
    debilidad :: Debilidad,
    nombreBestia :: String
}

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia unaBestia unHeroe 
    | (debilidad unaBestia) unHeroe  = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) unHeroe
    | otherwise = cambiarEpiteto "El cobarde" . modificarArtefactos (drop 1) $ unHeroe

{-
tenerCiertoArtefacto :: Bestia -> Heroe -> Bool
tenerCiertoArtefacto unaBestia unHeroe = elem (debilidad unaBestia) (artefactos unHeroe)

tenerCiertoReconocimiento :: Bestia -> Heroe -> Bool
tenerCiertoReconocimiento unaBestia unHeroe = reconocimiento unHeroe > debilidad unaBestia
-}

--- Punto 4 ---

heracles :: Heroe
heracles = UnHeroe "Guardian Del Olimpo" 700 [pistola, relampagoDeZeus] [matarUnaBestia leonDeNemea]

pistola :: Artefacto
pistola = UnArtefacto "Pistola" 1000

--- Punto 5 ---

leonDeNemea :: Bestia
leonDeNemea = UnaBestia ((>20) . length . epiteto) "Leon De Nemea"

--- Punto 6 ---

realizarTarea :: Tarea -> Heroe -> Heroe
realizarTarea unaTarea unHeroe = agregarTarea unaTarea (unaTarea unHeroe)

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea nuevaTarea unHeroe = unHeroe {tareas = nuevaTarea : tareas unHeroe}

--- Punto 7 ---

presumirLogros :: Heroe -> Heroe -> (Heroe, Heroe)
presumirLogros unHeroe otroHeroe 
    | hayGanador unHeroe otroHeroe = (unHeroe, otroHeroe)
    | hayGanador otroHeroe unHeroe = (otroHeroe, unHeroe)
    | otherwise = presumirLogros (realizarLabor (tareas otroHeroe) unHeroe) (realizarLabor (tareas unHeroe) otroHeroe)

hayGanador :: Heroe -> Heroe -> Bool
hayGanador ganador perdedor = reconocimiento ganador > reconocimiento perdedor || 
                              reconocimiento ganador == reconocimiento perdedor && cantidadDeRareza (artefactos ganador) > cantidadDeRareza (artefactos perdedor)

cantidadDeRareza :: [Artefacto] -> Int
cantidadDeRareza artefactos = sum (map rareza artefactos)

--- Punto 9 ---

realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor tareas unHeroe = foldl (flip realizarTarea) unHeroe tareas

heroe1 = UnHeroe "Pedro" 100 [] []
heroe2 = UnHeroe "Pepe" 100 [] []
tareasInfinitas :: [Tarea]
tareasInfinitas = map aumentarReconocimiento [1..]