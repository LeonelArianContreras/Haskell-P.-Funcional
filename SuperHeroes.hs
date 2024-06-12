data Heroe = Heroe {
    nombre :: String,
    vida :: Float,
    planetaOrigen :: String,
    artefacto :: Artefacto,
    enemigo :: Villano
} deriving (Show, Eq)

data Artefacto = Artefacto {
    nombreArtefacto :: String,
    daño :: Float
} deriving (Show, Eq)

data Villano = Villano {
    nombreVillano :: String,
    planeta :: String,
    arma :: Arma
} deriving (Show, Eq)

data Arma = Arma {
    nombreArma :: String,
    efectividad :: Float
} deriving (Show, Eq)

-------------
-- Punto 1 --
-------------

ironMan = Heroe "Tony Stark" 100.0 "Tierra" traje thanos
traje = Artefacto "Traje" 12.0
thanos = Villano "Thanos" "Titan" guanteleteDelInfinito 

thor = Heroe "Thor Odinson" 300.0 "Asgard" stormBreaker loki
stormBreaker = Artefacto "Stormbreaker" 0.0
loki = Villano "Loki Laufeyson" "Jotunheim" (cetro 20)

-------------
-- Punto 2 --
-------------
guanteleteDelInfinito = Arma "Guantelete del Infinito" 80.0

cetro :: Float -> Arma
cetro efectividad = Arma "Cetro" efectividad

usarGuantelete :: Heroe -> Heroe
usarGuantelete = alterarVida (* dañoRecibido guanteleteDelInfinito)

usarCetro :: Float -> Heroe -> Heroe
usarCetro efectividadDeArma = sufrirDañoDeArtefacto (+30) . cambiarArtefacto ("Machacado" ++).  alterarVida (* dañoRecibido (cetro efectividadDeArma))

alterarVida :: (Float -> Float) -> Heroe -> Heroe
alterarVida modificadorDeVida unHeroe = unHeroe {vida = modificadorDeVida . vida $ unHeroe}

cambiarArtefacto :: (String -> String) -> Heroe -> Heroe
cambiarArtefacto modificadorArtef unHeroe = unHeroe {artefacto = (artefacto unHeroe) {nombreArtefacto = modificadorArtef . nombreArtefacto $ (artefacto unHeroe)}}

sufrirDañoDeArtefacto :: (Float -> Float) -> Heroe -> Heroe
sufrirDañoDeArtefacto modificadorDaño unHeroe = unHeroe {artefacto = (artefacto unHeroe) {daño = modificadorDaño . daño $ (artefacto unHeroe)}}

-------------
-- Punto 3 --
-------------

sonAntagonistas :: Villano -> Heroe -> Bool
sonAntagonistas unVillano unHeroe = enemigo unHeroe == unVillano || 
                                    planetaOrigen unHeroe == planeta unVillano

-------------
-- Punto 4 --
-------------

atacar :: Heroe -> [Villano] -> Heroe
atacar unHeroe = foldl recibirDaño unHeroe

recibirDaño :: Heroe -> Villano -> Heroe
recibirDaño unHeroe unVillano 
    | sonEnemigos unVillano unHeroe = unHeroe
    | otherwise = alterarVida (* dañoRecibido (arma unVillano)) unHeroe

dañoRecibido :: Arma -> Float
dañoRecibido unArma = (100 - (efectividad unArma)) / 100

sonEnemigos :: Villano -> Heroe -> Bool
sonEnemigos unVillano = (== unVillano) . enemigo

-------------
-- Punto 5 --
-------------

heroesSalvadosDe :: Villano -> [Heroe] -> [Heroe]
heroesSalvadosDe unVillano = map convertirEnSuper . filter (seSalvaron unVillano)

convertirEnSuper :: Heroe -> Heroe
convertirEnSuper unHeroe = unHeroe {nombre = "Super " ++ nombre unHeroe}

seSalvaron :: Villano -> Heroe -> Bool
seSalvaron unVillano = (>50) . vida . alterarVida (* dañoRecibido (arma unVillano))

-------------
-- Punto 6 --
-------------

volverACasa :: [Heroe] -> [Heroe]
volverACasa = descansar . heroesSalvadosDe thanos 

descansar :: [Heroe] -> [Heroe]
descansar unosHeroes = map efectosDeDescanso unosHeroes

efectosDeDescanso :: Heroe -> Heroe
efectosDeDescanso = alterarVida (+30) . cambiarArtefacto dejarDeEstarMachacado
    where
        dejarDeEstarMachacado = (unwords . filter (/= "Machacado") . words)


-------------
-- Punto 7 --
-------------

esDebil :: Villano -> [Heroe] -> Bool
esDebil unVillano = all (sonEnemigos unVillano)



-------------
-- Punto 8 --
-------------

capa = Artefacto "Capa" 0

drStrange :: Heroe
drStrange = Heroe "Stephen Strange" 60 "Tierra" capa thanos

hacerClonesDeDrStrange :: [(Heroe, Int)]
hacerClonesDeDrStrange = [(drStrange, id) | id <- [1..]]

