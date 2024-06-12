import Text.Show.Functions

data Serie = UnaSerie {
    nombreDeSerie :: String,
    actores :: [Actor],
    presupuestoAnual :: Float,
    temporadas :: Int,
    ratingPromedio :: Float,
    cancelada :: Bool
} deriving (Show, Eq)

data Actor = UnActor {
    nombre :: String,
    sueldoAnual :: Float,
    restricciones :: [Restriccion]
} deriving (Show, Eq)

type Restriccion = String

paulRudd :: Actor
paulRudd = UnActor "Paul Rudd" 41000000 ["No actuar en bata", "Comer ensalada de rucula"]

johnnyDepp :: Actor
johnnyDepp = UnActor "Jhonny Depp" 20000000 []

helenaBohman :: Actor
helenaBohman = UnActor "Helena Bohman" 15000000 []

--- Punto 1 ---
estaEnRojo :: Serie -> Bool
estaEnRojo unaSerie = sum (sueldoTotalDeActores unaSerie) < presupuestoAnual unaSerie

sueldoTotalDeActores :: Serie -> [Float] 
sueldoTotalDeActores = map sueldoAnual . actores 

esSerieProblematica :: Serie -> Bool
esSerieProblematica = (>3) . actoresRestrictivos

sonRestrictivos :: Actor -> Bool
sonRestrictivos = (>1) . length . restricciones

actoresRestrictivos :: Serie -> Int
actoresRestrictivos = length . filter sonRestrictivos . actores 

--- Punto 2 ---

entranLosFavoritos :: [Actor] -> Produccion
entranLosFavoritos actoresFavs = modificarQuienesActuan ((++ actoresFavs) . drop 2)

modificarQuienesActuan :: ([Actor] -> [Actor]) -> Produccion
modificarQuienesActuan modificarActores unaSerie = unaSerie {actores = modificarActores . actores $ unaSerie}
 
actoresParaTimBurton :: Produccion
actoresParaTimBurton = modificarQuienesActuan (([johnnyDepp, helenaBohman] ++) . drop 2)

gatopardeitor :: Produccion
gatopardeitor unaSerie = unaSerie 

estireitor :: Produccion
estireitor unaSerie = unaSerie {temporadas = (*2) . temporadas $ unaSerie}

desespereitor :: Produccion
desespereitor = estireitor . actoresParaTimBurton

canceleitor :: Float -> Produccion
canceleitor ratingMinimo unaSerie
    | estaEnRojo unaSerie || ((<ratingMinimo) . ratingPromedio $ unaSerie) = unaSerie {cancelada = True} -- ¿Hace falta delegar esto? ---
    | otherwise = unaSerie

--- Punto 3 ---

bienestarPorTemporadas :: Serie -> Int
bienestarPorTemporadas unaSerie 
    | temporadas unaSerie > 4 = 5
    | otherwise = (10 - temporadas unaSerie) * 2

bienestarPorActores :: Serie -> Int
bienestarPorActores unaSerie
    | (<10) . length . actores $ unaSerie = 3
    | otherwise = 10 - actoresRestrictivos unaSerie

bienestarTotal :: Serie -> Int
bienestarTotal unaSerie 
    | estaCancelada unaSerie = 0
    | otherwise = bienestarPorActores unaSerie + bienestarPorTemporadas unaSerie --- No dice que hace, asi q supuse

estaCancelada :: Serie -> Bool
estaCancelada unaSerie = (==True) . cancelada $ unaSerie

--- Punto 4 ---<

type Produccion = Serie -> Serie

-- Función para comparar dos productores
maxByBienestar :: Produccion -> Produccion -> Serie -> Produccion
maxByBienestar productor1 productor2 unaSerie
    | bienestarTotal (productor1 unaSerie) > bienestarTotal (productor2 unaSerie) = productor1
    | otherwise = productor2

-- Función mayorEfectividad que encuentra el productor más efectivo
mayorEfectividad :: [Produccion] -> Serie -> Serie
mayorEfectividad productores unaSerie = foldl1 (\prod1 prod2 -> maxByBienestar prod1 prod2 unaSerie) productores unaSerie



--- Punto 5 ---

serieDeActoresInfinitos :: Serie
serieDeActoresInfinitos = UnaSerie "Joaco En Pelotas" muchosJohnnyDepp 0 0 0 False

muchosJohnnyDepp :: [Actor]
muchosJohnnyDepp = repeat johnnyDepp

--- 5. a) Como poder, se puede, el problema es que nunca va a mostrar los ultimos cuatro datos de la Serie
--- 5. b) No depende de nada, es lo mismo que el anterior, va a mostrarse infinitamente los actores. Lo unico que varia
---       es el orden en el que se agregan los nuevos actores, si se agregan antes, se muestran, sino nunca aparecerán


--- Punto 6 ---

esControvertida :: [Actor] -> Bool
esControvertida [] = False
esControvertida [_] = True
esControvertida (actor1 : actor2 : actores) = sueldoAnual actor1 < sueldoAnual actor2 && esControvertida (actor2 : actores)

--- Punto 7 ---

{- Explicar la inferencia del tipo de la siguiente función: funcionLoca x y = filter (even.x) . map (length.y) -}

funcionLoca :: (a -> Int) -> (Int -> [a]) -> [a] -> [Int]
funcionLoca x y {-elementos-} = filter (even . x) . map (length . y) {-elementos-}

--- La imagen de "x" tiene que estar dentro del dominio de "even", por ende, tiene que ser Int (o Integral)
--- La imagen de "y" tiene que estar dentro del dominio de "length", por ende, tiene que ser una lista de algo
--- Esta currificado, ya que al map le falta la lista a mappear
