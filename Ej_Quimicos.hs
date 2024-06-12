data Especie = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

data Sustancia = UnElemento {
    nombreElemento :: String,
    simboloQElemento :: String,
    numeroAtomico :: Int,
    especieElemento :: Especie
} | UnCompuesto {
    nombreCompuesto :: String,
    componentes :: [Componente],
    simboloQCompuesto :: String,
    especieCompuesto :: Especie
} deriving (Show, Eq)

data Componente = UnComponente {
    cantidadMoleculas :: Int,
    sustancia :: Sustancia
} deriving (Show, Eq)

data TipoSustancia = SustanciaSencilla Elemento | SustanciaCompuesta Compuesto deriving (Show, Eq)

hidrogeno :: Elemento
hidrogeno = UnElemento {
    nombreElemento = "Hidrogeno",
    simboloQElemento = "H",
    numeroAtomico = 1,
    especieElemento = NoMetal
}

oxigeno :: Elemento
oxigeno = UnElemento {
    nombreElemento = "Oxigeno",
    simboloQElemento = "O",
    numeroAtomico = 8,
    especieElemento = NoMetal
}

agua :: Compuesto
agua = UnCompuesto {
    nombreCompuesto = "Agua",
    simboloQCompuesto = "H2O",
    componentes = [(UnComponente 2 (SustanciaSencilla hidrogeno)), (UnComponente 1 (SustanciaSencilla oxigeno))],
    especieCompuesto = NoMetal
}

------------------------------------------------------------------

data Criterio = Electricidad | Calor deriving(Show,Eq)

esConductor :: TipoSustancia -> Criterio -> Bool
esConductor (_(_ _ _ _ Metal)) _ = True
esConductor (SustanciaSencilla(UnElemento _ _ _ Metal)) _ = True
esConductor (SustanciaCompuesta(UnCompuesto _ _ _ GasNoble)) Electricidad = True
esConductor (SustanciaSencilla(UnElemento _ _ _ Halogeno)) Calor = True 
esConductor _ _ = False

------------------------------------------------------------------

consonantes :: String
consonantes = "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"

lasVocalesFuera :: Char -> Bool
lasVocalesFuera letra = not(elem letra consonantes)

agregarSufijo :: String -> String
agregarSufijo nombreSustancia = reverse (dropWhile lasVocalesFuera (reverse nombreSustancia)) ++ "uro"


nombreDeUnion :: String -> String
nombreDeUnion nombre = agregarSufijo nombre

------------------------------------------------------------------

combinar :: String -> String -> String
combinar primerNombre segundoNombre = agregarSufijo primerNombre ++ " de " ++ segundoNombre

------------------------------------------------------------------

nombreSustancia :: Componente -> String
nombreSustancia (UnComponente _ (SustanciaSencilla (UnElemento nombreElemento _ _ _))) = nombreElemento
nombreSustancia (UnComponente _ (SustanciaCompuesta (UnCompuesto nombreCompuesto _ _ _))) = nombreCompuesto

mezclar :: Componente -> Componente -> Compuesto
mezclar primerComponente segundoComponente =
    UnCompuesto (combinar (nombreSustancia primerComponente) (nombreSustancia segundoComponente)) 
    [primerComponente, segundoComponente] "" NoMetal