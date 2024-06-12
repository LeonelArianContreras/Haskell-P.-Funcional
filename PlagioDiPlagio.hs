import Text.Show.Functions

data Autor = Autor {
    nombre :: String,
    obras :: [Obra]
} deriving (Show)

data Obra = Obra {
    texto :: String,
    añoDePublicacion :: Int
} deriving (Show)

habiaUnaVezUnPato = Obra "Había una vez un pato." 1997
habiaOtraVezUnPato = Obra "¡Había una vez un pato!" 1996
mirthaSusanaYMoria = Obra "Mirtha, Susana y Moria." 2010
amoblamientoVertebral = Obra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020
semantica = Obra "La semántica funcional de Mirtha, Susana y Moria." 2022

cortazar = Autor "Julio Cortazar" [habiaOtraVezUnPato, habiaOtraVezUnPato]
pizarnik = Autor "Pizarnik" [mirthaSusanaYMoria, amoblamientoVertebral, semantica]

-----------------------------------------------

letras :: String
letras = ['a' ..'z'] ++ ['A' ..'Z'] ++ [' '] ++ ['1'.. '9']

versionCruda :: String -> String
versionCruda texto = filter sonLetras (map eliminarPuntuaciones texto)

sonLetras :: Char -> Bool
sonLetras unCaracter = elem unCaracter letras

eliminarPuntuaciones :: Char -> Char
eliminarPuntuaciones 'á' = 'a'
eliminarPuntuaciones 'é' = 'e'
eliminarPuntuaciones 'í' = 'i'
eliminarPuntuaciones 'ó' = 'o'
eliminarPuntuaciones 'ú' = 'u'
eliminarPuntuaciones 'Á' = 'A'
eliminarPuntuaciones 'É' = 'E'
eliminarPuntuaciones 'Í' = 'I'
eliminarPuntuaciones 'Ó' = 'O'
eliminarPuntuaciones 'Ú' = 'U'
eliminarPuntuaciones caracter = caracter

--- Punto 3 ---

copiaLiteral :: Obra -> Obra -> Bool
copiaLiteral obraPlagio laObra = versionCruda (texto obraPlagio) == versionCruda (texto laObra) && esPosterior obraPlagio laObra

esPosterior :: Obra -> Obra -> Bool
esPosterior obraPlagio laObra = añoDePublicacion obraPlagio > añoDePublicacion laObra

empiezaIgual :: Int -> Obra -> Obra -> Bool
empiezaIgual cantidadLetras obraPlagio laObra = tieneMenosLetras obraPlagio laObra && tienenMismasLetras cantidadLetras (texto obraPlagio) (texto laObra) 
                                                && esPosterior obraPlagio laObra

tieneMenosLetras :: Obra -> Obra -> Bool
tieneMenosLetras obraPlagio laObra = length (texto obraPlagio) < length (texto laObra)

tienenMismasLetras :: Int -> String -> String -> Bool
tienenMismasLetras cantidadLetras textoPlagio textoOriginal = (take cantidadLetras textoPlagio) == (take cantidadLetras textoOriginal)

leAgregaronIntro :: Obra -> Obra -> Bool
leAgregaronIntro obraPlagio laObra = tienenMismasLetras (length . texto $ laObra) (darVuelta obraPlagio) (darVuelta laObra)

darVuelta :: Obra -> String
darVuelta = reverse . texto 

--- Punto 4 ---

type Bot = Obra -> Obra -> Bool

botBoton :: Bot
botBoton unaObra otraObra = empiezaIugal unaObra otraObra

botON :: Bot
botON unaObra otraObra = leAgregaronIntro unaObra otraObra

esPlagio :: 