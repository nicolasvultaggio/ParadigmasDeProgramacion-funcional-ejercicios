import Text.Show.Functions ()
import Text.XHtml (base)

data Persona = UnaPersona { 
    edad :: Int,
    sueniosQueCumplir :: [Suenio],
    nombre :: String,
    habilidades :: [Habilidad],
    felicidonios :: Int
} deriving Show

type Suenio = String
type Habilidad = String

cantidadDeSuenios :: Persona -> Int
cantidadDeSuenios unaPersona = length (sueniosQueCumplir unaPersona)

tipoDeFelicidad :: Persona -> String
tipoDeFelicidad unaPersona  
    | felicidonios unaPersona > 100 = "Muy Feliz"
    | felicidonios unaPersona <= 50 = "Poco Feliz"
    | otherwise = "Moderadamente Feliz"


gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona = enFuncionDeLaFelicidad  unaPersona (felicidonios unaPersona) (edad unaPersona) 2  * cantidadDeSuenios unaPersona

coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona 
    | enFuncionDeLaFelicidad  unaPersona (edad unaPersona) (cantidadDeSuenios unaPersona) 2 /= 2 = enFuncionDeLaFelicidad  unaPersona (edad unaPersona) (cantidadDeSuenios unaPersona) 2 * felicidonios unaPersona
    | otherwise = div (felicidonios unaPersona) 2

enFuncionDeLaFelicidad :: Persona -> Int -> Int -> Int -> Int 
enFuncionDeLaFelicidad unaPersona parametro1 parametro2 parametro3 
    | tipoDeFelicidad unaPersona == "Muy Feliz" =   parametro1
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" =  parametro2
    | tipoDeFelicidad unaPersona == "Poco Feliz" =  parametro3

{-enFuncionDeLaFelicidad :: Persona -> Int -> Int -> Int -> Int -> Int
enFuncionDeLaFelicidad unaPersona a b c d 
    | tipoDeFelicidad unaPersona == "Muy Feliz" =   a*b
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = a*c
    | tipoDeFelicidad unaPersona == "Poco Feliz" =  a * d 
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona 
    | tipoDeFelicidad unaPersona == "Muy Feliz" =  edad unaPersona * felicidonios unaPersona
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = cantidadDeSuenios unaPersona * felicidonios unaPersona
    | tipoDeFelicidad unaPersona == "Poco Feliz" = div (felicidonios unaPersona) 2
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona
    | tipoDeFelicidad unaPersona == "Muy Feliz" = felicidonios unaPersona * cantidadDeSuenios unaPersona
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = edad unaPersona * cantidadDeSuenios unaPersona 
    | tipoDeFelicdad unaPersona == "Poco Feliz" = 2 * cantidadDeSuenios unaPersona -}
