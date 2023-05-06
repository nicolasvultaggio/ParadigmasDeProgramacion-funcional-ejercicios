import Text.Show.Functions ()
import Text.XHtml (base)

data Persona = UnaPersona {
    edad :: Int,
    sueniosQueCumplir :: [Suenio],
    nombre :: String,
    habilidades :: [Habilidad],
    felicidonios :: Int
} deriving (Show,Eq)

type Suenio = String
type Habilidad = String

cantidadDeSuenios :: Persona -> Int
cantidadDeSuenios  = length . sueniosQueCumplir 

tipoDeFelicidad :: Persona -> String
tipoDeFelicidad unaPersona
    | felicidonios unaPersona > 100 = "Muy Feliz"
    | felicidonios unaPersona <= 50 = "Poco Feliz"
    | otherwise = "Moderadamente Feliz"



gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona = (* cantidadDeSuenios unaPersona ) (enFuncionDeLaFelicidad  unaPersona (felicidonios unaPersona) (edad unaPersona) 2)


coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona = definoOperacion unaPersona (felicidonios unaPersona) (enFuncionDeLaFelicidad unaPersona (edad unaPersona) (cantidadDeSuenios unaPersona) 2)

definoOperacion :: Persona -> (Int->Int->Int)
definoOperacion unaPersona
    | tipoDeFelicidad unaPersona == "Poco Feliz" =  div
    | otherwise = (*)

enFuncionDeLaFelicidad :: Persona -> Int -> Int -> Int -> Int
enFuncionDeLaFelicidad unaPersona parametro1 parametro2 parametro3
    | tipoDeFelicidad unaPersona == "Muy Feliz" =   parametro1
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" =  parametro2
    | tipoDeFelicidad unaPersona == "Poco Feliz" =  parametro3


{-
Primera Original
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona 
    | tipoDeFelicidad unaPersona == "Muy Feliz" =  (* felicidonios unaPersona) edad unaPersona 
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = (* felicidonios unaPersona) cantidadDeSuenios unaPersona 
    | tipoDeFelicidad unaPersona == "Poco Feliz" = div (felicidonios unaPersona) 2

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona
    | tipoDeFelicidad unaPersona == "Muy Feliz" = felicidonios unaPersona * cantidadDeSuenios unaPersona
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = edad unaPersona * cantidadDeSuenios unaPersona 
    | tipoDeFelicdad unaPersona == "Poco Feliz" = 2 * cantidadDeSuenios unaPersona 
-}
