import Text.Show.Functions ()

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

coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion unaPersona 
    | tipoDeFelicidad unaPersona == "Muy Feliz" =  edad unaPersona * felicidonios unaPersona
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = cantidadDeSuenios unaPersona * felicidonios unaPersona
    | tipoDeFelicidad unaPersona == "Poco Feliz" = div (felicidonios unaPersona) 2

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion unaPersona
    | tipoDeFelicidad unaPersona == "Muy Feliz" = felicidonios unaPersona * cantidadDeSuenios unaPersona
    | tipoDeFelicidad unaPersona == "Moderadamente Feliz" = edad unaPersona * cantidadDeSuenios unaPersona 
    | tipoDeFelicidad unaPersona == "Poco Feliz" = 2 * cantidadDeSuenios unaPersona 
