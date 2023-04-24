{-Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3-}
esMultiploDeTres :: Int -> Bool 
esMultiploDeTres numero = mod numero 3 == 0 
{-Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero-}
esmultiplode :: Int -> Int -> Bool
esmultiplode unNumero otroNumero = mod unNumero otroNumero == 0
{-Definir la función cubo/1, devuelve el cubo de un número-}
cubo :: Int -> Int 
cubo x = x * x * x 
{-Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.-}
area :: Int -> Int -> Int
area base altura = base * altura
{-Definir la función esBisiesto/1, indica si un año es bisiesto.
(Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) 
Nota: Resolverlo reutilizando la función esMultiploDe/2-}
esbisiesto :: Int -> Bool 
esbisiesto anio = esmultiplode anio 400 || esmultiplode anio 4 && not(esmultiplode anio 100)

{-Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.-}
celsiusToFahr :: Float -> Float
celsiusToFahr t = t * 1.8 + 32
fahrToCelsius :: Float -> Float
fahrToCelsius t = (t-32)/1.8

{-Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. 
Decimos que hace frío si la temperatura es menor a 8 grados Celsius. -}

esFria :: Float -> Bool
esFria unaTemperatura = unaTemperatura < 8

haceFrio :: Float -> Bool
haceFrio hace = (esFria.fahrToCelsius) hace




