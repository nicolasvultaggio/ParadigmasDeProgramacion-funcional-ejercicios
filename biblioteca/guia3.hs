import Graphics.Win32 (vK_NUMPAD1)
import GHC.Conc (numCapabilities)
import Distribution.PackageDescription (GenericPackageDescription(condBenchmarks))
import Data.Ratio (numerator)
type Tupla3 = (Float, Float, Float)

fst3 :: Tupla3 -> Float
fst3 (n,_,_) = n

snd3 :: Tupla3 -> Float
snd3 (_,n,_) = n

trd3 :: Tupla3 -> Float
trd3 (_,_,n) = n

type TuplaFunc = (Float->Float, Float->Float)
aplicar :: TuplaFunc -> Float -> (Float,Float)
aplicar (funcion1, funcion2) entero = (funcion1 entero, funcion2 entero)

type DosNumeros = (Float, Float)
cuentaBizarra :: DosNumeros -> Float
cuentaBizarra ( num1 , num2 )
    | num1 > num2 = num1 + num2
    | (num2-10) > num1 = num2 - num1
    | num2 >= num1 = num1 * num2

type Notas = ( Float , Float)

esNotaBochazo :: Float -> Bool
esNotaBochazo nota = nota<6

aprobo :: Notas -> Bool
aprobo ( notaA, notaB) = not(esNotaBochazo notaA) && not(esNotaBochazo notaB )

suman15 :: Float-> Float-> Bool
suman15 a b = (a+b)>15

promociono :: Notas -> Bool
promociono (nota1 ,nota2) = suman15 nota1 nota2 && (nota1>7 || nota2>8)

consulta :: String -> Notas -> Bool
consulta pregunta (parc1,parc2) = esNotaBochazo parc1

type Recup = (Float,Float)
type ParcialYRecup = (Notas,Recup)

notasFinales :: ParcialYRecup -> (Float,Float)
notasFinales ((parc1 , parc2),( recup1, recup2)) = ( max parc1 recup1 , max parc2 recup2 )


recuperoDeGusto :: ParcialYRecup -> Bool
recuperoDeGusto (notas,recup) = deGusto (fst notas) (fst recup) || deGusto (snd notas) (snd recup)

deGusto :: Float->Float->Bool
deGusto nota recup = not (esNotaBochazo nota) && recup/= (-1)

{-
Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva True si es mayor de 21 años y False en caso contrario. Por Ej:.
Main> esMayorDeEdad (juan,18) 
False 
Nota: Definir la función utilizando aplicación parcial y composición.
-}
type Nombre = String
type Edad = Float 
type Persona = (Nombre,Edad)

esMayorDeEdad :: Persona -> Bool
esMayorDeEdad = (>21) . edadDePersona 

edadDePersona :: Persona -> Edad
edadDePersona (_,edad) = edad



{-
Definir la función calcular, que recibe una tupla de 2 elementos, 
si el primer elemento es par lo duplica, sino lo deja como está 
y con el segundo elemento en caso de ser impar le suma 1
y si no deja esté último como esta. 
-}

calcular ::Integral a => (a,a)->(a,a) 
calcular (numero1, numero2) = (calcular1 numero1, calcular2 numero2)

calcular1 :: Integral a => a -> a
calcular1 numero = (funcionYCondicion numero (2*). even) numero

calcular2 :: Integral c => c -> c 
calcular2 numero = (funcionYCondicion numero (+1). odd) numero

funcionYCondicion :: Integral a => a -> (a->a) -> Bool -> a
funcionYCondicion numero funcion condicion 
    | condicion = funcion numero
    | otherwise = numero


{-dobleSiEsPar :: Integral a => a -> Bool -> a
dobleSiEsPar num cond
    | cond = 2*num
    | otherwise = num 

masUnoSiEsImpar :: Integral e => e -> Bool ->e
masUnoSiEsImpar num cond
    | cond = num+1
    | otherwise = num -}







