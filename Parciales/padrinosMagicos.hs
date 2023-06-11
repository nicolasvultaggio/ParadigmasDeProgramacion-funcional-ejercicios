
data Chico = UnChico{
    nombre::String,
    edad:: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
}

data Chica = UnaChica{
    nombrec :: String,
    condicion :: Condicion
}
type Habilidad =String
type Deseo = Chico -> Chico
type Padrino = Chico -> Chico

trixie :: Chica
trixie = UnaChica "Trixie Tang" noEsTimmy

vicky :: Chica
vicky = UnaChica "Vicky" (tieneHabilidad "ser un supermodelo noruego")



--PuntoA

--Auxiliares

modificarEdad :: Chico -> (Int->Int)->Chico
modificarEdad unChico funcion = unChico{edad = funcion . edad $ unChico}

--Pedido

    --Deseos

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades unasHabilidades unChico = unChico{habilidades = (++ unasHabilidades) . habilidades $ unChico}

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed = aprenderHabilidades ["jugar need for speed 1", "jugar need for speed 2"]

serMayor :: Deseo
serMayor unChico = modificarEdad unChico a18


--Padrinos

--Auxiliares Padrinos
madurar :: Chico -> Chico
madurar unChico = modificarEdad unChico (+1)

desmadurar ::Chico ->Chico
desmadurar unChico = modificarEdad unChico (`div` 2)

a18:: Int->Int
a18 numero = 18

noDeseo ::Deseo
noDeseo = id

--Padrinos Pedidos
wanda :: Padrino
wanda unChico = madurar. (head . deseos $ unChico) $ unChico

cosmo :: Padrino
cosmo =  noDeseo . desmadurar

muffinMagico :: Padrino
muffinMagico unChico = aplicarDeseos unChico (deseos unChico)


aplicarDeseos ::  Chico -> [Deseo] -> Chico
aplicarDeseos  = foldr ($) -- recibe el caso base y la lista
--Parte B

type Condicion = Chico -> Bool

tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad unaHabilidad  =  elem unaHabilidad . habilidades

esSuperMaduro :: Condicion
esSuperMaduro unChico = esMayorDeEdad unChico && sabeManejar unChico

esMayorDeEdad ::  Chico -> Bool
esMayorDeEdad  =  (>=18) . edad

sabeManejar :: Chico -> Bool
sabeManejar  =  tieneHabilidad "saber manejar"

noEsTimmy :: Condicion
noEsTimmy = (/= "Timmy") . nombre

quienConquistaA ::Chica -> [Chico] ->Chico
quienConquistaA _ [x] = x
quienConquistaA unaChica (x:xs)
    |  condicion unaChica x = x
    | otherwise = quienConquistaA unaChica xs

tuti :: Chica
tuti = UnaChica "Tuti" (tieneHabilidad "saber cocinar")

quienConquistaATuti :: [Chico] -> Chico
quienConquistaATuti = quienConquistaA tuti

--Da Rules

infractoresDeDaRules :: [Chico] -> [Chico]
infractoresDeDaRules  = filter tieneDeseosProhibidos 

tieneDeseosProhibidos :: Chico ->Bool
tieneDeseosProhibidos unChico = any (esDeseoProhibido unChico) (deseos unChico)

esDeseoProhibido :: Chico -> Deseo -> Bool
esDeseoProhibido unChico unDeseo = any esHabilidadProhibida . take 5 . habilidades . unDeseo $ unChico

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida habilidad = habilidad `elem` habilidadesProhibidas

habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["enamorar","matar","dominar el mundo"]

{-
Justificaciones


-}

