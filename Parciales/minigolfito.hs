
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Ratio (numerator)
{-# HLINT ignore "Use infix" #-}

data Jugador = UnJugador {
nombre :: String,
padre :: String,
habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
fuerzaJugador :: Int,
precisionJugador :: Int
} deriving (Eq, Show)

data Tiro = UnTiro {
velocidad :: Int,
precision :: Int,
altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord b => (a->b) -> [a]->a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

--Mi solución

--1)
type Palo = Habilidad -> Tiro

menos3 :: (Ord a, Num a) => a -> a
menos3 num
    | num <=3 = 0
    | otherwise = num - 3

putter :: Palo
putter habilidad = UnTiro 10 ((2*).precisionJugador $ habilidad) 0 

madera :: Palo
madera habilidad = UnTiro 100 (flip div 2 . precisionJugador $ habilidad) 5

hierroN :: Int -> Palo
hierroN n habilidad = UnTiro ((n*) . fuerzaJugador $ habilidad) (flip div n . precisionJugador $ habilidad) (menos3 n)

palos ::[Palo]
palos  = [putter,madera] ++ map hierroN [1,2..10]

-- 2)
golpe ::  Palo ->Jugador -> Tiro
golpe palo = palo . habilidad 

--3)
tiroNulo :: Tiro
tiroNulo = UnTiro 0 0 0

data Obstaculo = UnObstaculo {
    puedeSuperarTiro :: Tiro-> Bool,
    efectoTiro :: Tiro->Tiro
}

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo{
    puedeSuperarTiro = superaTunelConRampita,
    efectoTiro = efectoTunelConRampita
}
efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro ((*2) . velocidad $ tiro) 100 0
superaTunelConRampita :: Tiro->Bool
superaTunelConRampita tiro = esTiroRaso tiro && velocidad tiro > 90
esTiroRaso ::Tiro ->Bool
esTiroRaso = (==0) . altura

lagunaN ::Int ->Obstaculo
lagunaN largo =UnObstaculo{
    puedeSuperarTiro = superaLaguna,
    efectoTiro = efectoLaguna largo
}

superaLaguna :: Tiro ->Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5.altura) tiro
efectoLaguna :: Int ->Tiro ->  Tiro
efectoLaguna largo tiro = tiro { altura = altura tiro `div` largo } 

hoyo :: Obstaculo 
hoyo = UnObstaculo{
    puedeSuperarTiro = superaHoyo,
    efectoTiro = efectoHoyo
}

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && precision tiro > 95 && esTiroRaso tiro
efectoHoyo ::Tiro-> Tiro
efectoHoyo _ = tiroNulo

--4)

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo)  palos

leSirveParaSuperar :: Jugador -> Obstaculo ->Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperarTiro obstaculo (golpe  palo jugador)

--4b)
--Es la de los tiros consecutivos chan chan chan

--Solucion Recursiva
cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (o:os)  
    | puedeSuperarTiro o tiro 
        = 1+ cuantosObstaculosConsecutivosSupera (efectoTiro o tiro) os
    | otherwise = 0 

--4c)  

paloMasUtil :: Jugador -> Obstaculo -> Palo
paloMasUtil jugador obstaculos 
    = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos . golpe jugador ) palos


{-

No sirve el planteo del obstaculo para resolver el ejercicio
type Obstaculo = Tiro -> Tiro



tunelConRampita :: Obstaculo
tunelConRampita = superaObstaculo superaTunelConRampita efectoTunelConRampita
efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro ((*2) . velocidad $ tiro) 100 0
superaTunelConRampita :: Tiro->Bool
superaTunelConRampita tiro = esTiroRaso tiro && velocidad tiro > 90
esTiroRaso ::Tiro ->Bool
esTiroRaso = (==0) . altura

laguna ::Int->Obstaculo
laguna largo= superaObstaculo superaLaguna  (efectoLaguna largo)
superaLaguna :: Tiro ->Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5.altura) tiro
efectoLaguna :: Int ->Tiro ->  Tiro
efectoLaguna largo tiro = tiro { altura = altura tiro `div` largo } 

hoyo :: Obstaculo
hoyo = superaObstaculo superaHoyo efectoHoyo
superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && precision tiro > 95 && esTiroRaso tiro
efectoHoyo ::Tiro-> Tiro
efectoHoyo _ = tiroNulo

superaObstaculo :: (Tiro->Bool)->(Tiro->Tiro)->Tiro->Tiro
superaObstaculo superaObstaculo efectoObstaculo tiroOriginal
    | superaObstaculo tiroOriginal = efectoObstaculo tiroOriginal
    | otherwise = tiroNulo

--4a)

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter undefined palos

leSirveParaSuperar :: Jugador -> Obstaculo ->Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = 
-}











