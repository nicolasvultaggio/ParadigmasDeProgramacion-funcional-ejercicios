import Text.XHtml (base)

data Jugador = UnJugador {
    nombreDeJugador :: String,
    edad :: Int ,
    promedioDeGol :: Float ,
    habilidad :: Int,
    cansancio :: Float
} deriving (Show)


type NombreDeEquipo = String
type Jugadores = [Jugador]
type NombreDeGrupo = Char
type Grupo = (NombreDeGrupo , [Equipo])

type Equipo =( NombreDeEquipo, NombreDeGrupo , Jugadores )

quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs

esFigura :: Jugador -> Bool
esFigura unJugador = habilidad unJugador > 75 && promedioDeGol unJugador > 0

jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

esFarandulero :: Jugador-> Bool
esFarandulero unJugador = nombreDeJugador unJugador `elem` jugadoresFaranduleros

jugadoresDeUnEquipo :: Equipo ->(Jugador->Bool) -> Jugadores
jugadoresDeUnEquipo (_,_,[]) _ = []
jugadoresDeUnEquipo (_,_,listaDeJugadores) condicion = filter condicion listaDeJugadores

figurasDeEquipo :: Equipo -> Jugadores
figurasDeEquipo unEquipo = jugadoresDeUnEquipo unEquipo esFigura

tieneFarandulero :: Equipo -> Jugadores
tieneFarandulero unEquipo = jugadoresDeUnEquipo unEquipo esFarandulero

esJoven :: Jugador -> Bool
esJoven unJugador = edad unJugador < 27

esJugadorDificil :: Jugador -> Bool
esJugadorDificil unJugador = esFigura unJugador && esJoven unJugador && ( not . esFarandulero  $ unJugador)

jugadoresDificilesDeUnGrupo :: Grupo -> Jugadores
jugadoresDificilesDeUnGrupo ( _ , listaDeEquipos) = filter esJugadorDificil . concatMap (\(_,_,x)->x) $ listaDeEquipos

desgasteFisico :: Equipo -> Jugadores
desgasteFisico (_,_,listaDeJugadores)= map jugadorJuega listaDeJugadores

jugadorJuega :: Jugador ->Jugador
jugadorJuega unJugador
    | not (esFarandulero unJugador) && esFigura unJugador && esJoven unJugador = unJugador { cansancio = 50}
    | esJoven unJugador = modificarCansancio unJugador (*) 1.1
    | esFigura unJugador = modificarCansancio unJugador (+) 20
    | otherwise =  modificarCansancio unJugador (*) 2

modificarCansancio :: Jugador -> (Float->Float->Float) -> Float -> Jugador
modificarCansancio unJugador funcion parametro = unJugador { cansancio =  funcion (cansancio unJugador) parametro }

determinarGanador :: Equipo-> Equipo-> Equipo
determinarGanador equipoA equipoB =  head (quickSort juegaMejor [ equipoA,equipoB])

juegaMejor:: Equipo -> Equipo -> Bool
juegaMejor (_,_,equipoA) (_,_,equipoB) = rendimientoDeJugadores equipoA > rendimientoDeJugadores equipoB

rendimientoDeJugadores :: Jugadores -> Float
rendimientoDeJugadores = sum . map promedioDeGol . take 11 . quickSort cansancioDeJugadores

cansancioDeJugadores :: Jugador -> Jugador -> Bool
cansancioDeJugadores jugadorA jugadorB = cansancio jugadorA < cansancio jugadorB

jugarPartido :: Equipo -> Equipo ->( NombreDeEquipo, Jugadores)
jugarPartido equipoA equipoB = ( (\(x,_,_)->x)(determinarGanador equipoA equipoB) , desgasteFisico (determinarGanador equipoA equipoB) )