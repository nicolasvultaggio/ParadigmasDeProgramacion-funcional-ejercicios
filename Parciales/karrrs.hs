{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Console.Haskeline (Settings(autoAddHistory))
import System.Win32 (COORD(yPos))
--import qualified Control.Applicative as infinitas 
{-# HLINT ignore "Use infix" #-}

data Carrera = UnaCarrera {
    vueltas :: Int,
    longitudDePista:: Int,
    participantes :: [Auto],
    publico :: [String]
}

data Auto = UnAuto{
    nombre:: String,
    nafta ::Int,
    velocidad::Int,
    nombreEnamorada :: String,
    truco :: Truco
}

type Truco = Auto -> Auto

modificarNafta :: (Int-> Int)->Auto -> Auto
modificarNafta funcion auto = auto{ nafta = funcion . nafta $ auto}

modificarVelocidad :: (Int-> Int)->Auto -> Auto
modificarVelocidad funcion auto = auto{ velocidad = funcion . velocidad $ auto}

deReversaRocha :: Int -> Truco
deReversaRocha metros  = modificarNafta (metros * 5 +)

impresionar :: [String] -> Truco
impresionar publico auto
    | elem (nombreEnamorada auto) publico = modificarVelocidad (*2) auto
    | otherwise = auto

nitro ::Truco
nitro = modificarVelocidad (+15)

comboLoco ::Int->Truco
comboLoco metros = nitro . deReversaRocha metros


rochaMcQueen ::Auto
rochaMcQueen = UnAuto "Rocha McQueen" 282 0 "Ronco" (deReversaRocha  undefined)

biankerr:: Auto
biankerr = UnAuto "Biankerr" 378 0 "Tincho" (impresionar undefined)

gushtav :: Auto
gushtav = UnAuto "Gushtav" 230 0 "Peti" nitro

rodra:: Auto
rodra = UnAuto "Rodra" 153 0 "Tais" (comboLoco undefined)

darVuelta ::Carrera-> Carrera
darVuelta  carrera = carrera{ participantes = ultimoReacciona . map (darVueltaAuto carrera) . participantes $ carrera } 

darVueltaAuto :: Carrera->Auto->Auto
darVueltaAuto carrera = incrementarVelocidad . restarNaftaPorVuelta carrera 

restarNaftaPorVuelta :: Carrera -> Auto -> Auto
restarNaftaPorVuelta carrera auto=  modificarNafta ( flip (-) (longitudDePista carrera * (length . nombre $ auto) ) ) auto

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad auto 
    | (not . null. nombre $ auto) && (length . nombre $ auto)<=5 = modificarVelocidad (+15) auto
    | (length . nombre $ auto)>=6 && (length . nombre $ auto)<=8 = modificarVelocidad (+20) auto
    | otherwise = modificarVelocidad (+30) auto 


ultimoReacciona :: [Auto]->[Auto]
ultimoReacciona autos =  map  (reaccionar . esUltimo (obtenerMinimaVelocidad autos)) autos


obtenerMinimaVelocidad :: [Auto] -> Int
obtenerMinimaVelocidad = minimum . map velocidad 

esUltimo ::  Int->Auto->(Auto, Bool)
esUltimo velMin auto 
    | velocidad auto == velMin = ( auto , True)
    | otherwise = (auto, False)

reaccionar :: (Auto,Bool) ->Auto
reaccionar (auto, True) = truco auto auto
reaccionar (auto, False) = auto

correrCarrera :: Carrera -> Carrera
--correrCarrera carrera = carrera{ participantes = map (darTodasLasVueltas (vueltas carrera) carrera) . participantes $ carrera}
correrCarrera carrera = foldr ($) carrera (replicate (vueltas carrera) darVuelta)


darTodasLasVueltas :: Int -> Carrera-> Auto-> Auto
darTodasLasVueltas cantVueltas carrera auto= foldr ($) auto (replicate cantVueltas (darVueltaAuto carrera))

obtenerGanador ::Carrera ->Auto
obtenerGanador  = ganador  . participantes . correrCarrera 

ganador ::[Auto]-> Auto 
ganador [x] = x 
ganador (x:y) = masVeloz x (ganador y)

masVeloz :: Auto -> Auto -> Auto
masVeloz auto1 auto2 
    | velocidad auto1 > velocidad auto2 = auto1
    | otherwise = auto2

recompetidores :: Carrera->[Auto]
recompetidores carrera  = filter (puedeCompetirDeNuevo carrera ). participantes . correrCarrera $ carrera

puedeCompetirDeNuevo :: Carrera->Auto-> Bool
puedeCompetirDeNuevo carrera auto = sobrevive auto || esGanadorDe carrera auto

esGanadorDe :: Carrera ->Auto -> Bool
esGanadorDe carrera auto =  (nombre .  ganador . participantes $ carrera) == nombre auto

sobrevive ::Auto->Bool
sobrevive auto = nafta auto > 27

unParticipante :: Auto
unParticipante = UnAuto undefined undefined undefined undefined undefined
carreraUltraSupremaDeLasAltasLigas:: Carrera
carreraUltraSupremaDeLasAltasLigas = UnaCarrera undefined undefined (repeat unParticipante) []

{-
Suponiendo que las variables indefinidas tienen valores reales
En CorrerCarrera, todos podrian dar la cantidad de vueltas necesarias para terminar la carrera
porque no tiene vueltas infinitas.
En DarVuelta, todos podrian dar una vuelta
en UltimoReacciona jamas se podria obtener el ultimo porque hay infinitos participantes
entonces jamas se podría completar la tarea de que este use su truco, por lo tanto, jamas terminaría de dar un resultado

No, por el mismo motivo

No, por el mismo motivo

-}