
data Heroe = Heroe {
    nombre :: String,
    epiteto :: String,
    reconocimiento ::Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}

data Bestia = Bestia{
    nombreB :: String,
    debilidad :: Heroe -> Bool
}

type Artefacto = (String, Rareza)
type Rareza = Int
type Tarea = Heroe -> Heroe
type Labor = [Tarea]

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria heroe
    | (1000<) . reconocimiento $ heroe = modificarEpiteto "El Mítico" heroe
    | (500<=) . reconocimiento $ heroe = modificarEpiteto "El Magnífico" . aniadirArtefacto ("Lanza Del Olimpo", 100) $ heroe
    | (\x -> x<500 && x>100) . reconocimiento $ heroe =  modificarEpiteto "Hoplita" . aniadirArtefacto ("Xiphos", 50) $ heroe
    | otherwise = heroe

modificarEpiteto :: String -> Heroe -> Heroe
modificarEpiteto unEpiteto heroe = heroe{ epiteto = unEpiteto}

aniadirArtefacto :: Artefacto -> Heroe -> Heroe
aniadirArtefacto unArtefacto heroe = heroe{ artefactos = (:) unArtefacto  . artefactos $ heroe}

modificarReconocimiento :: (Int -> Int) -> Heroe -> Heroe
modificarReconocimiento unaFuncion heroe = heroe { reconocimiento = unaFuncion . reconocimiento $ heroe}

rareza :: (a, b) -> b
rareza = snd

modificarRarezaDeArtefacto :: (Int-> Int)-> Artefacto -> Artefacto
modificarRarezaDeArtefacto unaFuncion (suNombre,suRareza) = (suNombre , unaFuncion suRareza)

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto artefacto  = modificarReconocimiento (rareza artefacto +) . aniadirArtefacto artefacto

escalarElOlimpo :: Tarea
escalarElOlimpo heroe = modificarReconocimiento (+500) . aniadirArtefacto ("Relampago de Zeus",500) $ heroe {artefactos = filter ((>1000) . rareza) . map (modificarRarezaDeArtefacto (*3)) . artefactos $ heroe  }

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras  = modificarEpiteto ( "Gros" ++  replicate cuadras 'o')

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia bestia heroe
    | debilidad bestia heroe = modificarEpiteto ( "El Asesino de " ++ nombreB bestia)  heroe
    | otherwise = modificarEpiteto "El Cobarde" $ heroe{ artefactos = tail . artefactos $ heroe}

unaTarea :: Tarea
unaTarea = undefined

heracles :: Heroe
heracles = Heroe "Heracles" "Guardian del Olimpo" 700 [("Pistola",1000),("Relampago de Zeus",500)] [unaTarea]

matarLeonDeNemea :: Tarea
matarLeonDeNemea = matarUnaBestia (Bestia "Leon de Nemea" ((>=20) . length . epiteto))

hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea tarea heroe = tarea $ heroe{ tareas = (:) tarea . tareas $ heroe}

presumirLogros :: Heroe-> Heroe -> (Heroe, Heroe)
presumirLogros unHeroe otroHeroe = (ganador unHeroe otroHeroe , perdedor unHeroe otroHeroe)

resultado :: (Int->Int->Bool)->Heroe -> Heroe -> Heroe
resultado funcion unHeroe otroHeroe 
    | reconocimiento unHeroe /= reconocimiento otroHeroe = obtener funcion reconocimiento unHeroe otroHeroe
    | sumaDeRarezas unHeroe /= sumaDeRarezas otroHeroe = obtener funcion sumaDeRarezas unHeroe otroHeroe
    | otherwise = resultado funcion (foldr ($) unHeroe (tareas otroHeroe)) (foldr ($) otroHeroe (tareas unHeroe))

ganador :: Heroe -> Heroe -> Heroe
ganador = resultado (>)

perdedor :: Heroe -> Heroe -> Heroe
perdedor = resultado (<)

obtener ::(Int->Int->Bool)-> (Heroe -> Int)->Heroe->Heroe -> Heroe
obtener funcion dato unHeroe otroHeroe
    | funcion (dato unHeroe) (dato otroHeroe) = unHeroe
    | otherwise = otroHeroe

sumaDeRarezas :: Heroe -> Int
sumaDeRarezas = sum . map rareza . artefactos

-- 8) Con mi solucion, no terminaría nunca de obtener un ganador ya que estaría constantemente aplicandose la funcion

hacerLabor :: Labor -> Heroe -> Heroe
hacerLabor labor = hacerTarea ( foldr (.) id labor ) 

-- 10) Con mi solucion es imposible obtener un resultado final, ya que primero
-- debe componer todas las tareas de la lista en una sola tarea, para luego aplicarla
-- al heroe, entonces si esta lista es infinita, sería imposible terminar de componer las distintas
-- tareas por hacer, entonces nunca podría finalmente aplicarselas al heroe, sin poder obtener
-- resultado alguno.