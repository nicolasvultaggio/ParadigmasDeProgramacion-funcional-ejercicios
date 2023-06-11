
data Persona = UnaPersona {
    nombre :: String,
    calorias :: Int,
    hidratacion :: Int,
    tiempoDeEntrenamiento :: Int,
    equipamientos :: [Equipamiento]
}

type Ejercicio = Persona -> Persona
type Repeticiones = Int
type Peso  = Int
type Equipamiento = String
type Accion = Persona -> Persona

modificarHidratacion ::(Int->Int)->Persona -> Persona
modificarHidratacion  unaFuncion unaPersona = unaPersona { hidratacion = unaFuncion . hidratacion  $ unaPersona   }

modificarCalorias ::(Int->Int)->Persona -> Persona
modificarCalorias unaFuncion unaPersona = unaPersona { calorias = unaFuncion . calorias $ unaPersona }

abdominales :: Repeticiones -> Ejercicio
abdominales repeticiones  = modificarCalorias (flip (-) (repeticiones * 8))

flexiones :: Repeticiones ->Ejercicio
flexiones repeticiones = modificarCalorias (flip (-) (repeticiones * 16))  . modificarHidratacion (flip (-) (2 * cadaXRepeticiones repeticiones 10) )

tieneEquipamiento :: Equipamiento-> Persona -> Bool
tieneEquipamiento elemento = elem elemento . equipamientos

levantarPesas :: Repeticiones -> Peso -> Ejercicio
levantarPesas repeticiones peso unaPersona
    | tieneEquipamiento "pesa" unaPersona = modificarCalorias (flip (-) (repeticiones*32)) . modificarHidratacion  (flip (-) ( peso * cadaXRepeticiones repeticiones 10))  $ unaPersona
    | otherwise = unaPersona

cadaXRepeticiones:: Int->Int->Int
cadaXRepeticiones = div

laGranHomeroSimpson :: Ejercicio
laGranHomeroSimpson = id

renovarEquipo :: Accion
renovarEquipo unaPersona = unaPersona { equipamientos =  map ("Nuevo " ++) . equipamientos $ unaPersona}

volverseYoguista :: Accion
volverseYoguista unaPersona = modificarCalorias (2 `div`) . modificarHidratacion duplicarHidratacion $ unaPersona {equipamientos = ["Colchoneta"]}

duplicarHidratacion :: Int->Int
duplicarHidratacion numero
    | numero <= 50 = 2*numero
    | otherwise = 100

volverseBodyBuilder :: Accion
volverseBodyBuilder unaPersona
    | all (=="pesa"). equipamientos $ unaPersona = modificarCalorias (*3) . modificarHidratacion id $ unaPersona{ nombre = (++" BB"). nombre $ unaPersona}
    | otherwise = unaPersona

comerUnSandwich ::Accion
comerUnSandwich unaPersona = modificarCalorias (+500) $ unaPersona{ hidratacion =100}

data Rutina = Rutina {
    tiempo :: Int,
    ejercicios :: [Ejercicio]
}

puedeHacerRutina :: Rutina -> Persona -> Bool
puedeHacerRutina rutina persona = tiempo rutina > tiempoDeEntrenamiento persona

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina rutina persona
    | puedeHacerRutina rutina persona = foldr ($) persona (ejercicios rutina)
    | otherwise = persona

estaAgotada :: Persona -> Bool
estaAgotada persona = calorias persona < 50 && hidratacion persona<10

esPeligrosa :: Rutina -> Persona ->Bool
esPeligrosa rutina = estaAgotada . hacerRutina rutina

esBalanceada :: Rutina -> Persona ->Bool
esBalanceada rutina persona =  ((>80) . hidratacion . hacerRutina rutina $ persona) &&  (calorias . hacerRutina rutina $ persona) < div (calorias persona) 2

elAbominableAbdominal :: Rutina
elAbominableAbdominal = Rutina 60 (iterate ($) (abdominales 1))
--suponiendo que el tiempo es en minutos

seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio persona  = filter (puedeEntrenarCon persona)

puedeEntrenarCon :: Persona -> Persona -> Bool
puedeEntrenarCon unaPersona otraPersona = tiempoDeEntrenamiento unaPersona == tiempoDeEntrenamiento otraPersona

promedioRutina :: Rutina -> [Persona] -> (Int, Int)
promedioRutina rutina grupo = ( promedio calorias rutina grupo , promedio hidratacion rutina grupo)

promedio :: (Persona -> Int) -> Rutina -> [Persona] -> Int
promedio dato rutina grupo = div (sum.map (dato.hacerRutina rutina) $ grupo) (length grupo)