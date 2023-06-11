data Participante = Participante {
  nombre       :: String,
  trucos       :: [Truco],
  especialidad :: Plato
}

type Truco = Plato -> Plato

data Plato = Plato {
  dificultad  :: Int,
  componentes :: [Componente]
} deriving Show

type Componente = (Ingrediente, Int)
type Ingrediente = String

endulzar :: Int -> Truco
endulzar = agregarComponente "Azucar"

salar :: Int -> Truco
salar = agregarComponente "Sal"

agregarComponente :: String -> Int -> Plato -> Plato
agregarComponente unNombre unosGramos = modificarComponentes (\unosComponentes -> (unNombre, unosGramos) : unosComponentes)
--agregarComponente unNombre unosGramos unPlato = modificarComponentes ((unNombre, unosGramos) :) unPlato

modificarComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
modificarComponentes unaFuncion unPlato = unPlato { componentes = unaFuncion . componentes $ unPlato }

darSabor :: Int -> Int -> Truco
darSabor unosGramosDeSal unosGramosDeAzucar = endulzar unosGramosDeAzucar . salar unosGramosDeSal

duplicarPorcion :: Truco
duplicarPorcion = modificarComponentes $ map duplicarCantidad

duplicarCantidad :: Componente -> Componente
duplicarCantidad (ingrediente, cantidad) = (ingrediente, cantidad * 2)

simplificar :: Truco
simplificar unPlato
  | esComplejo unPlato = modificarComponentes (filter hayMucho) $ unPlato { dificultad = 5 }
-- | esComplejo unPlato = unPlato { dificultad = 5, componentes = filter ((>= 10). snd) (componentes unPlato) }
  | otherwise         = unPlato

hayMucho :: Componente -> Bool
hayMucho unComponente = snd unComponente >= 10
--hayMucho (_, unosGramos) = unosGramos >= 10

cantidadDeComponentes :: Plato -> Int
cantidadDeComponentes = length . componentes

esVegano :: Plato -> Bool
esVegano = not . any esProductoAnimal . componentes

esProductoAnimal :: Componente -> Bool
esProductoAnimal (ingrediente, _) = ingrediente `elem` productosAnimales

productosAnimales :: [Ingrediente]
productosAnimales = ["Leche", "Carne", "Huevo", "Manteca"]

esSinTacc :: Plato -> Bool
esSinTacc = not . tiene "Harina"

tiene :: Ingrediente -> Plato -> Bool
tiene unIngrediente = elem unIngrediente . ingredientes

ingredientes :: Plato -> [Ingrediente]
ingredientes = map fst . componentes

esComplejo :: Plato -> Bool
esComplejo unPlato = dificultad unPlato > 7 && (not . null . drop 5 . componentes) unPlato
--esComplejo unPlato = dificultad unPlato > 7 && ((> 5) . cantidadDeComponentes) unPlato
--f (g (h x))

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = tiene "Sal" unPlato && cantidadDe "Sal" unPlato > 2

cantidadDe :: Ingrediente -> Plato -> Int
cantidadDe unIngrediente = cantidad . conseguirComponente unIngrediente

cantidad :: Componente -> Int
cantidad = snd

conseguirComponente :: Ingrediente -> Plato -> Componente
conseguirComponente unIngrediente = head . filter (esDe unIngrediente) . componentes

esDe :: Ingrediente -> Componente -> Bool
esDe unIngrediente (ingredienteDelComponente, _) = unIngrediente == ingredienteDelComponente

pepeRonccino :: Participante
pepeRonccino = Participante "Pepe Ronccino" [darSabor 2 5, simplificar, duplicarPorcion] unPlatoComplejo

unPlatoComplejo :: Plato
unPlatoComplejo = Plato 10 [("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100)]

-- Parte C

cocinar :: Participante -> Plato
cocinar unParticipante = aplicarTrucos (trucos unParticipante) (especialidad unParticipante)

-- foldl unaFuncion unaSemilla unaLista
-- foldr
aplicarTrucos :: [Truco] -> Plato -> Plato
aplicarTrucos unosTrucos unPlato = foldr aplicarTruco unPlato unosTrucos
--aplicarTrucos unosTrucos unPlato = foldr ($) unPlato unosTrucos
--aplicarTrucos unosTrucos unPlato = foldr (.) id unosTrucos $ unPlato

aplicarTruco :: Truco -> Plato -> Plato
aplicarTruco unTruco = unTruco

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && esMasLigero unPlato otroPlato

esMasDificil :: Plato -> Plato -> Bool
esMasDificil unPlato otroPlato = dificultad unPlato > dificultad otroPlato

esMasLigero :: Plato -> Plato -> Bool
esMasLigero unPlato otroPlato = peso unPlato < peso otroPlato

peso :: Plato -> Int
peso = sum . map cantidad . componentes

participanteEstrella :: [Participante] -> Participante
participanteEstrella = foldr1 mejorParticipante
--foldr1 unaFuncion (cabeza : cola) = foldr unaFuncion cabeza cola

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
  | esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
  | otherwise                                                      = otroParticipante

participanteEstrella' :: [Participante] -> Participante
participanteEstrella' [unParticipante] = unParticipante
--participanteEstrella' [unParticipante, otroParticipante] = mejorParticipante unParticipante otroParticipante
participanteEstrella' (unParticipante : otrosParticipantes) =
  mejorParticipante unParticipante (participanteEstrella' otrosParticipantes)

platinum :: Plato
platinum = Plato 10 unaListaDecomponentesRara

unaListaDecomponentesRara :: [Componente]
unaListaDecomponentesRara =
  map (\unNumero -> ("Ingrediente " ++ show unNumero, unNumero)) [1..]



{-
unPlatoComplejo :: a
unPlatoComplejo = undefined

--PARTE C
aplicarTruco :: Truco -> Plato-> Plato
aplicarTruco truco = truco

cocinar:: Participante -> Plato
cocinar participante = foldr aplicarTruco (especialidad participante) (trucos participante)

pesoTotalDeComponentes :: Plato -> Int
pesoTotalDeComponentes =  sum . map snd . componentes

esMejorQue :: Plato -> Plato -> Bool
esMejorQue platoA platoB = dificultad platoA > dificultad platoB && pesoTotalDeComponentes platoA < pesoTotalDeComponentes platoB

queParticipanteEsMejor :: Participante -> Participante -> Participante
queParticipanteEsMejor participanteA participanteB
  | cocinar participanteA `esMejorQue`cocinar participanteB = participanteA
  | otherwise = participanteB

participanteEstrella:: [Participante] -> Participante
participanteEstrella [x,y] = queParticipanteEsMejor x y
participanteEstrella (x:y:xs) = queParticipanteEsMejor x .  participanteEstrella $ (y:xs)

--Parte D

platinum :: Plato
platinum = Plato 10 [("Integrante 1",1),("Integrante 2",2),("Integrante 3",3)]
-}