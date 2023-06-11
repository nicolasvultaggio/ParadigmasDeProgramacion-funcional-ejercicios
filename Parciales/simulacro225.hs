{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
data Participante = UnParticipante {
    nombre :: Nombre,
    trucos :: [Truco],
    especialidad :: Plato
}

type Nombre = String
type Truco = Plato -> Plato
data Plato = UnPlato {
    componentes :: [Componente],
    dificultad :: Int
}

type Componente = (String, Int)


--A

endulzar :: Int ->Truco
endulzar cantAzucar unPlato = unPlato {componentes = ("Azucar" , cantAzucar ) : componentes unPlato }

modificarComponentes :: (Componente->Componente)->Plato->Plato
modificarComponentes funcion unPlato = unPlato {componentes = map funcion (componentes unPlato)}

darSabor:: Int -> Truco
darSabor cantSal = modificarComponentes (agregarSal cantSal)

agregarSal :: Int -> Componente -> Componente
agregarSal cantidadAAgregar ("Sal", cant) = ("Sal", cant + cantidadAAgregar)
agregarSal _ componente = componente

duplicarPorcion :: Truco
duplicarPorcion = modificarComponentes (\(x,y)->(x,2*y))

simplificar:: Truco
simplificar unPlato
    | esComplejo unPlato = unPlato {dificultad = 5 , componentes = filter (cantidadMenorA 10) (componentes unPlato)}
    | otherwise = unPlato

esComplejo :: Plato->Bool
esComplejo unPlato = dificultad unPlato > 7 && length (componentes unPlato) > 5

esVegano :: Plato->Bool
esVegano unPlato = (not . contiene "Carne" $ unPlato) && (not . contiene "Huevos" $ unPlato)

contiene :: String -> Plato -> Bool
contiene unComponente = elem unComponente . map fst . componentes

cantidadMenorA :: Int-> Componente-> Bool
cantidadMenorA cant (_,x) = cant>x

noAptoHipertension :: Plato -> Bool
noAptoHipertension  unPlato
    | not . contiene "Sal" $ unPlato = False
    | otherwise = cantidadMenorA 2 . obtenerSal  . componentes $ unPlato

obtenerSal :: [Componente]->Componente
obtenerSal = head . filter esSal

esSal :: Componente ->Bool
esSal ("Sal",_)= True
esSal (_,_) = False



