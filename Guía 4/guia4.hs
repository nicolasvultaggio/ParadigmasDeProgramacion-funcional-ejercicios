
type Frecuencias = [Frecuencia]
type Frecuencia = Float

frecuenciaCardiaca :: Frecuencias
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

cantidadDeElementos :: Fractional b => [a]-> b
cantidadDeElementos  = foldr (\ head -> (+) 1) 0

promedioFrecuenciaCardiaca :: Fractional a=> [a]-> a
promedioFrecuenciaCardiaca unaLista =  sum unaLista / cantidadDeElementos unaLista

frecuenciaCardiacaMinuto ::[Frecuencia] ->Int-> Float
frecuenciaCardiacaMinuto lista posicionLista = last  . take (posicion posicionLista) $ lista

frecuenciaCardiacaHastaElMomento :: Int -> [Frecuencia]-> [Frecuencia]
frecuenciaCardiacaHastaElMomento posicionLista = take (posicion posicionLista) 


posicion :: Integral a => a -> a
posicion x = div x 10 + 1

esCapicua :: [String] -> Bool
esCapicua lista = (concat . reverse $ lista) == concat lista

ejemplo :: ((String,[Int]),(String, [Int]))
ejemplo =(("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

cuandoHabloMasMinutos :: ((String,[Int]),(String, [Int])) -> String
cuandoHabloMasMinutos ((horarioA,listaA),(horarioB,listaB))
    | sum listaA >= sum listaB = horarioA
    | otherwise = horarioB

cuandoHizoMasLlamadas :: ((String,[Int]),(String, [Int])) -> String
cuandoHizoMasLlamadas ((horarioA,listaA),(horarioB,listaB))
    | length listaA >= length listaB = horarioA
    | otherwise = horarioB



