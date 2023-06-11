{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use $" #-}
import Data.Char (toUpper)
data Barbaro = Barbaro {
    nombre:: String,
    fuerza::Int,
    habilidades ::[Habilidad],
    objetos:: [Objeto]
} 

type Habilidad = String
type Objeto = Barbaro -> Barbaro
dave :: Barbaro
dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

kronk ::Barbaro
kronk = Barbaro "Kronk" 999 ["hornear galletitas","ser scout"] [ espada `cuerda` ardilla]

po::Barbaro
po = Barbaro "Po" 999 ["ser guerrero dragon","ser barbaro barbaro"] [espada, varitasDefectuosas ]

modificarFuerza :: (Int -> Int) -> Barbaro -> Barbaro
modificarFuerza funcion barbaro = barbaro{ fuerza= funcion . fuerza $ barbaro}

agregarHabilidad :: Habilidad->Barbaro->Barbaro
agregarHabilidad habilidad barbaro = barbaro{ habilidades = (:) habilidad . habilidades $ barbaro }

espada :: Objeto
espada = modificarFuerza (*2)

amuletosMisticos :: Habilidad ->Objeto
amuletosMisticos = agregarHabilidad

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro =  agregarHabilidad "HacerMagia" $ barbaro{ objetos = []}

ardilla ::Objeto
ardilla = id

cuerda ::Objeto->Objeto->Objeto
cuerda = (.)

--2)

megafono :: Objeto
megafono unBarbaro = unBarbaro{ habilidades = flip (:) [] . concatMap (map toUpper) . habilidades $ unBarbaro}

megafonoBarbarico :: Objeto
megafonoBarbarico = ardilla `cuerda` megafono

--3)

type Evento = Barbaro -> Bool

barbarosSinPulgares :: [String]
barbarosSinPulgares = ["Faffy","Astro"]

tieneHabilidad :: Barbaro -> Habilidad -> Bool
tieneHabilidad barbaro habilidad = elem habilidad (habilidades barbaro)

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbaro = tieneHabilidad barbaro "Escribir Poesía Atroz" 

cremalleraDelTiempo :: Evento
cremalleraDelTiempo  = flip elem barbarosSinPulgares . nombre 

ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = saqueo unBarbaro || gritoDeGuerra unBarbaro || caligrafia unBarbaro

saqueo :: Barbaro -> Bool
saqueo barbaro = tieneHabilidad barbaro "robar" && ((>80) . fuerza $ barbaro)

gritoDeGuerra::Barbaro->Bool
gritoDeGuerra barbaro = poderGritoDeGuerra barbaro >= 4 * (length . objetos $ barbaro)

poderGritoDeGuerra ::Barbaro -> Int
poderGritoDeGuerra  = sum . map length . habilidades 

caligrafia::Barbaro->Bool
caligrafia  = all esHabilidadcaligrafica . habilidades 

esHabilidadcaligrafica :: Habilidad -> Bool
esHabilidadcaligrafica habilidad = inicialMayuscula habilidad && tiene3Vocales habilidad

inicialMayuscula :: String -> Bool
inicialMayuscula = flip elem ['A','B'..'Z'] . head 

tiene3Vocales :: String ->Bool
tiene3Vocales = (3>).  length .filter esVocal 

esVocal ::Char -> Bool
esVocal char = elem char "AEIOUaeiou"

sobrevivientes :: Evento ->[Barbaro]->[Barbaro]
sobrevivientes = filter 

--4)
--a
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos [x] = [x]
sinRepetidos (x:xs) = quitarSiSeRepite x (sinRepetidos xs)

quitarSiSeRepite :: Eq a => a -> [a] -> [a]
quitarSiSeRepite elemento lista 
    | elem elemento lista =  lista
    | otherwise = elemento : lista

--b

descendiente :: Int ->Barbaro -> Barbaro
descendiente generacion papa = foldr (.) id (objetos papa) $ Barbaro (nombre papa ++ replicate generacion '*')  (fuerza papa) (sinRepetidos . habilidades $ papa) (objetos papa)

descendientes :: Barbaro -> [Barbaro]
descendientes  = iterate (descendiente 1)

--No se podria aplicar sin repetidos sobre la lista de objetos porque las funciones no pueden compararse
--Si se podria sobre el nombre de un barbaro ya que este es un string, o sea una lista de chars, que si pueden compararse entre si