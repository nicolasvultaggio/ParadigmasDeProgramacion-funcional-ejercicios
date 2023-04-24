import Text.Show.Functions ()
data Jugador = UnJugador {
    nombre :: String,
    dinero :: Int ,
    tactica :: String ,
    propiedades :: [Propiedad],
    acciones :: [Accion]
} deriving Show

type Propiedad = (NombreProp,PrecioProp)
type NombreProp = String
type PrecioProp = Int
type Accion = Jugador -> Jugador

pasarPorElBanco:: Jugador->Jugador
pasarPorElBanco = agregarPlata 40 . cambiarTactica "Comprador compulsivo"

cambiarTactica :: String -> Jugador -> Jugador
cambiarTactica unaTactica unJugador = unJugador { tactica = unaTactica }

enojarse :: Accion -- enojarse:: Jugador -> Jugador
enojarse = agregarPlata 50 . agregarAccion gritar

gritar :: Accion
gritar unJugador = unJugador { nombre = "AHHHH" ++ nombre unJugador}

agregarPlata :: Int -> Jugador -> Jugador
agregarPlata unaCantidad alguienQueJuega = alguienQueJuega { dinero = dinero alguienQueJuega + unaCantidad }

agregarAccion :: Accion -> Jugador -> Jugador
agregarAccion unaAccion unJugador = unJugador{ acciones = unaAccion : acciones unJugador  }

{-subastar: al momento de una subasta solo quienes tengan como tácticas 
“Oferente singular” o “Accionista” podrán ganar la propiedad. Ganar implica restar el 
precio de la propiedad de su dinero y sumar la nueva adquisición a sus propiedades. -}

esTactica :: String -> Jugador -> Bool
esTactica unaTactica unJugador = tactica unJugador == unaTactica

subastar :: Propiedad -> Int ->Jugador -> Jugador
subastar propiedadAComprar precio unJugador
    | puedeSubastar unJugador = (sumarAdquisicion propiedadAComprar . quitarPlata precio) unJugador
    | otherwise = unJugador

sumarAdquisicion :: Propiedad -> Jugador -> Jugador
sumarAdquisicion propiedadNueva unJugador = unJugador { propiedades = propiedadNueva : propiedades unJugador  }

quitarPlata :: Int -> Jugador -> Jugador
quitarPlata unaCantidad alguienQueJuega = alguienQueJuega { dinero = dinero alguienQueJuega - unaCantidad }

puedeSubastar:: Jugador -> Bool
puedeSubastar unJugador = esTactica "Accionista" unJugador || esTactica "Oferente Singular" unJugador

type ListaDePropiedades = [Propiedad]
type PrecioAlquiler = Int

cobrarAlquileres:: Accion
cobrarAlquileres unJugador = unJugador{ dinero = dinero unJugador + conteoDePrecios (propiedades unJugador) }

conteoDePrecios :: ListaDePropiedades -> Int
conteoDePrecios  = sum . map barataOCara

barataOCara :: Propiedad -> PrecioAlquiler
barataOCara (_,precio)
    | precio >= 150 = 20
    | otherwise = 10

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | tactica unJugador == "Accionista" = agregarPlata 200 unJugador
    | otherwise = quitarPlata 100 unJugador

carolina :: Jugador
carolina = UnJugador { nombre = "Carolina",dinero =500 , tactica = "Accionista",  propiedades = [] ,acciones = [pasarPorElBanco] }

manuel :: Jugador
manuel = UnJugador { nombre = "Manuel",dinero =500 , tactica = "Oferente Singular",  propiedades = [] ,acciones = [enojarse] }

