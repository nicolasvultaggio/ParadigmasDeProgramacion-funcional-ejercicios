import Distribution.PackageDescription (emptyGenericPackageDescription)
type Titulo = String
type Autor = String
type CantPags = Float
type Libro = ( Titulo, Autor, CantPags )
type Biblioteca = [Libro]
elVisitante :: Libro
elVisitante = ("El visitante","Stephen King",592)
snK1 :: Libro
snK1 =("Shingeki no Kyojin 1","Hajime Isayama",40)
snk3 :: Libro
snk3 = ("Shingeki no Kyojin 3","Hajime Isayama",40)
snk127 :: Libro
snk127 = ("Shingeki no Kyojin 127","Hajime Isayama",40)
fundacion :: Libro
fundacion = ( "Fundacion" , "Isaac Asimov" , 230)
sandman5 :: Libro 
sandman5 = ( "Sandman 5" , "Neil Gaiman" , 35)
sandman10 :: Libro
sandman10 = ( "Sandman 10" , "Neil Gaiman" , 35)
sandman12 :: Libro
sandman12 = ( "Sandman 12" , "Neil Gaiman" , 35)
eragon :: Libro
eragon = ( "Eragon" , "Christopher Paolini" , 544)
eldest :: Libro
eldest = ( "Eldest" , "Christopher Paolini" , 704)
brisignr :: Libro
brisignr = ( "Brisignr" , "Christopher Paolini" , 700)
legado :: Libro
legado = ( "Legado" , "Christopher Paolini" , 811)

{-Si la biblioteca es fantasiosa, es decir, si tiene algún libro de Christopher Paolini o de Neil Gaiman.-}

esFantasiosa :: Biblioteca -> Bool
esFantasiosa unaBiblioteca = any esLibroFantasioso 

autor :: Libro -> Autor
autor (_, unAutor, _) = unAutor

esLibroFantasioso :: Libro -> Bool
esLibroFantasioso unLibro = esDe "Christopher Paolini" unLibro || esDe "NeilGaiman" unLibro

esDe :: Autor -> Libro -> Bool
esDe unAutor unLibro = ((==unAutor).autor) unLibro
