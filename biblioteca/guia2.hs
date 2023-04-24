import Distribution.InstalledPackageInfo (InstalledPackageInfo(haddockHTMLs))
siguiente :: Num a => a -> a
siguiente  = (+1)

mitad :: Fractional b => b -> b
mitad = (/2)  

inversa :: Fractional c => c -> c
inversa = (1/)

triple :: Num d => d -> d
triple = (3*)

esNumeroPositivo :: Float -> Bool
esNumeroPositivo = (>0)

esMultiploDe2 :: Int -> Bool
esMultiploDe2 = (==0). rem 2
{-
esBisiesto :: Int -> Bool
esBisiesto = (|| ((==0). rem 400)).((&&((/=0). rem 100)).((==0). rem 4))-}

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa . sqrt

incrementMCuadradoN :: Num g => g -> g -> g
incrementMCuadradoN m = (+m ) .(^2)


esResultadoPar :: Int -> Int -> Bool
esResultadoPar n = esMultiploDe2.(n^)





