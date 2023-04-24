pesoAlto :: Float -> Float
pesoAlto x = (x - min x 3) * 200
pesoBajo :: Float -> Float
pesoBajo x = min x 3 * 300
pesoPino :: Float -> Float
pesoPino x = pesoBajo x + pesoAlto x

esPesoUtil :: Float -> Bool
esPesoUtil x = (400<x) && (x<1000) 

sirvePino :: Float -> Bool
sirvePino = esPesoUtil . pesoPino 



