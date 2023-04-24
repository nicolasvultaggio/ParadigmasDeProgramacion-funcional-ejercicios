import Data.String (IsString)
dispersion :: Int -> Int -> Int -> Int  
dispersion dia1 dia2 dia3  = (max3 dia1 dia2 dia3)-(min3 dia1 dia2 dia3) 

max3 :: Int -> Int -> Int -> Int
max3 num1 num2 num3 = max (max num1 num2) num3

min3 :: Int -> Int -> Int -> Int
min3 num1 num2 num3 = min (min num1 num2) num3

min3' :: Int -> Int -> Int -> Int
min3' num1 num2 = min (min num1 num2) 

diasParejos :: Int -> Int -> Int -> Bool
diasParejos dia1 dia2 dia3 = (30>) . dispersion dia1 dia2 dia3
diasLocos :: Int -> Int -> Int -> Bool
diasLocos dia1 dia2 dia3 = (100<) . dispersion dia1 dia2 dia3
diasNormales :: Int -> Int -> Int -> Bool
diasNormales dia1 dia2 dia3 = not(diasLocos dia1 dia2 dia3) && not(diasParejos dia1 dia2 dia3) 