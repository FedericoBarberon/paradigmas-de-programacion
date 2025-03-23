--  Ejercicio 2

valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0 = x | otherwise = -x

bisiesto :: Int -> Bool
bisiesto n = mod n 4 == 0 && (mod n 100 /= 0 || mod n 400 == 0)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

divisores :: Int -> [Int]
divisores n = filter (\x -> mod n x == 0) [1 .. n]

esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length (filter esPrimo (divisores n))

-- Ejercicio 3

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (n ^^ (-1))

aEntero :: Either Int Bool -> Int
aEntero (Right bool) = if bool then 1 else 0
aEntero (Left n) = n

-- Ejercicio 4

limpiar :: String -> String -> String
limpiar _ "" = ""
limpiar s1 (c : s2)
  | elem c s1 = limpiar s1 s2
  | otherwise = c : limpiar s1 s2

promedio :: [Float] -> Float
promedio ls = (sum ls) / fromIntegral (length ls)

difPromedio :: [Float] -> [Float]
difPromedio ls = map (\x -> x - promedio ls) ls

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales ls = all (== (ls !! 0)) ls

-- Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq val der) = Bin (negacionAB izq) (not val) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq n der) = n * productoAB izq * productoAB der