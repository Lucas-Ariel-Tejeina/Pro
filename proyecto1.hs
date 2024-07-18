--1a)
--Esta funcion busca saber si un entero dado es igual o no a 0.
esCero :: Int -> Bool
esCero x = (x == 0)
{-
*Main> esCero 4
False
*Main> esCero 1
False
*Main> esCero 0
True
-}

--1b)
--Esta funcion busca verifica si un entero es estrictamente mayor a 0.
esPositivo :: Int -> Bool
esPositivo x = (x > 0)
{-
*Main> esPositivo (-1)
False
*Main> esPositivo 0
False
*Main> esPositivo 1
True
-}

--1c)
--Esta funcion verifica si un caracter es una vocal en min ́uscula.
esVocal :: Char -> Bool
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'
{-
*Main> esVocal 'a'
True
*Main> esVocal 'e'
True
*Main> esVocal 'i'
True
*Main> esVocal 'o'
True
*Main> esVocal 'u'
True
*Main> esVocal 'A'
False
-}

--1d)
--Esta funcion que devuelve el valor absoluto de un entero ingresado.
valorAbsoluto :: Int -> Int
valorAbsoluto x | esPositivo x = x
                | x <= 0 = -x   
{-
*Main> valorAbsoluto 1
1
*Main> valorAbsoluto (-1)
1
*Main> valorAbsoluto 0
0
-}

-----------------------------------------------------------------------------------------

--2a)
--Esta funcion verifica que todos los elementos de una lista sean True.
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs
{-
*Main> paratodo [True,True,True,True]
True
*Main> paratodo [True,True,True,False]
False
-}

--2b)
--Esta funcion calcula la suma de todos los elementos de una lista de enteros.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
{-
*Main> sumatoria [1,2,3,4,5]
15
*Main> sumatoria [1,2,3,4,-1,-2,-3,-4]
0
-}

--2c)
--Esta funcion alcula el producto de todos los elementos de la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs
{-
*Main> productoria [2,6,5,4]
240
*Main> productoria [2,6,5,4,2]
480
-}

--2d)
--Esta funcion toma un n ́umero n y calcula n!.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
{-
*Main> factorial 4
24
*Main> factorial 5
120
*Main> factorial 0
1
-}

--2e)
--Esta funcion toma una lista de n ́umeros no vacia y calcula el valor promedio (truncado, usando divisi ́on entera).
contador :: [Int] -> Int
contador [] = 0
contador (x:xs) = 1 + contador xs

promedio :: [Int] -> Int
promedio xs = quot (sumatoria xs) (contador xs)
{-
*Main> promedio [1,1,1,1,1]
1
*Main> promedio [1,2,3,4,5]
3
-}

---------------------------------------------------------------------------------------------------------------------

--3)
--Esta funcion verifica si un numero se encuentra en una lista.
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (n == x) || pertenece n xs

pertenecE :: Int -> [Int] -> Bool
pertenecE n [] = False
pertenecE n (x:xs) | x == n = True
                   | x /= n = pertenecE n xs  
{-
*Main> pertenece 4 [2,4,6]
True
*Main> pertenece 6 [2,4,6]
True
*Main> pertenece 7 [2,4,6]
False
-}

--------------------------------------------------------------------------------------------------

-- 4a)
-- Esta funcion determina si todos los elementos de xs satisfacen el predicado t.
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] f = True
paratodo' (x:xs) f = f x && paratodo' xs f
{-
*Main> paratodo' [1,2,3,4,5] esPositivo 
True
*Main> paratodo' [1,2,3,4,-5] esPositivo 
False

*Main> paratodo' ['a','e','i','o','u'] esVocal 
True
*Main> paratodo' ['a','e','i','o','p'] esVocal 
False
-}

-- 4b)
-- Esta funcion determina si alg ́un elemento de xs satisface el predicado t.
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f = f x || existe' xs f
{-
*Main> existe' [-1,-2,-3,-4,-5] esPositivo 
False
*Main> existe' [-1,-2,-3,-4,5] esPositivo 
True
*Main> existe' ['x','b','c','d','f'] esVocal
False
*Main> existe' ['x','b','c','d','a'] esVocal
True
-}

-- 4c)
-- Esta funcion dada una lista xs de tipo [a] y una funci ́on t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicaci ́on de t a los elementos de xs.
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + sumatoria' xs f
{-
*Main> sumatoria' [1,2,3,4] valorAbsoluto 
10
*Main> sumatoria' [1,2,-3,-4] valorAbsoluto 
10
*Main> sumatoria' [1,2,3,4] factorial 
33
-}

-- 4d)
-- Esta funcion dada una lista de xs de tipo [a] y una funci ́on t :: a -> Int, calcula el producto de los valores que resultan de la aplicaci ́on de t a los elementos de xs.
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = f x * productoria' xs f
{-
*Main> productoria' [1,2,3,4,5] factorial 
34560
*Main> productoria' [-1,2,-3,4,-5] valorAbsoluto  
120
-}

-----------------------------------------------------------------------------------------------------------------------------------------------------

-- 5)
-- 
paratodo'' :: [Bool] -> Bool
paratodo'' [] = True
paratodo'' xs = paratodo' xs id
{-
*Main> paratodo [True,True,True,True]
True
*Main> paratodo [True,True,True,False]
False
-}

---------------------------------------------------------------------------------------------------

-- 6a)
-- Esta funcion verifica que todos los n ́umeros de una lista sean pares.

esPar :: Int -> Bool
esPar x = mod x 2 == 0

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs esPar
{-
*Main> todosPares [0,2,4,6]
True
*Main> todosPares [0,2,4,7]
False
-}

-- 6b)
-- Esta funcion verifica si existe alg ́un n ́umero dentro del segundo par ́ametro que sea m ́ultiplo del primer par ́ametro.

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo m xs = existe' xs (\x -> mod x m == 0)
{-
*Main> hayMultiplo 2 [1,3,5,7,9]
False
*Main> hayMultiplo 2 [1,3,5,7,8]
True
-}

-- 6c)
-- Esta funcion calcula la suma de los primeros n cuadrados.

sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [1..x] (\x -> x*x)
{-
*Main> sumaCuadrados 3
14
*Main> sumaCuadrados 4
30
-}

-- 6d)
-- Esta funcion que dado en entero n y una lista ls , devuelve True si y solo si, existe alg ́un elemento en ls que divida a n.

existeDivisor :: Int-> [Int] -> Bool
existeDivisor n xs = existe' xs (\x -> mod n x == 0)
{-
*Main> existeDivisor 7 [1,2,3,4,5]
True
*Main> existeDivisor 7 [11,2,3,4,5]
False
-}

-- 6e)
-- Esta funcion dado un entero n, devuelve True si y solo si n es primo.
esPrimo:: Int -> Bool
esPrimo n = n > 1 && not(existeDivisor n [2..n-1])
{-
*Main> esPrimo 1
False
*Main> esPrimo 2
True
*Main> esPrimo 3
True
*Main> esPrimo 4
False
*Main> esPrimo 5
True
*Main> esPrimo 6
False
*Main> esPrimo 7
True
*Main> esPrimo 8
False
*Main> esPrimo 9
False
*Main> esPrimo 10
False
*Main> esPrimo 11
True
*Main> esPrimo 12
False
*Main> esPrimo 13
True
-}

-- 6f)
-- ¿Se te ocurre como redefinir factorial (ej. 2d ) para evitar usar recursi ́on?
factorial' :: Int -> Int
factorial' x = productoria' [1..x] id
{-
*Main> factorial' 3
6
*Main> factorial' 4
24
*Main> factorial' 5
120
*Main> factorial' 6
720
-}

-- 6g)
-- Esta funcion calcula el producto de todos los n ́umeros primos de una lista.
multiplicaPrimos :: [Int] -> Int
multiplicaPrimos xs = productoria' [m | m <- xs, esPrimo m] id 
{-
*Main> multiplicaPrimos [1,2,3,4,5]
30
*Main> multiplicaPrimos [1,2,3,4,5,6,8,9]
30
-}

-- 6h)
-- Es una funcion que dado un entero n, devuelve True si y s ́olo si n est ́a en la sucesi ́on de Fibonacci.

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

listfib :: Int -> [Int]
listfib 0 = []
listfib x = fib x : listfib (x-1)

esFib :: Int -> Bool
esFib n = pertenece n (listfib n) || n == 0
{-
*Main> esFib 0
True
*Main> esFib 1
True
*Main> esFib 2
True
*Main> esFib 3
True
*Main> esFib 4
False
*Main> esFib 5
True
*Main> esFib 6
False
*Main> esFib 7
False
*Main> esFib 8
True
-}

-- 6i)
-- En esta funcion que dada una lista xs de enteros, devuelva si todos los elementos de la lista pertenecen (o no) a la sucesi ́on de Fibonacci.
todosFib :: [Int] -> Bool
todosFib xs = paratodo' xs esFib
{-
*Main> todosFib [0,1,2,3,5]
True
*Main> todosFib [0,1,2,3,4,5]
False
-}

------------------------------------------------------------------------------------------------------

-- 7)
-- Indag ́a en Hoogle sobre las funciones map y filter. Tambi ́en podes consultar su tipo en ghci con el comando :t.
-- ¿Que hacen estas funciones?
{-
La funcion map :: (a -> b) -> [a] -> [b], toma una lista xs y una funcion f, donde el resultado es una lista xs' donde a cada elemento de xs se le aplica la funcion f.
La funcion filter :: (a -> Bool) -> [a] -> [a], toma un predicado p y una lista xs, donde el resultado es una lista xs' que contiene los elementos que satisface el predicado p.
-}
-- ¿A qu ́e equivale la expresi ́on map succ [1, -4, 6, 2, -8], donde succ n = n+1?
{-
Por definicion de map, aplica a la lista a todos los elementos de la lista [1, -4, 6, 2, -8] la funcion succ n = n+1
por lo tanto el resultado [2, -3, 7, 3, -7]
-}
-- ¿Y la expresi ́on filter esPositivo [1, -4, 6, 2, -8]?
{-
Por definicion de filter, aplica la propocicion esPositivo a todos los elementos de [1, -4, 6, 2, -8] y devuelve una lista que satisface el predicado esPositivo [1,6,2].
-}

--------------------------------------------------------------------------------------------------------------------------------------

-- 8)
-- Program ́a una funci ́on que dada una lista de n ́umeros xs, devuelve la lista que resulta de duplicar cada valor de xs.

-- a) Definila usando recursion.
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = 2*x : duplica xs
{-
*Main> duplica [1,-8,2,-3,7]
[2,-16,4,-6,14]
*Main> duplica [1,-8,27,13,0]
[2,-16,54,26,0]
-}

-- b) Definila utilizando la funci ́on map.
duplica' :: [Int] -> [Int]
duplica' xs = map (2*) xs
{-
*Main> duplica' [1,-8,2,-3,7]
[2,-16,4,-6,14]
*Main> duplica' [1,-8,27,13,0]
[2,-16,54,26,0]
-}

------------------------------------------------------------------------------------------------------

-- 9)
-- Program ́a una funci ́on que dada una lista de n ́umeros xs, calcula una lista que tiene como elementos aquellos n ́umeros de xs que son primos.

-- a) Definila usando recursi ́on.
listP :: [Int] -> [Int]
listP [] = []
listP (x:xs) | esPrimo x = x : listP xs
             | not(esPrimo x) = listP xs
{-
*Main> listP [1,2,3,4,5]
[2,3,5]
*Main> listP [1,2,3,4,5,7,8,9,10,11]
[2,3,5,7,11]
-}

-- b) Definila utilizando la funci ́on filter.
listP' :: [Int] -> [Int]
listP' xs = filter esPrimo xs
{-
*Main> listP' [1,2,3,4,5]
[2,3,5]
*Main> listP' [1,2,3,4,5,7,8,9,10,11]
[2,3,5,7,11]
-}

-- c)
-- Revis ́a tu definici ́on del ejercicio 6g . ¿C ́omo podes mejorarla?
-- Programar la funci ́on multiplicaPrimos :: [Int] -> Int que calcula el producto de todos los n ́umeros primos de una lista.
multiplicaPrimos' :: [Int] -> Int
multiplicaPrimos' xs = productoria' (listP' xs) id
{-
*Main> multiplicaPrimos' [1,2,3,4,5]
30
*Main> multiplicaPrimos' [1,2,3,4,5,6,8,9]
30
-}

----------------------------------------------------------------------------------------------------------------------

-- 10)La funci ́on primIgualesA toma un valor y una lista, y calcula el tramo inicial m ́as largo de la lista cuyos elementos son iguales a ese valor.

-- a)Program ́a primIgualesA por recursi ́on.
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | n == x = x : primIgualesA n xs
                      | n /= x = []
{-
*Main> primIgualesA 3 [3,3,4,1]
[3,3]
*Main> primIgualesA 3 [4,3,3,4,1]
[]
*Main> primIgualesA 3 []
[]
*Main> primIgualesA 'a' "aaadaa"
"aaa"
-}

-- b) Program ́a nuevamente la funci ́on utilizando takeWhile.
{-
takeWhile Toma una preposicion y una lista, devolviendo los elementos de esa lista mientras que la proposicion sea verdadera.
-}
primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n xs = takeWhile (n ==) xs
{-
*Main> primIgualesA' 3 [3,3,4,1]
[3,3]
*Main> primIgualesA' 3 [4,3,3,4,1]
[]
*Main> primIgualesA' 3 []
[]
*Main> primIgualesA' 'a' "aaadaa"
"aaa"
-}

-------------------------------------------------------------------------------------------------------

-- 11) La funci ́on primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre s ́ı.

-- a) Program ́a primIguales por recursi ́on.
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales (x:y:xs) | x == y = x : primIguales (y:xs) 
                     | x /= y = [x]  
{-
*Main> primIguales [3,3,4,1]
[3,3]
*Main> primIguales [4,3,3,4,1] 
[4]
*Main> primIguales []
[]
*Main> primIguales "aaadaa"
"aaa"
-}

-- b) Us ́a cualquier versi ́on de primIgualesA para programar primIguales. Est ́a permitido dividir en casos, pero no usar recursi ́on.

primIguales' :: Eq a => [a] -> [a]
primIguales' [] = []
primIguales' [x] = [x]
primIguales' (x:y:xs) = primIgualesA' x (x:y:xs)
{-
*Main> primIguales' [3,3,4,1]
[3,3]
*Main> primIguales' [4,3,3,4,1]
[4]
*Main> primIguales' []
[]
*Main> primIguales "aaadaa"
"aaa"
-}

--------------------------------------------------------------------------------------------------------

--  12)
-- 

cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t = t x `op` cuantGen op z xs t

-- a) 
paraTodo :: [a] -> (a -> Bool) -> Bool
paraTodo xs t = cuantGen (&&) True xs t
{-
*Main> paraTodo [1,2,3,4,5] esPositivo
True
*Main> paraTodo [1,2,3,4,-5] esPositivo
False
-}

-- b)
eXiste :: [a] -> (a -> Bool) -> Bool
eXiste xs t = cuantGen (||) False xs t
{-
*Main> eXiste [-1,-2,-3,-4,-5] esPositivo
False
*Main> eXiste [-1,-2,-3,-4,5] esPositivo
True
*Main> eXiste ['x','b','c','d','f'] esVocal
False
*Main> eXiste ['x','b','c','d','a'] esVocal
True
-}

-- c)
sumaToria :: [a] -> (a -> Int) -> Int
sumaToria xs f = cuantGen (+) 0 xs f
{-
*Main> sumaToria [1,2,3,4] valorAbsoluto
10
*Main> sumaToria [1,2,-3,-4] valorAbsoluto
10
*Main> sumaToria [1,2,3,4] factorial
33
-}

-- d)
producToria :: [a] -> (a -> Int) -> Int
producToria xs f = cuantGen (*) 1 xs f
{-
*Main> producToria [1,2,3,4,5] factorial
34560
*Main> producToria [-1,2,-3,4,-5] valorAbsoluto
120
-}

-----------------------------------------------------------------------------------------------------

-- 13)
distanciaEdicion::[Char]->[Char]-> Int
distanciaEdicion [] xs = length xs
distanciaEdicion xs [] = length xs
distanciaEdicion (x:xs) (y:ys) | x == y = distanciaEdicion xs ys
                               | x /= y = 1 + distanciaEdicion xs ys
{-
*Main> distanciaEdicion "hola" []
4
*Main> distanciaEdicion [] "hola"
4
*Main> distanciaEdicion "holaa" "hola"
1
-}                               

------------------------------------------------------------------------------------------------

-- 14)
primQueCumplen :: [a]->(a->Bool)->[a]
primQueCumplen xs f = takeWhile f xs

{-
*Main> primQueCumplen [1,2,3,4,5,6,7,8,9] (>5)
[]
*Main> primQueCumplen [1,2,3,4,5,6,7,8,9] (<5)
[1,2,3,4]

*Main> primQueCumplen "qaeioup" esVocal 
""
*Main> primQueCumplen "aeioup" esVocal 
"aeiou"
-}

-------------------------------------------------------------------------------------------------------

-- 15)
---a) f :: (a, b) -> ...
--   f (x , y) = ...
{-La función está bien tipado, ya que f :: (a, b) -> ... está definida como una funcion 
que toma una tupla donde su primera componente de la tupla es de tipo a y la segunda componente
es de tipo b y la función f toma una tupla (x , y) donde 'x' es de tipo a e 'y' es de b, por
lo tanto no hay problemas con la definición.
Abarca todos los casos
f :: (a, b) -> a
f (x,y) = x
-}

--b) f :: [(a, b)] -> ...
--   f (a , b) = ...
{- La función está bien tipada pero mal definida, ya que f :: [(a, b)] -> ... toma una 
lista de tuplas, donde la primera componente de la tupla es de tipo a y la segunda componente
de la tupla es de tipo b. Y la funcion está definida como f (a , b) siendo que f toma una 
tupla y no una lista de tuplas, por lo tanto hay problemas de definición.
-}
--c) f :: [(a, b)] -> ...
--   f (x:xs) = ...
{-- La funcion está bien tipada ya que f :: [(a, b)] -> ... toma una lista de tuplas, donde
la primera componente de la tupla es de tipo a y la segunda componente es de tipo b, y 
f (x:xs) toma como x = (a, b) como primer elemento de xs. 
Solo que no abarca todos los casos ya que no sabemos que sucede si f toma la lista vacia.

f :: [(a,b)] -> [(a,b)]
f ((x,y):xs) = (x,y):xs 

Básicamente no hace nada como la identidad.
-}

--d) f :: [(a, b)] -> ...
--   f ((x, y) : ((a, b) : xs)) = ...
{-La funcion está bien tipada ya que f :: [(a, b)] -> ... toma una lista de tuplas donde
la primera componente de la tupla es de tipo a y la segunda componente es de tipo b, y 
f ((x, y) : ((a, b) : xs)) = ... donde (x, y) es el primer elemento de la lista xs y 
(a, b) es el segundo elemento de la lista, donde 'x' y 'a' son de tipo a y, 'b' e 'y' son
de tipo b. 
En sintesis, la funcion esta bien definida para listas de tuplas que tengan como minimos 2 
elementos, pero no abarca todos ya que no sabemos que pasa cuando toma una lista vacia o 
una lista con un solo elemento.

f :: [(a,b)] -> [(a,b)]
f ((x,y):(e,t):xs) = (e,y):(x,t):xs 

Intercambia la primera componente de los dos primeros elementos.
-}

--e) f :: [(Int, a)] -> ...
--   f [(0, a)] = ...
{-Está bien tipada, ya que f :: [(Int, a)] -> ... toma una lista de tuplas donde el primer
elemento de la tupla es de tipo int y el segundo elemento es de tipo a, y f [(0, a)] 
donde f toma una lista de un solo elemento, donde la primera componente de la tupla de tipo 
int, ya que 0 es Int o Integer, y la segunda componente es de tipo a. 
Ademas esta funcion no abarca todos los casos, ya que no sabemos que pasa cuando la funcion 
toma una lista vacia o una lista con mas de un elemento o cuando el primer elemento de la 
tupla no es 0.

f :: [(Int, a)] -> [(Int,a)]
   f [(0, x)] = (1,x):xs
Cambia la primera componente del primer elemento.
-}

--f ) f :: [(Int, a)] -> ...
--    f ((x, 1) : xs) = ...
{-
f es una funcion que esta mal tipada, ya que en su definicion es una funcion que toma una 
lista de tuplas donde su primera componente es de tipo Int y su segunda componente es de 
tipo a, cuando se escribe f ((x, 1) : xs) = ... donde 'x' es de tipo Int y 1 es de tipo 'a'
, pero como es una funcion polimorfica por 'a', si ejecutamos f((x,'asd'):xs) = tendriamos un error
ya que la funcion lo permite por definicion pero faltan casos de analisis necesarios para no
podrucir errores, ya sea como f [] o f de una lista con un solo elemento.
-}
--g) f :: (Int -> Int) -> Int -> ...
--   f a b = ...
{-Está bien tipada ya que f :: (Int -> Int) -> Int -> ... y f a b = ... donde a es la 
funcion (Int -> Int) y b un Int y cubre todos los casos.

f :: (Int -> Int) -> Int -> Int
f a b = (a b) + 1
Básicamente te devuelve el siguiente de aplicar a b.
-}
--h) f :: (Int -> Int) -> Int -> ...
--   f a 3 = ...
{-Está bien tipada ya que es un caso del inciso g) f :: (Int -> Int) -> Int -> ... y 
f a 3 = ... donde a es la funcion (Int -> Int) y 3 un Int, 
solo que no subre todos los casos, ya que no sabemos que sucede si ponemos cualquier otro
numero que no sea el 3 en f a 3 = ... .

Como es un caso del inciso g),

f :: (Int -> Int) -> Int -> Int
f a 3 = (a 3) + 1
-}
--i) f :: (Int -> Int) -> Int -> ...
-- f 0 1 2 = ...
{-No esta bien tipada ya que f :: (Int -> Int) -> Int -> ... esta definida por una 
funcion (Int -> Int) y un Int que en total son 2 argumentos, en cambio a f le damos 
f 0 1 2 = ... 3 argumentos de tipo Int y ninguna funcion.
-}
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--16. (*) Para las siguientes declaraciones de funciones, da al menos una definici´on cuando sea posible. ¿Pod´es dar alguna otra definici´on alternativa a la que diste en cada caso?
-- Cuando no están definido los tipos de una funcion solo podemos ejecutar una funcion de caracter id, que dado un elemento me vuelve el mismo elemento.
--a) f :: (a, b) -> b
{-Es posible dar una definición-}
f :: (a, b) -> b
f (x,y) = y

--b) f :: (a, b) -> c
{-No es posible dar una definición precisa de esta declaración.-}

--c) f :: (a -> b) -> a -> b
{-Es posible dar una definición-}
g :: (a -> b) -> a -> b  
g q a = q a 

--d) f :: (a -> b) -> [a] -> [b]
{-Es posible ya que si definimos la función-}
e :: (a -> b) -> [a]-> [b]  
e q [] = []
e q (a:as) = q a : e q as
