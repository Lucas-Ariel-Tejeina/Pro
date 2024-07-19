-- Ejercicio 1)
{-
a) Definir el tipo Ropa que consta de los constructores Camisa, Pantalon, Pollera,
Short. Los constructores no toman parámetros. El tipo Ropa no debe estar en la clase
Eq. Luego programa la función usando pattern matching:
misma_ropa :: Ropa -> Ropa -> Bool
que dados dos valores p1 y p2 del tipo Ropa debe devolver True cuando p1 y p2
son la misma ropa (se construyen con el mismo constructor) y False en caso contrario.
Si se usan más de cinco casos, o menos de cinco casos este apartado sumará menos
puntaje.
-}
data Ropa = Camisa | Pantalon | Pollera | Short

misma_ropa :: Ropa -> Ropa -> Bool
misma_ropa Camisa Camisa = True
misma_ropa Pantalon Pantalon = True
misma_ropa Pollera Pollera = True
misma_ropa Short Short = True
misma_ropa _ _ = False

{-
b) Definir el tipo Prenda que representa una prenda de ropa. Tiene constructores:
●
●
Constructor ConTalle: Toma dos parámetros, el primero de tipo Talle y el
segundo de tipo Ropa
Constructor TalleUnico: Toma un sólo parámetro de tipo Ropa
El tipo Talle debe ser un sinónimo del tipo Int.
-}
type Talle = Int
data Prenda = ConTalle Talle Ropa | TalleUnico Ropa
 
{-
c) Programar la función
valor_talle :: Prenda -> Int
teniendo en cuenta que el valor de una prenda será:
●
●
Si es una prenda con talle : Su valor es el talle de la prenda.
Si es una prenda talle único : Su valor es 0.
-}
valor_talle :: Prenda -> Int
valor_talle (ConTalle a _) = a
valor_talle (TalleUnico _) = 0
{-
*Main> valor_talle (ConTalle 19 Camisa)
19
*Main> valor_talle (TalleUnico  Camisa)
0
-}

{-
d) Incluir el tipo Prenda en la clase Ord de manera tal que una prenda se considere
mayor que otra si su valor según la función valor_talle es más grande.
-}
{-
-- RTA
Primero debemos estrablecer una igualdad cuando los valores de valor_talle sean iguales, para
luego establecer una comparacion de orden.
-}
instance Eq Prenda 
    where
        (==) prenda1 prenda2 = valor_talle (prenda1) == valor_talle (prenda2)
-- Ahora si podemos establecer una comparacion 
instance Ord Prenda
    where 
        (<=) prenda1 prenda2 = valor_talle (prenda1) <= valor_talle (prenda2)
{-
*Main> ConTalle 16 Camisa == ConTalle 16 Pantalon
True
*Main> ConTalle 16 Camisa == TalleUnico Pantalon
False
*Main> ConTalle 16 Camisa <= TalleUnico Pantalon
False
*Main> ConTalle 16 Camisa >= TalleUnico Pantalon
True
-}

---------------------------------------------------------------------------------------------------

-- Ejercicio 2)
{-
a) Programar de manera recursiva la función
solo_con_talle :: [Prenda] -> Ropa -> [Talle]
que dada una lista de prendas ps y una ropa r devuelve una lista con los números de los
talles de las prendas con talle (las que no son talle único) de ps que son de la ropa r.
-}
solo_con_talle :: [Prenda] -> Ropa -> [Talle]
solo_con_talle [] _ = []
solo_con_talle (p:ps) a = case p of
                            (ConTalle t r)| misma_ropa r a -> t : solo_con_talle ps a
                            _-> solo_con_talle ps a
{-
b) Escribir una lista de prendas con al menos tres elementos, donde al menos uno de ellos
debe tener talle, y otro debe ser talle único.
[(ConTalle 19 Camisa),(ConTalle 36 Pantalon),(TalleUnico Short),(ConTalle 15 Camisa)]
-}

{-
c) Escribir el resultado de solo_con_talle para la lista del punto b)
*Main> solo_con_talle [(ConTalle 19 Camisa),(ConTalle 36 Pantalon),(TalleUnico Short),(ConTalle 15 Camisa)] Camisa
[19,15]
-}

-------------------------------------------------------------------------------------------------------------------------

-- Ejercicio 3)
{-
Basados en el tipo ListaAsoc del Proyecto 2, programar la función:
la_duplica_pares :: ListaAsoc a b -> ListaAsoc a b
que dada una lista de asociaciones la, devuelve una nueva lista de asociaciones con las
asociaciones de la cuyos valores son : Si la clave es par el valor será
multiplicado por 2, y si la clave es impar permanecerá el valor
que tenía. Completar el tipado de la función para incluir los type clases necesarios para
programarla.
-}

data ListaAsoc a b = VaciaC | Nodo a b (ListaAsoc a b) deriving Show
-- Agregamos la propiedad Show a ListaAsoc ya que queremos ser capaces de mostrar elementos de este tipo.

la_duplica_pares ::(Integral a, Num b) => ListaAsoc a b -> ListaAsoc a b
la_duplica_pares VaciaC = VaciaC
la_duplica_pares (Nodo a b c) = case a of
                                _| even a -> Nodo a (2*b) (la_duplica_pares c)
                                _-> la_duplica_pares c
{-
*Main> la_duplica_pares (Nodo 4 15 (Nodo 5 47 (Nodo 2 1 VaciaC)))
Nodo 4 30 (Nodo 2 2 VaciaC)
-}

---------------------------------------------------------------------------------------------------

-- Ejercicio 4)
{-
a) Programar la función
a_esCota_inf :: a -> Arbol a -> Bool
que dado un valor e de tipo a y un árbol as indica si e es una cota inferior de todos
los elementos dentro del árbol as. Es decir indica si e es menor o igual a todos los
elementos del árbol as. Completar el tipado de la función para incluir los type classes
necesarios para programarla
-}

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)
a_esCota_inf :: (Eq a, Ord a) => a -> Arbol a -> Bool
a_esCota_inf _ Hoja = True
a_esCota_inf q (Rama a b c) = q <= b && (a_esCota_inf q a) && (a_esCota_inf q c)

{-
*Main> a_esCota_inf 1 (Rama (Rama Hoja 5 Hoja) 7 (Rama Hoja 3 Hoja))
True
*Main> a_esCota_inf 4 (Rama (Rama Hoja 5 Hoja) 7 (Rama Hoja 3 Hoja))
False
-}