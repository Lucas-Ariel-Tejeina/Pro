-- Ejercicio 1)
{-
a) Definir el tipo Palo que consta de los constructores Treboles, Corazones, Picas,
Diamantes. Los constructores no toman parámetros. El tipo Palo no debe estar en la
clase Eq. Luego programa la función usando pattern matching:
mismo_palo :: Palo -> Palo -> Bool
que dados dos valores p1 y p2 del tipo Palo debe devolver True cuando p1 y p2
son el mismo palo (se construyen con el mismo constructor) y False en caso contrario.
Si se usan más de cinco casos, este apartado sumará menos puntaje.
-}
data Palo = Treboles | Corazones | Picas | Diamantes

mismo_palo :: Palo -> Palo -> Bool
mismo_palo  Treboles Treboles = True
mismo_palo  Corazones Corazones = True
mismo_palo  Picas Picas = True
mismo_palo  Diamantes Diamantes = True
mismo_palo  _ _ = False

{-
b) Definir el tipo Naipe que representa una carta de poker. Tiene constructores:
*Constructor Numerada: Toma dos parámetros, el primero de tipo Numero y el
segundo de tipo Palo
*Constructores Rey, Reina, Jota, As: Todos son constructores con un sólo
parámetro de tipo Palo
El tipo Numero debe ser un sinónimo del tipo Int.
-}
type Numero = Int

data Naipe = Numerada Numero Palo | Rey Palo | Reina Palo | Jota Palo | As Palo

{-
c) Programar la función
valor_naipe :: Naipe -> Int
teniendo en cuenta que el valor de una carta será:
Si es una carta numerada : Su valor es el número de la carta.
Si es el naipe Jota : Su valor es 11
Si es el naipe Reina : Su valor es 12
Si es el naipe Rey : Su valor es 13
Si es el naipe As : Su valor es 14
-}
valor_naipe :: Naipe -> Int
valor_naipe (Jota _) = 11
valor_naipe (Reina _) = 12
valor_naipe (Rey _) = 13
valor_naipe (As _) = 14
valor_naipe (Numerada a _) = a

{-
d) Incluir el tipo Naipe en la clase Ord de manera tal que un naipe se considere mayor
que otro si su valor según la función valor_naipe es más grande.
-}
-- RTA:
-- Para poder comparar si 2 elementos son distintos, primero debemos saber cuando son iguales
instance Eq Naipe 
    where
        (==) carta_x carta_y = valor_naipe (carta_x) == valor_naipe (carta_y)
{-
*Main> Jota Treboles == Jota Picas
True
*Main> Jota Treboles == Reina Picas
False
*Main> Reina Treboles == Reina Picas
True
*Main> Rey Treboles == Rey Picas
True
*Main> As Treboles == As Picas
True
*Main> Numerada 7 As == Numerada 7 Rey
True
*Main> Numerada 7 Diamantes == Numerada 7 Picas
True
-}
-- Ahora podemos agregar las instancias del Ord
instance Ord Naipe 
    where
        (<=) carta_x carta_y = valor_naipe (carta_x) <= valor_naipe (carta_y)
{-
*Main> Jota Treboles <= Reina Picas
True
*Main> Jota Treboles <= Rey Corazones 
True
-}

------------------------------------------------------------------------------------------------------

-- Ejericico 2)
{-
a) Programar de manera recursiva la función
solo_numeradas :: [Naipe] -> Palo -> [Numero]
que dada una lista de cartas ns y un palo p devuelve una lista con los números de las
cartas numeradas (las que no son ases, jotas, reyes ni reinas) de ns que son del palo p.
-}
solo_numeradas :: [Naipe] -> Palo -> [Numero]
solo_numeradas [] _ = []
solo_numeradas (n:ns) a = case n of
                            Numerada m b | mismo_palo b a -> m : solo_numeradas ns a
                            _ -> solo_numeradas ns a   

{-
b) Escribir una lista de naipes con al menos tres elementos, donde uno de ellos debe ser
una figura, y otro debe ser una carta numerada.
-}
-- [Numerada 7 Treboles, Numerada 9 Picas, Rey Corazones, As Diamantes]

{-
c) Escribir el resultado de solo_numeradas para la lista del punto b)
-}
{-
*Main> solo_numeradas [Numerada 7 Treboles, Numerada 9 Picas, Rey Corazones, As Diamantes] Diamantes 
[]
*Main> solo_numeradas [Numerada 7 Treboles, Numerada 9 Picas, Rey Corazones, As Diamantes] 
Picas 
[9]
-}

---------------------------------------------------------------------------------------------------

{-
-- Ejericico 3)

Basados en el tipo ListaAsoc del Proyecto 2, programar la función:
la_menores :: ListaAsoc a b -> b -> ListaAsoc a b
que dada una lista de asociaciones la y un dato x devuelve una nueva lista de
asociaciones con las asociaciones de la cuyos valores son menores que x. Completar el
tipado de la función para incluir los type classes necesarios para programarla.
-}

data ListaAsoc a b = VaciaC | Nodo a b (ListaAsoc a b) deriving Show
-- Agregamos deriving Show para poder mostrar resulados por pantalla de tipo ListaAsooc a b
la_menores :: (Ord b,Eq b) => ListaAsoc a b -> b -> ListaAsoc a b
-- Agregamos los tipos Ord y Eq ya que si el tipo b los cumple, pueda entrar en la funcion y luego comparar.
la_menores VaciaC _ = VaciaC
la_menores (Nodo a b c) n = case b of
                            _| b < n -> Nodo a b (la_menores c n)
                            _-> la_menores c n
{-
*Main> la_menores (Nodo "Hola" 15(Nodo "callao" 100 (Nodo "bb" 7 VaciaC))) 16
Nodo "Hola" 15 (Nodo "bb" 7 VaciaC)
-}

--------------------------------------------------------------------------------------------------

-- Ejercicio 4)
{-
a) Programar la función
a_esCota_sup :: a -> Arbol a -> Bool
que dado un valor e de tipo a y un árbol as indica si e es una cota superior de todos
los elementos dentro del árbol as. Es decir indica si e es mayor o igual a todos los
elementos del árbol as. Completar el tipado de la función para incluir los type classes
necesarios para programarla
-}

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)

a_esCota_sup ::(Ord a, Eq a) => a -> Arbol a -> Bool
a_esCota_sup a Hoja = True
a_esCota_sup a (Rama s d c) = d < a && (a_esCota_sup a s) && (a_esCota_sup a c) 

{-
-- b)
(Rama (Rama Hoja 14 Hoja) 6 (Rama Hoja 8 Hoja))
-}
{-
-- c)
*Main> a_esCota_sup 17 (Rama (Rama Hoja 14 Hoja) 6 (Rama Hoja 8 Hoja))
True
*Main> a_esCota_sup 17 (Rama (Rama Hoja 14 Hoja) 6 (Rama Hoja 17 Hoja))
False
-}