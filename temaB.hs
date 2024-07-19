-- Ejercicio 1)
{-
a) Definir los tipos Titulo y Artista como sinónimos del tipo String y el tipo
Duracion como sinónimo del tipo Int. Además se debe definir el tipo Genero, con
constructores Rock, Blues, Pop, Jazz (todos sin parámetros).
El tipo Genero no debe estar en la clase Eq.
Por último deben definir el tipo Cancion que tiene constructores:
●Constructor Tema con parámetros:
○ El primero de tipo Titulo
○ El segundo del tipo Artista
○ El tercero del tipo Genero
○ El cuarto del tipo Duracion (la cantidad de segundos que dura la canción)
●Constructor Publicidad que tiene un único parámetro Duracion (cantidad de
segundos que dura la molesta publicidad)
-}
type Titulo = String
type Artista = String
type Duracion = Int
data Genero = Rock | Blues | Pop | Jazz
data Cancion = Tema Titulo Artista Genero Duracion | Publicidad Duracion

{-
b) Definir mediante pattern matching la función
mismo_genero :: Genero -> Genero -> Bool
que dados dos valores g1 y g2 del tipo Genero, debe devolver True cuando g1 y
g2 correspondan al mismo género musical (se construyen con el mismo constructor) y
False en caso contrario. Si se usan más de cinco casos, este apartado sumará menos
puntaje.
-}
mismo_genero :: Genero -> Genero -> Bool
mismo_genero Rock Rock = True
mismo_genero Blues Blues = True
mismo_genero Pop Pop = True
mismo_genero Jazz Jazz = True
mismo_genero _ _ = False

{-
c) Definir la función
duracion_de :: Cancion -> Duracion
que dada una canción c devuelve la cantidad de segundos que dura su reproducción (ya
sea un tema musical o una publicidad).
-}
duracion_de :: Cancion -> Duracion
duracion_de (Tema _ _ _ s) = s
duracion_de (Publicidad s) = s
{-
*Main> duracion_de (Tema "Callaita" "ConejoMalo" Rock 475)
475
*Main> duracion_de (Publicidad 175)
175
-}
{-
d) Incluir el tipo Cancion en la clase Ord de manera tal que una canción c1 sea
menor o igual que otra canción c2 si la duración de c1 es menor o igual que la duración
de c2.
-}
-- RTA:
{-
Primero que nada debemos definir una instacia de Eq para Cancion ya que no podemos 
definir previamente un Ord sin haber aclarado Eq
-}
instance Eq Cancion 
    where 
        (==) cancion1 cancion2 = duracion_de (cancion1) == duracion_de (cancion2)
{-
Como definimos una instancia de Eq con respecto a la duracion de la cancion, ahora
podemos definir un orden con respecto a la duracion.
-}
instance Ord Cancion
    where
        (<=) cancion1 cancion2 = duracion_de (cancion1) <= duracion_de (cancion2)

----------------------------------------------------------------------------------------------------

-- ejercicio 2)
{-
Definir usando recursión y pattern matching:
solo_genero :: [Cancion] -> Genero -> [Titulo]
que dada una lista de canciones cs y un género gi devuelve los títulos de las canciones
en cs que son temas musicales con género gi
IMPORTANTE: No se puede utilizar el operador == para hacer la comparación entre
valores del tipo Genero puesto que el tipo no está en la clase Eq
-}
solo_genero :: [Cancion] -> Genero -> [Titulo]
solo_genero [] _ = []
solo_genero (c:cs) b = case c of
                        (Tema t a g d) | mismo_genero b g -> t : solo_genero cs b
                        _-> solo_genero cs b
{-
*Main> solo_genero [(Tema "Ella" "perry" Rock 195),(Tema "es" "lol" Rock 250),(Tema "calladita" "messi" Rock 250), (Publicidad 240)] Rock
["Ella","es","calladita"]
-}

-------------------------------------------------------------------------------------------------------

-- Ejercicio 3)
{-
Basados en el tipo ListaAsoc del Proyecto 2, programar la función:
la_suma_mayores :: ListaAsoc a b -> b -> b
que dada una lista de asociaciones la y un dato x devuelve la suma de los datos de la
que son mayores a x. Completar el tipado de la función para incluir los type classes
necesarios para programarla.
-}
data ListaAsoc a b = VaciaC | Nodo a b (ListaAsoc a b)
la_suma_mayores :: (Num b, Ord b, Eq b) => ListaAsoc a b -> b -> b
-- Para poder sumar agregamos la condicion de que b tenga la propiedad de numero, y al ord y eq se introducen para poder hacer la comparacion.
la_suma_mayores VaciaC _ = 0
la_suma_mayores (Nodo a b c) q = case b of
                                _| b > q -> b + (la_suma_mayores c q)
                                _-> la_suma_mayores c q

{-
*Main> la_suma_mayores (Nodo "hola" 15 (Nodo "messi" 10 (Nodo "vale" 5 VaciaC))) 9
25
-}

-------------------------------------------------------------------------------------------------------------------------

-- Ejercicio 4)
{-
a) Programar la función
a_listar :: Arbol a -> [a]
que dado un árbol as de devuelve una lista con los elementos de as. En la lista resultante
el elemento del padre siempre debe estar antes que los elementos de sus hijos, Por ejemplo
la lista debe ser [7, 4, 5, 15, 10, 18]
-}
data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)
a_listar :: Arbol a -> [a]
a_listar Hoja = []
a_listar (Rama a b c) = [b] ++ a_listar a ++ a_listar c

{-
b) Inventar un ejemplo de uso de la función creando un árbol con al menos 3 elementos
(Rama (Rama Hoja 4 (Rama Hoja 5 Hoja)) 7 (Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 18 Hoja)))
-}
{-
-- c)
*Main> a_listar (Rama (Rama Hoja 4 (Rama Hoja 5 Hoja)) 7 (Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 18 Hoja)))
[7,4,5,15,10,18]
-}