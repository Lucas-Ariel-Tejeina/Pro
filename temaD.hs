-- Ejericico 1)
{-
a) Definir:
● El tipo Deportista que consta de tres constructores:
○ Constructor Futbolista: Tiene tres parámetros, el primero de tipo Nombre,
el segundo de tipo Zona y el tercero de tipo Titulos.
○ Constructor Tenista: Tiene tres parámetros, el primero de tipo Nombre, el
segundo de tipo Categoria y el tercero de tipo Titulos
○ Constructor Velocista: Tiene dos parámetros, el primero de tipo Nombre y
el segundo de tipo Titulos.
●El tipo Nombre: Debe ser un sinónimo de String se usará para los nombres de
los deportistas
●El tipo Zona: Tiene constructores Arco, Defensa, Mediocampo y Delantera,
todos constructores sin parámetros.
●El tipo Categoria: Tiene constructores Simples y Dobles ambos sin parámetros.
●El tipo Titulos: Debe ser sinónimo del tipo Int y se usan para la cantidad de
títulos del deportista.
-}
type Nombre = String
data Zona = Arco | Defensa | Mediocampo | Delantera
data Categoria = Simples | Dobles
type Titulos = Int

data Deportista = Futbolista Nombre Zona Titulos 
                | Tenista Nombre Categoria Titulos
                | Velocista Nombre Titulos

{-
b) Programar la función usando pattern matching:
misma_zona :: Zona -> Zona -> Bool
que dados dos valores z1 y z2 del tipo Zona debe devolver True cuando z1 y z2 la
misma zona de juego (se construyen con el mismo constructor) y False en caso
contrario.
Si se usan más de cinco casos, este apartado sumará menos puntaje.
-}
misma_zona :: Zona -> Zona -> Bool
misma_zona  Arco Arco = True
misma_zona  Defensa Defensa = True
misma_zona  Mediocampo Mediocampo = True
misma_zona  Delantera Delantera = True
misma_zona _ _ = False

{-
c) Programar la función
puntaje_titulos :: Deportista -> Int
que devuelve un valor de puntaje según la cantidad de títulos que tiene el deportista. El
criterio es el siguiente:
●Si el deportista es tenista: El puntaje será la cantidad de títulos
●Si el deportista es futbolista: El puntaje será el doble de la cantidad de títulos
●Si el deportista es velocista: El puntaje será el triple de la cantidad de títulos.
-}
puntaje_titulos :: Deportista -> Int
puntaje_titulos (Futbolista a b c) = c
puntaje_titulos (Tenista a b c) = c
puntaje_titulos (Velocista a b) = b
{-
*Main> puntaje_titulos (Futbolista "Lucas" Arco 19)
19
*Main> puntaje_titulos (Tenista "Lucas" S 19)
Semigroup  Show       ShowS      Simples    String
*Main> puntaje_titulos (Tenista "Lucas" Simples 39)
39
*Main> puntaje_titulos (Velocista  "Lucas" 39)
39
-}

{-
d) Incluir el tipo Deportista en la clase Ord de manera tal que un deportista se
considere mayor que otro si su valor según la función puntaje_titulos es más grande
-}
{-
-- RTA
Primero debemos definir una instancia de igualdad en el parametro de puntaje_titulos antes
de realizar una instancia de orden con puntaje_titulos.
-}
-- 1) primero
instance Eq Deportista 
    where 
        (==) deport1 deport2 = puntaje_titulos (deport1) == puntaje_titulos (deport2)
{-
*Main> puntaje_titulos (Futbolista "Lucas" Arco 19) == puntaje_titulos (Tenista "Matute" Simples 19)
True
-}
-- 2) segundo
instance Ord Deportista
    where
        (<=) deport1 deport2 = puntaje_titulos(deport1) <= puntaje_titulos (deport2)
{-
*Main> puntaje_titulos (Futbolista "Lucas" Arco 19) < puntaje_titulos (Tenista "Matute" Simples 39)
True
-}
-------------------------------------------------------------------------------------------------

-- Ejercicio 2)

{-
a) Programar de manera recursiva la función
futbolistas_zona :: [Deportista] -> Zona -> [Nombre]
que dada una lista de deportistas ds y una zona z devuelve una lista con los nombres de
los los futbolistas de ds que juegan en la zona z.
-}
futbolistas_zona :: [Deportista] -> Zona -> [Nombre]
futbolistas_zona [] _ = []
futbolistas_zona (f:fs) s = case f of
                            (Futbolista a z t) | misma_zona z s -> a : futbolistas_zona fs s
                            _-> futbolistas_zona fs s
{-
b) Escribir una lista de deportistas con al menos tres elementos, donde uno de ellos debe
ser un tenista, y otro debe ser un futbolista.
 [(Futbolista "Lucas" Arco 19),(Futbolista "Mario" Mediocampo 5),(Tenista "Leo" Simples 15)] Arco
-}
{-
c) Escribir el resultado de futbolistas_zona para la lista del punto b)
*Main> futbolistas_zona [(Futbolista "Lucas" Arco 19),(Futbolista "Mario" Mediocampo 5),(Tenista "Leo" Simples 15)] Arco
["Lucas"]
-}

--------------------------------------------------------------------------------------------------------

-- Ejercicio 3)
{-
Basados en el tipo ListaAsoc del Proyecto 2, programar la función:
la_adicionar :: ListaAsoc a b -> b -> ListaAsoc a b
que dada una lista de asociaciones la y un valor x devuelve una nueva lista de
asociaciones que resulta de adicionar x a cada valor de las asociaciones de la.
Completar el tipado de la función para incluir los type classes necesarios para programarla.
-}
data ListaAsoc a b = VaciaC | Nodo a b (ListaAsoc a b) deriving Show
-- Agregamos la caracteristica Show para poder mostrar por pantalla resultado de este tipo.

la_adicionar :: Num b => ListaAsoc a b -> b -> ListaAsoc a b
la_adicionar VaciaC _ = VaciaC
la_adicionar (Nodo a b c) n = Nodo a (b + n) (la_adicionar c n)
{-
*Main> la_adicionar (Nodo "oa" 19 (Nodo "Aa" 15 (Nodo "po" 1 VaciaC))) 2
Nodo "oa" 21 (Nodo "Aa" 17 (Nodo "po" 3 VaciaC))
-}

---------------------------------------------------------------------------------------------------------------

-- Ejercicio 4)
{-
a) Programar la función
a_filter :: (a -> Bool) -> Arbol a -> Arbol (Maybe a)
que dado un predicado p de y un árbol as idevuelve un nuevo árbol con los elementos
de as aplicándoles el constructor Just cuando cumplen con el predicado p y
reemplazándolos por Nothing cuando no satisfacen p.
-}
data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving Show

a_filter :: (a -> Bool) -> Arbol a -> Arbol (Maybe a)
a_filter _ Hoja = Hoja
a_filter f (Rama a b c) = case b of
                            _| f b -> Rama (a_filter f a) (Just b) (a_filter f c)
                            _-> Rama (a_filter f a) Nothing (a_filter f c)

{-
b) Inventar un ejemplo de uso de la función creando un árbol con al menos 3 elementos
Rama (Rama Hoja 1 Hoja ) 5 (Rama Hoja 2 Hoja)
-}

{-
c) Escribir el resultado de la función aplicada al ejemplo del inciso b)
*Main> a_filter (>2) (Rama (Rama Hoja 1 Hoja ) 5 (Rama Hoja 2 Hoja))
Rama (Rama Hoja Nothing Hoja) (Just 5) (Rama Hoja Nothing Hoja)
-}
