-- Ejercicio 1)
{-
a) Definir el tipo Dedicacion que consta de los constructores: Simple, Semi, Full,
Investigador. Los constructores no toman parámetros. El tipo Dedicacion no debe
estar en la clase Eq. Luego programa la función usando pattern matching:
misma_dedicacion :: Dedicacion -> Dedicacion -> Bool
que dados dos valores p1 y p2 del tipo Dedicacion debe devolver True cuando p1
y p2 son la misma dedicación (se construyen con el mismo constructor) y False en caso
contrario.
Si se usan más de cinco casos, o menos de cinco casos este apartado sumará menos
puntaje.
-}

data Dedicacion = Simple | Semi | Full | Investigador deriving Show
misma_dedicacion :: Dedicacion -> Dedicacion -> Bool
misma_dedicacion  Simple Simple = True
misma_dedicacion  Semi Semi = True
misma_dedicacion  Full Full = True
misma_dedicacion  Investigador Investigador = True
misma_dedicacion  _ _ = False

{-
b) Programar la función
horas_trabajo :: Dedicacion -> Cantidad
●Si es Simple devuelve 10
●Si es Semi devuelve 20
●Full devuelve 50
●Investigador devuelve 60
●Cantidad debe ser definida como sinónimo de Int
-}
type Cantidad = Int
horas_trabajo :: Dedicacion -> Cantidad
horas_trabajo Simple = 10
horas_trabajo Semi = 20
horas_trabajo Full = 50
horas_trabajo Investigador = 60
{-
*Main> horas_trabajo Simple 
10
*Main> horas_trabajo Semi
20
*Main> horas_trabajo Full
50
*Main> horas_trabajo Investigador 
60
-}

{-
c) Agregar al tipo Persona la Dedicacion a los constructores Decane, Docente y NoDocente.
-}
data Persona = Decane Dedicacion | Docente Dedicacion | NoDocente Dedicacion deriving Show

{-
d) Incluir el tipo Dedicacion en la clase Ord de manera tal que un dedicacion se
considere mayor que otra si su valor según la función horas_trabajo es más grande.
-}
{-
-- RTA
Primero debemos agregar la instancia Eq al parametro horas_trabajo para que luego lo
podamos comparar con Ord
-}
--  Primero
instance Eq Dedicacion
    where 
        (==) dedi1 dedi2 = horas_trabajo (dedi1) == horas_trabajo (dedi2)
{-
*Main> horas_trabajo (Simple) == horas_trabajo (Simple)
True
-}
-- Segundo
instance Ord Dedicacion
    where 
        (<=) dedi1 dedi2 = horas_trabajo (dedi1) <= horas_trabajo (dedi2)
{-
*Main> horas_trabajo (Simple) == horas_trabajo (Investigador)
False
*Main> horas_trabajo (Simple) < horas_trabajo (Investigador)
True
-}

---------------------------------------------------------------------------------------------
-- Ejercicio 1)
{-a) Programar de manera recursiva la función
solo_dedicacion :: [Persona] -> Dedicacion -> [Persona]
que dada una lista de personas ps y un dedicacion r devuelve una lista con las personas
de ps que tienen dedicación r.-}
solo_dedicacion :: [Persona] -> Dedicacion -> [Persona]
solo_dedicacion [] _ = []
solo_dedicacion (p:ps) d = case p of
                            (Decane e)| misma_dedicacion e d -> (Decane e) : solo_dedicacion ps d
                            (Docente e)| misma_dedicacion e d -> (Docente e) : solo_dedicacion ps d
                            (NoDocente e)| misma_dedicacion e d -> (NoDocente e) : solo_dedicacion ps d 
                            _->solo_dedicacion ps d

{-
b) Escribir una lista de personas con al menos tres elementos, donde al menos uno de ellos
debe tener dedicación, y otro no debe tener dedicación.
[(Decane Simple),(Docente Full),(NoDocente Investigador)]
-}
{-
c) Escribir el resultado de solo_dedicacion para la lista del punto b)
*Main> solo_dedicacion [(Decane Simple),(Docente Full),(NoDocente Investigador)] Simple 
[Decane Simple]
-}

--------------------------------------------------------------------------------------------------------------------------

-- Ejercicio3)
{-
Ejercicio 3
Basados en el tipo ListaAsoc del Proyecto 2, programar la función:
la_mismo_valor :: ListaAsoc a b -> b -> ListaAsoc a b
que dada una lista de asociaciones la, devuelve una nueva lista de asociaciones con las
asociaciones de la cuyos valores son iguales al segundo parámetro. Completar el tipado
de la función para incluir los type clases necesarios para programarla.
-}

data ListaAsoc a b = VaciaC | Nodo a b (ListaAsoc a b) deriving Show
-- Agregamos la caracteristica Show ya que queremos mostrar por pantalla algo del tipo ListaAsoc.
la_mismo_valor ::Eq b => ListaAsoc a b -> b -> ListaAsoc a b
-- Agregamos la condicion Eq b, ya que queremos comparar los elementos que son de tipo b. 
la_mismo_valor VaciaC _ =  VaciaC
la_mismo_valor (Nodo a b c) q = case b of
                                _| b == q -> Nodo a b (la_mismo_valor c q)
                                _-> (la_mismo_valor c q)
{-
Nodo "hola" 12 (Nodo "chau" 11 (Nodo "Hello" 17 (Nodo "Bye" 11 VaciaC))).

*Main> la_mismo_valor (Nodo "hola" 12 (Nodo "chau" 11 (Nodo "Hello" 17 (Nodo "Bye" 11 VaciaC)))) 11 
Nodo "chau" 11 (Nodo "Bye" 11 VaciaC)
-}

--------------------------------------------------------------------------------------------------------------------

-- Ejercicio 4)

{-
a) Programar la función
dar_subarbol :: a -> Arbol a -> Arbol a
que dado un valor e de tipo a y un árbol as devuelve el subarbol que tiene como raiz a
e o Hoja en caso de no existir. Completar el tipado de la función para incluir los type classes
necesarios para programarla.
-}
-- Agregamos la caracteristica Show para poder mostrar por pantalla el tipo Arbol a.
data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving Show

dar_subarbol :: Eq a => a -> Arbol a -> Arbol a
dar_subarbol _ Hoja = Hoja
dar_subarbol q (Rama izq b der) | q == b = Rama izq b der
                                | otherwise = case (dar_subarbol q izq, dar_subarbol q der) of
                                            (subIzq,Hoja) -> subIzq
                                            (Hoja,subDer) -> subDer
                                            (subIzq,subDer) -> subIzq 
{-
*Main> dar_subarbol 7 (Rama (Rama Hoja 4 (Rama Hoja 5 Hoja)) 7 (Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 18 Hoja)))
Rama (Rama Hoja 4 (Rama Hoja 5 Hoja)) 7 (Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 18 Hoja))
*Main> dar_subarbol 1 (Rama (Rama Hoja 4 (Rama Hoja 5 Hoja)) 7 (Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 18 Hoja)))
Hoja
*Main> dar_subarbol 15 (Rama (Rama Hoja 4 (Rama Hoja 5 Hoja)) 7 (Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 18 Hoja)))
Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 18 Hoja)
-}