-- 1)

-- a) Implement ́a el tipo Carrera como est ́a definido arriba.
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving Eq

{-
b) Defin ́ı la siguiente funci ́on, usando pattern matching : titulo :: Carrera -> String
que devuelve el nombre completo de la carrera en forma de string. Por ejemplo, para el
constructor Matematica, debe devolver ”Licenciatura en Matem ́atica”.
-}
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica" 
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"
{-
*Main> titulo Matematica 
"Licenciatura en Matematica"
*Main> titulo Fisica 
"Licenciatura en Fisica"
*Main> titulo Computacion 
"Licenciatura en Computacion"
*Main> titulo Astronomia 
"Licenciatura en Astronomia"
-}

-- c)
{-
c ) Para escribir m ́usica se utiliza la denominada notaci ́on musical, la cual consta de
notas (do, re, mi, ...). Adem ́as, estas notas pueden presentar alg ́un modificador ]
(sostenido) o [ (bemol), por ejemplo do], si[, etc. Por ahora nos vamos a olvidar de
estos modificadores (llamados alteraciones) y vamos a representar las notas b ́asicas.
Definir el tipo NotaBasica con constructores Do, Re, Mi, Fa, Sol, La y Si
-}

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq,Ord,Show,Bounded)

-- d)
{-
d ) El sistema de notaci ́on musical anglosaj ́on, tambi ́en conocido como notaci ́on o cifrado
americano, relaciona las notas b ́asicas con letras de la A a la G. Este sistema se usa por
ejemplo para las tablaturas de guitarra. Programar usando pattern matching la funci ́on:
cifradoAmericano : : NotaBasica −>C h a r
que relaciona las notas Do, Re, Mi, Fa, Sol, La y Si con los caracteres ’C’ , ’D’, ’E’,
’F’, ’G’, ’A’ y ’B’ respectivamente.
-}

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

{-
*Main> cifradoAmericano Do
'C'
*Main> cifradoAmericano Re
'D'
*Main> cifradoAmericano Mi
'E'
*Main> cifradoAmericano Fa
'F'
*Main> cifradoAmericano Sol
'G'
*Main> cifradoAmericano La
'A'
*Main> cifradoAmericano Si
'B'
-}

--------------------------------------------------------------------------------------------------

-- a) Agregamos el tipo Eq en Carrera para poder comparar entre constructores.
-- Agregamos el tipo Eq en NotaBasica para poder comparar entre constructores, 
-- agregamos Ord para poder ver si un constructor es mas grande que otro,
-- agregamos Show para poder mostrar el constructor por pantalla.
{-
*Main> Matematica == Matematica
True
*Main> Matematica == Computacion
False
*Main> Do <= Re
True
*Main> Fa `min` Sol
Fa
-}

-----------------------------------------------------------------------------------------------------------------

-- 3)
{-
a) Definir usando polimorfismo ad hoc la funci ́on minimoElemento que calcula (de manera
recursiva) cu ́al es el menor valor de una lista de tipo [a]. Asegurarse que s ́olo est ́e
definida para listas no vac ́ıas.
-}

minimoElemento :: Ord a => [a] -> a
minimoElemento [a] = a
minimoElemento (a:as) = a `min` minimoElemento as
{-
*Main> minimoElemento [100,5,4,2,1]
1
*Main> minimoElemento "abcdefghijk"
'a'
-}

{-
b) Definir la funci ́on minimoElemento’ de manera tal que el caso base de la recursi ́on
sea el de la lista vac ́ıa. Para ello revisar la clase Bounded.
Ayuda: Para probar esta funci ́on dentro de ghci con listas vac ́ıas, indicar el tipo concre-
to con tipos de la clase Bounded, por ejemplo: ([1,5,10]::[Int]), ([]::[Bool]),
etc.
-}

minimoElemento' :: (Ord a, Bounded a) => [a] -> a
minimoElemento' [] = maxBound 
minimoElemento' (a:as) = a `min` minimoElemento as
{-
*Main> minimoElemento' "abcdefghijk"
'a'
*Main> minimoElemento' ([1,5,10]::[Int])
1
*Main> minimoElemento' ([]::[Bool])
True
-}

-- c) Usar la funci ́on minimoElemento para determinar la nota m ́as grave de la melod ́ıa: [Fa, La, Sol, Re, Fa]
{-
*Main> minimoElemento [Fa, La, Sol, Re, Fa]
Re
-}

------------------------------------------------------------------------------------------------------------

-- a) Implement ́a el tipo Deportista y todos sus tipos accesorios (NumCamiseta, Altura, Zona, etc) tal como est ́an definidos arriba.

-- Sin ́onimos de tipo
type Altura = Int
type NumCamiseta = Int
-- Tipos algebr ́aicos sin par ́ametros (aka enumerados)
data Zona = Arco | Defensa | Mediocampo | Delantera deriving (Eq,Show)
data TipoReves = DosManos | UnaMano deriving Show
data Modalidad = Carretera | Pista | Monte | BMX deriving Show
data PiernaHabil = Izquierda | Derecha deriving Show
-- Sin ́onimo
type ManoHabil = PiernaHabil
-- Deportista es un tipo algebraico con constructores param ́etricos
data Deportista = Ajedrecista | Ciclista Modalidad | Velocista Altura | Tenista TipoReves ManoHabil Altura | Futbolista Zona NumCamiseta PiernaHabil Altura deriving Show
-- b) ¿Cu ́al es el tipo del constructor Ciclista?
{-
*Main> :t Ciclista
Ciclista :: Modalidad -> Deportista, entonces el tipo es Modalidad, ya que toma argumentos de tipo Modalidad.
-}

{-
c ) Program ́a la funci ́on contar_velocistas :: [Deportista] -> Int que dada una
lista de deportistas xs, devuelve la cantidad de velocistas que hay dentro de xs.
Programar contar_velocistas sin usar igualdad, utilizando pattern matching.
-}

contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas ((Velocista a):xs) = 1 + contar_velocistas xs
contar_velocistas ((Ajedrecista):xs) = contar_velocistas xs
contar_velocistas ((Ciclista a):xs) = contar_velocistas xs
contar_velocistas ((Tenista a b c):xs) = contar_velocistas xs
contar_velocistas ((Futbolista a b c d):xs) = contar_velocistas xs

{-
*Main> contar_velocistas [Velocista 4,Ciclista Monte,Velocista 16,Ajedrecista,Tenista DosManos Derecha 150]
2
-}

{-
d ) Program ́a la funci ́on contar_futbolistas :: [Deportista] -> Zona -> Int que
dada una lista de deportistas xs, y una zona z, devuelve la cantidad de futbolistas
incluidos en xs que juegan en la zona z. No usar igualdad, s ́olo pattern matching.
-}
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] z = 0
contar_futbolistas ((Futbolista z _ _ _):xs) z = 1 + contar_futbolistas xs z
contar_futbolistas (x:xs) z = contar_futbolistas xs z

{-
*Main> contar_futbolistas [Futbolista Arco 1 Derecha 19] Arco
1
*Main> contar_futbolistas [Futbolista Arco 1 Derecha 190,Futbolista Arco 12 Izquierda 210,Futbolista Delantera 23 Derecha 195] Arco
2
*Main> contar_futbolistas [Futbolista Arco 1 Derecha 190, Tenista DosManos Derecha 150] Arco
1
*Main> contar_futbolistas [Tenista DosManos Derecha 150, Velocista 260, Ciclista Pista] Arco
0
-}

-- e) ¿La funci ́on anterior usa filter? Si no es as ́ı, reprogramala para usarla.
f :: Zona -> Deportista -> Bool
f b (Futbolista z _ _ _) = z == b
f x b = False

contar_futbolistas' :: [Deportista] -> Zona -> Int
contar_futbolistas' [] z = 0
contar_futbolistas' xs z = length (filter (f z) xs)
{-
*Main> contar_futbolistas' [Futbolista Arco 1 Derecha 19] Arco
1
*Main> contar_futbolistas' [Futbolista Arco 1 Derecha 190,Futbolista Arco 12 Izquierda 210,Futbolista Delantera 23 Derecha 195] Arco
2
*Main> contar_futbolistas' [Futbolista Arco 1 Derecha 190, Tenista DosManos Derecha 150] Arco
1
*Main> contar_futbolistas' [Tenista DosManos Derecha 150, Velocista 260, Ciclista Pista] Arco
0
-}

----------------------------------------------------------------------------------------------------------

-- 5) 
-- a) Implement ́a la funci ́on sonidoNatural como est ́a definida arriba.

sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

-- b) Definir el tipo enumerado Alteracion que consta de los constructores Bemol, Natural y Sostenido.

data Alteracion = Bemol | Natural | Sostenido

{-
c ) Definir el tipo algebraico NotaMusical que debe tener un solo un constructor que llama-
remos Nota el cual toma dos par ́ametros. El primer par ́ametro es de tipo NotaBasica
y el segundo de tipo Alteracion. De esta manera cuando se quiera representar una
nota alterada se puede usar como segundo par ́ametro del constructor un Bemol o
Sostenido y si se quiere representar una nota sin alteraciones se usa Natural como
segundo par ́ametro.
-}

data NotaMusical = Nota NotaBasica Alteracion

{-
d ) Defin ́ı la funci ́on sonidoCromatico :: NotaMusical -> Int que devuelve el sonido
de una nota, incrementando en uno su valor si tiene la alteraci ́on Sostenido, decre-
mentando en uno si tiene la alteraci ́on Bemol y dejando su valor intacto si la alteraci ́on
es Natural
-}

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota a Bemol) = sonidoNatural a - 1
sonidoCromatico (Nota a Sostenido) = sonidoNatural a + 1
sonidoCromatico (Nota a Natural) = sonidoNatural a
{-
*Main> sonidoCromatico (Nota Do Bemol)
-1
*Main> sonidoCromatico (Nota Do Sostenido)
1
*Main> sonidoCromatico (Nota Do Natural)
0
-}

-- e) Inclu ́ı el tipo NotaMusical a la clase Eq de manera tal que dos notas que tengan el
-- mismo valor de sonidoCromatico se consideren iguales.

instance Eq NotaMusical where
    (Nota b a) == (Nota c v) = sonidoCromatico (Nota b a) == sonidoCromatico (Nota c v)
{-
*Main> Nota Re Sostenido == Nota Mi Bemol
True
*Main> Nota Do Sostenido == Nota Fa Bemol
False
-}

-- f)
{-
Inclu ́ı el tipo NotaMusical a la clase Ord definiendo el operador <=. Se debe definir
que una nota es menor o igual a otra si y s ́olo si el valor de sonidoCromatico para la
primera es menor o igual al valor de sonidoCromatico para la segunda.
-}
instance Ord NotaMusical where
    (Nota a b) <= (Nota c d) = sonidoCromatico (Nota a b) <= sonidoCromatico (Nota c d)
{-
*Main> Nota Re Sostenido <= Nota Mi Bemol
True
*Main> Nota Fa Sostenido <= Nota Do Sostenido 
False
-}

-----------------------------------------------------------------------------------------------------

-- 6)

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just (x)

{-
*Main> primerElemento []
Nothing
*Main> primerElemento [1,2,3,4,5]
Just 1
-}

-------------------------------------------------------------------------------------------------------------

-- 7)

data Cola = VaciaC | Encolada Deportista Cola deriving Show

-- a)
{-
1) atender :: Cola -> Maybe Cola, que elimina de la cola a la persona que est ́a
en la primer posici ́on de una cola, por haber sido atendida. Si la cola est ́a vac ́ıa,
devuelve Nothing.
-}

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada a b) = Just(b)
{-
*Main> atender (Encolada (Velocista 8) (Encolada (Ciclista Pista) (Encolada (Ciclista Pista) VaciaC)))
Just (Encolada (Ciclista Pista) (Encolada (Ciclista Pista) VaciaC))
*Main> atender VaciaC 
Nothing
-}

-- b)
{-
2) encolar :: Deportista -> Cola -> Cola, que agrega una persona a una cola
de deportistas, en la  ́ultima posici ́on.
-}
encolar :: Deportista -> Cola -> Cola
encolar a VaciaC = (Encolada a (VaciaC))
encolar z (Encolada a b) = Encolada a(encolar z b)
{-
*Main> encolar Ajedrecista (Encolada (Velocista 8) (Encolada (Ciclista Pista) (Encolada (Ciclista Pista) VaciaC)))
Encolada (Velocista 8) (Encolada (Ciclista Pista) (Encolada (Ciclista Pista) (Encolada Ajedrecista VaciaC)))
-}

-- c)
{-
3) busca :: Cola -> Zona -> Maybe Deportista, que devuelve el/la primera
futbolista dentro de la cola que juega en la zona que se corresponde con el segundo
par ́ametro. Si no hay futbolistas jugando en esa zona devuelve Nothing
-}

busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing
busca (Encolada a b) z = case a of
                        Futbolista m n c q | m == z -> Just(Futbolista m n c q) 
                        _ -> busca b z
{-
*Main> busca  (Encolada (Velocista 8) (Encolada (Futbolista Defensa 12 Derecha 180) (Encolada (Ciclista Pista) VaciaC))) Arco
Nothing
*Main> busca  (Encolada (Velocista 8) (Encolada (Futbolista Arco 12 Derecha 180) (Encolada (Ciclista Pista) VaciaC))) Arco
Just (Futbolista Arco 12 Derecha 180)
-}

-- e)
-- ¿A qu ́e otro tipo se parece Cola?
-- No es un tipo, pero se parece mucho a la estructura de listas, ya que las podemos
-- recorrer o podemos agregar y quitar elementos.

----------------------------------------------------------------------------------------------------

-- 8)
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving Show

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

-- a)

{-
a) ¿Como se debe instanciar el tipo ListaAsoc para representar la informaci ́on almacenada
en una gu ́ıa telef ́onica?
Teniendo la consideracion de que cuando buscamos a alguien en la guia telefonica, buscamos el nombre (porque seria una dato conocido) para obtener su numero y ademas teniendo en cuenta que cuando buscamos a alguien y si no lo encontramos no obtenemos ningun numero, planteamos la funcion de la siguiente manera.
-}

type Guiatelefonica = ListaAsoc String Int 

guiaTelefonica :: Guiatelefonica -> String -> Maybe Int
guiaTelefonica Vacia _ = Nothing
guiaTelefonica (Nodo m n e) z = case m of
                                _ | m == z -> Just(n) 
                                _ -> guiaTelefonica e z
{-
*Main> guiaTelefonica (Nodo "Leandro" 146874 (Nodo "Agustin Paz" 785142 (Nodo "Agustin Perez" 123456(Vacia)))) "Agustin Perez"
Just 123456
*Main> guiaTelefonica (Nodo "Leandro" 146874 (Nodo "Agustin Paz" 785142 (Nodo "Agustin Poriz" 123456(Vacia)))) "Agustin Perez"
Nothing
-}

-- b) Program ́a las siguientes funciones:
{-
1) la_long :: ListaAsoc a b -> Int que devuelve la cantidad de datos en una
lista.
-}
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b c) = 1 + la_long c
{-
*Main> la_long (Nodo "Leandro" 146874 (Nodo "Agustin Paz" 785142 (Nodo "Agustin Poriz" 123456(Vacia))))
3
*Main> la_long (Vacia)
0
-}
{-
2) la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b, que de-
vuelve la concatenaci ́on de dos listas de asociaciones.
-}
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat m v = case (m,v) of
                (Vacia,v) -> v
                (m,Vacia) -> m
                (Nodo a b c, v) -> case c of
                                   Vacia -> Nodo a b (v)
                                   _ -> Nodo a b (la_concat c v)
{-
*Main> la_concat (Nodo "Leandro" 146874 (Nodo "Agustin Paz" 785142 (Nodo "Agustin Poriz" 123456(Vacia)))) (Nodo "Lucas" 74 (Nodo "Agusaz" 7852 (Nodo "Aguoriz" 1256(Vacia))))
Nodo "Leandro" 146874 (Nodo "Agustin Paz" 785142 (Nodo "Agustin Poriz" 123456 (Nodo "Lucas" 74 (Nodo "Agusaz" 7852 (Nodo "Aguoriz" 1256 Vacia)))))
-}

-- c)
{-
3) la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b, que
agrega un nodo a la lista de asociaciones si la clave no est ́a en la lista, o actualiza
el valor si la clave ya se encontraba
-}
la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia a b = Nodo a b Vacia
la_agregar (Nodo a b c) m n = case a of
                            _| a == m -> Nodo a n (la c m n)
                            _ -> Nodo a b (la_agregar c m n)

la :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la Vacia a b = Vacia
la (Nodo a b c) m n = case a of
                   _| a == m -> Nodo a n (la c m n)     
                   _ -> Nodo a b(la c m n)
{-
*Main> la_agregar (Nodo 2 "Pajaro" (Nodo 3 "Pajaro" Vacia)) 3 "elfenates"
Nodo 2 "Pajaro" (Nodo 3 "elfenates" Vacia)
*Main> la_agregar (Nodo 2 "Pajaro" (Nodo 2 "Pajaro" Vacia)) 3 "elfenates"
Nodo 2 "Pajaro" (Nodo 2 "Pajaro" (Nodo 3 "elfenates" Vacia))
*Main> la_agregar (Nodo 3 "Pajaro" (Nodo 3 "Pajaro" Vacia)) 3 "elfenates"
Nodo 3 "elfenates" (Nodo 3 "elfenates" Vacia)
-}

-- d)
{-
4) la_pares :: ListaAsoc a b -> [(a, b)] que transforma una lista de asocia-
ciones en una lista de pares clave-dato
-}
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b c) = (a,b) : la_pares c
{-
*Main> la_pares (Nodo 2 "Pajaro" (Nodo 2 "Pajaro" Vacia))
[(2,"Pajaro"),(2,"Pajaro")]
*Main> la_pares Vacia
[]
-}

-- e)
{-
5) la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b que dada una lista
y una clave devuelve el dato asociado, si es que existe. En caso contrario devuelve
Nothing.
-}
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia a = Nothing
la_busca (Nodo a b c) n = case a of
                         _| a == n -> Just b
                         _-> la_busca c n
{-
*Main> la_busca (Nodo 67 "Pajaro" (Nodo 36 "Pajaro" Vacia)) 2
Nothing
*Main> la_busca (Nodo 67 "Pajaro" (Nodo 2 "Pajaro" Vacia)) 2
Just "Pajaro"
-}

-- f)
{-
6) la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b que dada
una clave a elimina la entrada en la lista.
-}
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia
la_borrar n (Nodo a b c) = case a of
                            _| a == n -> la_borrar n c
                            _-> Nodo a b (la_borrar n c)
{-
*Main> la_borrar 3 (Nodo 3 "Pajaro" (Nodo 3 "Pajaro" Vacia))
Vacia
*Main> la_borrar 3 (Nodo 32 "Pajaro" (Nodo 3 "Pajaro" Vacia))
Nodo 32 "Pajaro" Vacia
-}

-----------------------------------------------------------------------------------------------------

-- 9) Puntos estrellas

data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving Show
type Prefijos = Arbol String
can, cana, canario, canas, cant, cantar, canto :: Prefijos
can = Rama cana "can" cant
cana = Rama canario "a" canas
canario = Rama Hoja "rio" Hoja
canas = Rama Hoja "s" Hoja
cant = Rama cantar "t" canto
cantar = Rama Hoja "ar" Hoja
canto = Rama Hoja "o" Hoja
-- a)
{-
a) a_long :: Arbol a -> Int que dado un  ́arbol devuelve la cantidad de datos alma-
cenados.
-}
a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama a c b) = a_long a + a_long b + 1
{-
*Main> a_long can
7
*Main> a_long cana
3
*Main> a_long canas
1
*Main> a_long canario
1
*Main> a_long cant
3
*Main> a_long canto
1
*Main> a_long cantar
1
-}

-- b)
{-
b) a_hojas :: Arbol a -> Int que dado un  ́arbol devuelve la cantidad de hojas.
-}
a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama a c b) = a_hojas a + a_hojas b
{-
*Main> a_hojas can
8
*Main> a_hojas cana
4
*Main> a_hojas canas
2
*Main> a_hojas canario
2
*Main> a_hojas cant
4
*Main> a_hojas canto
2
*Main> a_hojas cantar
2
-}

-- c)
{-
c ) a_inc :: Num a => Arbol a -> Arbol a que dado un  ́arbol que contiene n ́umeros,
los incrementa en uno.
Para mostrar un resultado de tipo arbol, devemos agregar al tipo la caracteristica Show.
-}
a_inc :: Num a => Arbol a -> Arbol a
a_inc (Rama a b c) = case a of
                    Hoja -> Rama Hoja (b+1) Hoja
                    _ -> Rama (a_inc a) (b+1) (a_inc c)
{-
*Main> a_inc (Rama Hoja 5 Hoja)
Rama Hoja 6 Hoja
*Main> a_inc (Rama (Rama Hoja 2 Hoja) 0 (Rama Hoja 1 Hoja))
Rama (Rama Hoja 3 Hoja) 1 (Rama Hoja 2 Hoja)
-}

-- d)
{-
d ) a_map :: (a -> b) -> Arbol a -> Arbol b que dada una funci ́on y un  ́arbol,
devuelve el  ́arbol con la misma estructura, que resulta de aplicar la funci ́on a cada uno
de los elementos del  ́arbol. Revis ́a la definici ́on de la funci ́on anterior y reprogramala
usando a_map.
-}
a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f (Rama Hoja a Hoja) = Rama Hoja (f a) Hoja
a_map f (Rama a b c) = Rama (a_map f a) (f b) (a_map f c)

aInc :: Num a => Arbol a -> Arbol a
aInc a = a_map (+1) a

{-
*Main> aInc (Rama Hoja 5 Hoja)
Rama Hoja 6 Hoja
*Main> aInc (Rama (Rama Hoja 2 Hoja) 0 (Rama Hoja 1 Hoja))
Rama (Rama Hoja 3 Hoja) 1 (Rama Hoja 2 Hoja)
-}

-------------------------------------------------------------------------------------------------

-- 10)
{-
a) : Definir el tipo recursivo ABB utilizando los constructores:
RamaABB :: ABB a -> a -> ABB a
VacioABB :: ABB a
-}

data ABB a = RamaABB (ABB a) a | VacioABB deriving Show
{-
*Main> :t RamaABB
RamaABB :: ABB a -> a -> ABB a
*Main> :t VacioABB
VacioABB :: ABB a
-}

{-
b) Definir una funci ́on insertarABB que tome un valor y un  ́arbol binario como entrada y
devuelva un nuevo  ́arbol que contenga el valor insertado en el  ́arbol original. La funci ́on
tiene que tener el siguiente tipado:
insertarABB :: Ord a => a -> ABB a -> ABB a
-}
-- Para mostrar este resultado agregamos la clase Show a ABB
insertarABB :: Ord a => a -> ABB a -> ABB a 
insertarABB n VacioABB = VacioABB
insertarABB n (RamaABB b a) = RamaABB (insertarABB n b) n 

{-
*Main> insertarABB 5 (RamaABB VacioABB 7)
RamaABB VacioABB 5

*Main> insertarABB 5 (RamaABB (RamaABB (RamaABB VacioABB 11) 9) 7)
RamaABB (RamaABB (RamaABB VacioABB 5) 5) 5
-}

-- c)

{-
c ) Define una función llamada buscarEnArbol que tome un valor y un  ́arbol binario como
entrada y devuelva True si el valor est ́a presente en el  ́arbol y False en caso contrario.
La funci ́on tiene que tener el siguiente tipado:
buscarABB :: Eq a => a -> ABB a -> Bool
-}
buscarABB :: Eq a => a -> ABB a -> Bool
buscarABB a VacioABB = False
buscarABB a (RamaABB c b) = a == b || (buscarABB a c)

{-
*Main> buscarABB 5 VacioABB
False

*Main> buscarABB 5 (RamaABB (RamaABB (RamaABB VacioABB 1) 11) 9)
False

*Main> buscarABB 5 (RamaABB (RamaABB (RamaABB VacioABB 5) 11) 9)
True
-}
-- d)
{-
Definir la funci ́on verificarABB que devuelve True si el  ́arbol cumple con la propiedad
fundamental o False en caso contrario. De manera auxiliar pueden definir las funciones
:
mayor_a_todos :: Ord a => a -> ABB a -> Bool
menor_a_todos :: Ord a => a -> ABB a -> Bool
-}
mayor_a_todos :: Ord a => a -> ABB a -> Bool
mayor_a_todos a VacioABB = True
mayor_a_todos a (RamaABB m c) = a > c && mayor_a_todos a m

menor_a_todos :: Ord a => a -> ABB a -> Bool
menor_a_todos a VacioABB = True
menor_a_todos a (RamaABB m c) = a < c && menor_a_todos a m

verificarABB :: Ord a => (a -> ABB a -> Bool) -> a -> ABB a -> Bool
verificarABB f a (RamaABB m c) = f a (RamaABB m c)

-- Al definir verificarABB de esta forma puedo saber que estoy verificando exactamente.
-- Sea un arbol
{- ejemplo1 = RamaABB ( RamaABB VacioABB 10 VacioABB ) 2 ( RamaABB VacioABB 11 VacioABB )
Este ejemplo tendría sentido si nuestro tipo recursivo fuera de la forma:
data ABB a = RamaABB (ABB a) a (ABB a) | VacioABB deriving Show
donde 
RamaABB :: ABB a -> a -> ABB a -> ABB a, lo cual difiere de nuestro tipo recursivo.

*Main> verificarABB mayor_a_todos 15 (RamaABB (RamaABB VacioABB 10) 11)
True
*Main> verificarABB menor_a_todos 15 (RamaABB (RamaABB VacioABB 10) 11)
False
-}
data ERR a = RamaERR (ERR a) a (ERR a) | VacioERR deriving Show
ejemplo1 = RamaERR ( RamaERR VacioERR 10 VacioERR ) 2 ( RamaERR VacioERR 11 VacioERR )