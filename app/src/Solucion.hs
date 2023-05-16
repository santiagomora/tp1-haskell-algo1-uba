module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- listadoDeNombreses basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-----------------
-- Ejercicio 1 --
-----------------

-- Primero se obtiene la lista de usuarios. Luego, por recursion se van obteniendo los nombres y
-- se los va agregando a una lista. Se utilizan las funciones basicas usuarios y nombreDeUsuarios.

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = listadoDeNombres (usuarios red)

listadoDeNombres :: [Usuario] -> [String]
listadoDeNombres [] = [] 
listadoDeNombres (x:xs) = nombreDeUsuario x : listadoDeNombres xs 


-----------------
-- Ejercicio 2 --
-----------------

-- Se usa una funcion auxiliar que indica si un usuario es parte de una relacion. En caso de que sea parte, se agrega dicha relacion a 
-- una lista.

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = listaDeAmigos (relaciones red) u

listaDeAmigos :: [Relacion] -> Usuario -> [Usuario]
listaDeAmigos [] _ = []
listaDeAmigos (rel:rels) u | elUsuarioPerteneceALaRelacion = amigoDelUsuarioIngresado rel u : listaDeAmigos rels u
                           | otherwise = listaDeAmigos rels u
                           where elUsuarioPerteneceALaRelacion = fst rel == u || snd rel == u

amigoDelUsuarioIngresado :: Relacion -> Usuario -> Usuario
amigoDelUsuarioIngresado (us1, us2) u | us1 == u = us2 
                                      | us2 == u = us1


-----------------
-- Ejercicio 3 --
-----------------

-- Para resolver este ejericio se va a usar la funcion amigosDe que devuelve la lista de amigos.
-- Luego se usa una funcion para contar la cantidad de amigos.

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = sumaElementosDeLista (amigosDe red u)


sumaElementosDeLista :: [Usuario] -> Int
sumaElementosDeLista [] = 0
sumaElementosDeLista (u:us) = 1 + sumaElementosDeLista us


-----------------
-- Ejercicio 4 --
-----------------

-- Se accede a los usuarios de la red con la funcion basica usuarios. Se usa la funcion cantidadDeAmigos y se forman tuplas con el usuario 
-- y la cantidad de amigos. Se agregan en una lista y luego se determina cual es la tupla con mas amigos. Luego se proyecta el usuario es esta.

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = fst (mayorCantidadAmigos (conteoDeAmigos (usuarios red) red))


conteoDeAmigos :: [Usuario] -> RedSocial -> [(Usuario, Int)]
conteoDeAmigos [] red = []
conteoDeAmigos (x:xs) red = (x, cantidadDeAmigos red x) : conteoDeAmigos xs red


mayorCantidadAmigos :: [(Usuario, Int)] -> (Usuario, Int) -- Devuelve la tupla con la mayor segunda componente
mayorCantidadAmigos (x:[]) = x
mayorCantidadAmigos (x:xs) = mayorCantidadAmigos (mayor x (head xs) : tail xs)


mayor :: (Usuario, Int) -> (Usuario, Int) -> (Usuario, Int) -- Compara dos tuplas y devuelve la que tiene segunda componente mas grande
mayor (us1, a) (us2, b) | a >= b = (us1, a)
                        | a < b = (us2, b)


-----------------
-- Ejercicio 5 --
-----------------

-- La siguiente funcion indica si hay algun usuario con mas de 10 de amigos.
-- Utilizamos la funcion mayorCantidadAmigos y proyectamos la cantidad. Luego se determina si esta es mayor que diez.

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos red = snd (mayorCantidadAmigos (conteoDeAmigos (usuarios red) red)) > 10


-----------------
-- Ejercicio 6 --
-----------------

-- Con la funcion basica publicaciones se accede a las publicaciones de la red. Se usa una funcion auxiliar que indica si la publicacion es
-- del usuario ingresado. Con otra funcion con recursion sobre listas, si la publicacion es del usuario, se va agregando a una lista.

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = filtrarPublicaciones (publicaciones red) u


filtrarPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPublicaciones [] _ = []
filtrarPublicaciones (pub:pubs) u | pub `esLaPublicacionDe` u = pub : filtrarPublicaciones pubs u
                                  | otherwise = filtrarPublicaciones pubs u

esLaPublicacionDe :: Publicacion -> Usuario -> Bool
esLaPublicacionDe (us1, _, _) u = us1 == u 
                         

-----------------
-- Ejercicio 7 --
-----------------

-- Se usan dos funciones que usan recursion sobre listas. Una que indica cuando al usuario le gusta una publicacion. 
-- Otra funcion que agregue la publicacion a una lista cuando le gusta al usuario. 

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_,[],_) u = []
publicacionesQueLeGustanA red u = publicacionesDeInteres (publicaciones red) u


publicacionesDeInteres :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeInteres [] u = []
publicacionesDeInteres (pub:pubs) u | aUsuarioLeGustaPub = pub : publicacionesDeInteres pubs u
                                    | otherwise = publicacionesDeInteres pubs u
                                    where aUsuarioLeGustaPub = u `perteneceA` (likesDePublicacion pub)

perteneceA :: Usuario -> [Usuario] -> Bool
perteneceA u [] = False
perteneceA u (x:xs) | x == u = True
                     | x /= u = perteneceA u xs


-----------------
-- Ejercicio 8 --
-----------------

-- Se usa la funcion publicacionesQueLeGustanA aplicada a los usuarios u1 y u2. Se compara la devolucion de estas funciones para ver 
-- si son iguales

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2
                                          
-----------------
-- Ejercicio 9 --
-----------------

-- Si un usuario tiene un seguidor fiel, a este, en particular le gusta la primer publicacion. Por eso, se toman los usuarios a los que
-- les gusta la primer publicacion y despues se verifica si les gustan las otras publicaciones del usuario indicado.

-- tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
-- tieneUnSeguidorFiel red u = hayAlgunSeguidorFiel (publicacionesDe red u) (seguidoresPrimerPublicacion red u)
--
--
-- seguidoresPrimerPublicacion :: RedSocial -> Usuario -> [Usuario]
-- seguidoresPrimerPublicacion red u | publicacionesDe red u == [] = []
--                                   | otherwise = likesDePublicacion (head (publicacionesDe red u))
--
-- hayAlgunSeguidorFiel :: [Publicacion] -> [Usuario] -> Bool
-- hayAlgunSeguidorFiel _ [] = False
-- hayAlgunSeguidorFiel pubs (us:uss) | us `leGustanTodas` pubs = True
--                                    | otherwise = hayAlgunSeguidorFiel pubs uss
--
-- leGustanTodas :: Usuario -> [Publicacion] -> Bool
-- leGustanTodas usuario [] = True
-- leGustanTodas usuario (pub:pubs) | aUsuarioLeGustaPub = leGustanTodas usuario pubs
--                                  | otherwise = False
--                                  where aUsuarioLeGustaPub = usuario `perteneceA` (likesDePublicacion pub)


------------------
-- Ejercicio 10 --
------------------

-- Existe una secuencia de amigos entre dos usuarios si estos se relacionan entre si, la lista en ese caso podria ser [u1, u2]. Tambien 
-- existiria una secuencia si tienen un amigo en comun (u), en ese caso la lista seria del tipo [u1, u, us2]


-- existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
-- existeSecuenciaDeAmigos red u1 u2 | seRelacionanEntreSi = True
--                                   | tienenAmigosEnComun = True
--                                   | otherwise = False
--                                   where seRelacionanEntreSi = relacionados (relaciones red) u1 u2
--                                         tienenAmigosEnComun = amigosEnComun (amigosDe red u1) (amigosDe red u2)
--
--
-- relacionados :: [Relacion] -> Usuario -> Usuario -> Bool
-- relacionados [] u1 u2 = False
-- relacionados (rel:rels) u1 u2 | rel == (u1, u2) || rel == (u1, u2) = True
--                               | otherwise = relacionados rels u1 u2
--
--
-- dentroDeUnaLista :: [Usuario] -> Usuario -> Bool
-- dentroDeUnaLista [] _ = False
-- dentroDeUnaLista (us:uss) u | us == u = True
--                             | otherwise = dentroDeUnaLista uss u
--
-- amigosEnComun :: [Usuario] -> [Usuario] -> Bool
-- amigosEnComun l [] = False
-- amigosEnComun l (u:us) | dentroDeUnaLista l u = True
--                        | otherwise = amigosEnComun l us

-------------------------------
-- PROPUESTA EJERCICIOS 9 Y 10
-------------------------------
pertenece :: (Eq t) => [t] -> t -> Bool
pertenece [] y = False
pertenece (x:xs) y | x == y = True
                   | otherwise = pertenece xs y
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
-- Ejercicio 8: decide si a dos usuarios les gustan las mismas publicaciones
interseccion :: (Eq t) => [t] -> [t] -> [t]
interseccion [] ys = []
interseccion (x:xs) ys | pertenece ys x = [x] ++ interseccion xs ys
                       | otherwise = interseccion xs ys

-- Ejercicio 9: decide si un usuario tiene un seguidor al que le gustan todas sus publicaciones
likesEnComun :: [Publicacion] -> [Usuario]
likesEnComun (p:[]) = likesDePublicacion p
likesEnComun (p:ps) = interseccion (likesDePublicacion p) (likesEnComun ps)
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs u = longitud (likesEnComun (publicacionesDe rs u)) > 0

-- Ejercicio 10: Indica si existe una cadena de relaciones con la cual desde el usuario 1 se llega al usuario 2
-- permutaciones
insertar :: t -> Int -> [t] -> [t]
insertar w 0 xs = [w] ++ xs
insertar w at (x:xs) = [x] ++ insertar w (at-1) xs
perm :: t -> [t] -> Int-> [[t]]
perm y xs 0 = [[y] ++ xs]
perm y xs l = [insertar y l xs] ++ perm y xs (l-1)
permarray :: t -> [[t]] -> [[t]]
permarray x [] = []
permarray x (a:ac) = perm x a (longitud a) ++ permarray x ac
_permutaciones :: [t] -> [[t]] -> [[t]]
_permutaciones [] ac = ac
_permutaciones (x:xs) ac = _permutaciones xs (permarray x ac)
permutaciones :: [t] -> [[t]]
permutaciones xs = _permutaciones xs [[]]
-- posicion de un elemento en una lista
posicion :: (Eq t) => [t] -> t -> Int
posicion (x:xs) y | x == y = 0
                  | otherwise = 1 + posicion xs y
-- trozo de una lista desde indice fr hasta indice to
slice :: [t] -> Int -> Int -> [t]
slice [] fr to = []
slice (x:xs) 0 0 = []
slice (x:xs) 0 to = [x] ++ slice xs 0 (to-1)
slice xs fr to = slice xs (fr-1) (to-1)
-- verifica que las relaciones todas las relaciones de la cadena de usuarios existan entre las Relaciones de la red social
relacionesEnCadena :: [Relacion] -> [Usuario] -> Bool
relacionesEnCadena rs (u:us) | longitud us>1 = (pertenece rs r1 || pertenece rs r2) && relacionesEnCadena rs us
                             | otherwise = (pertenece rs r1 || pertenece rs r2)
                             where r1 = (u, head us)
                                   r2 = (head us, u)
-- toma cadenas de amigos validas a partir de las permutaciones de usuarios
cadenaDeAmigos :: [Relacion] -> [[Usuario]] -> Usuario -> Usuario -> [Usuario]
cadenaDeAmigos rs [] u1 u2 = []
cadenaDeAmigos rs (a:as) u1 u2 | longitud s > 0 && relacionesEnCadena rs s = s
                               | otherwise = cadenaDeAmigos rs as u1 u2
                               where s = slice a (posicion a u1) (posicion a u2)
-- toma cadenas de amigos validas a partir de las permutaciones de usuarios
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2 = longitud (cadenaDeAmigos (relaciones rs) (permutaciones (usuarios rs)) u1 u2) > 0
