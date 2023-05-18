module Solucion where

import AuxiliaresCatedra

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

-- compara los usuarios de la red social apoyandose en la funcion conMasAmigos, que decide cual de los dos usuarios de entrada tiene mas amigos

conMasAmigos :: RedSocial -> Usuario -> Usuario -> Usuario
conMasAmigos rs u1 u2 | (cantidadDeAmigos rs u1) >= (cantidadDeAmigos rs u2) = u1
                      | otherwise = u2
--    implementacion usuario con mas amigos en la red social
_usuarioConMasAmigos :: [Usuario] -> RedSocial -> Usuario
_usuarioConMasAmigos (u:[]) rs = u
_usuarioConMasAmigos (u:us) rs = conMasAmigos rs u (_usuarioConMasAmigos us rs)
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos rs = _usuarioConMasAmigos (usuarios rs) rs

-----------------
-- Ejercicio 5 --
-----------------

-- La siguiente funcion indica si hay algun usuario con mas de 10 de amigos.
-- Utilizamos la funcion mayorCantidadAmigos y proyectamos la cantidad. Luego se determina si esta es mayor que diez.

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = (cantidadDeAmigos red (usuarioConMasAmigos red)) > 10


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

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = hayAlgunSeguidorFiel (publicacionesDe red u) (seguidoresPrimerPublicacion red u)


seguidoresPrimerPublicacion :: RedSocial -> Usuario -> [Usuario]
seguidoresPrimerPublicacion red u | publicacionesDe red u == [] = []
                                  | otherwise = likesDePublicacion (head (publicacionesDe red u))

hayAlgunSeguidorFiel :: [Publicacion] -> [Usuario] -> Bool
hayAlgunSeguidorFiel _ [] = False
hayAlgunSeguidorFiel pubs (us:uss) | us `leGustanTodas` pubs = True
                                   | otherwise = hayAlgunSeguidorFiel pubs uss

leGustanTodas :: Usuario -> [Publicacion] -> Bool
leGustanTodas usuario [] = True
leGustanTodas usuario (pub:pubs) | aUsuarioLeGustaPub = leGustanTodas usuario pubs
                                 | otherwise = False
                                 where aUsuarioLeGustaPub = usuario `perteneceA` (likesDePublicacion pub)

------------------
-- Ejercicio 10 --
------------------
-- Dado el primer elementos de una posible secuencia, busco los candidatos a segundos elementos. Estos van a ser los amigos del primero.
-- Asi voy avanzando en las siguientes posiciones quitando los usuarios ya evaluados y viendo si esta el usuario final.

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = recorrerSecuencia red (amigosDe red u1) u2 [u1]

recorrerSecuencia :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool         -- La lista todosLosAmigos puede tener usuarios repetidos.
recorrerSecuencia red [] b l = False                                                -- Se pueden eliminar con una funcion pero en este caso
recorrerSecuencia red a u2 l | u2 `perteneceA` a = True                             -- no se hizo porque funciona y para no hacer mas largo
                             | otherwise = siguientePosicion
                             where siguientePosicion = recorrerSecuencia red (quitaUsuariosYaEvaluados (todosLosAmigos red a) l) u2 (l ++ a)

todosLosAmigos :: RedSocial -> [Usuario] -> [Usuario]
todosLosAmigos _ [] = []
todosLosAmigos red (x:xs) = amigosDe red x ++ todosLosAmigos red xs

quitaUsuariosYaEvaluados :: [Usuario] -> [Usuario] -> [Usuario]
quitaUsuariosYaEvaluados [] l = []
quitaUsuariosYaEvaluados (x:xs) l | x `perteneceA` l = quitaUsuariosYaEvaluados xs l
                                  | otherwise = x : quitaUsuariosYaEvaluados xs l

