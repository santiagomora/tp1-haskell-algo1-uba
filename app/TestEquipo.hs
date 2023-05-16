module TestEquipo where

import Test.HUnit
import Solucion
-----------------
-- Ejercicio 1 --
-----------------
-- Primero se obtiene la lista de usuarios. Luego, por recursion se van obteniendo los nombres y
-- se los va agregando a una lista. Se utilizan las funciones basicas usuarios y nombreDeUsuarios.
-- Para testeo segun especificacion:
-- NOTE Testing depende de correctitud funciones auxiliares (listadoDeNombres)
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = listadoDeNombres (usuarios red)

-- Para testeo segun especificacion:
-- 1. asegura: que los nombres de parametro usuario aparezcan en la respuesta
-- 2. asegura: que no haya repetidos en la respuesta de la funcion FIXME
-- 3. asegura: que los nombres en la respuesta correspondan a usuarios recibidos como parametro de entrada
listadoDeNombres :: [Usuario] -> [String]
listadoDeNombres = undefined

-----------------
-- Ejercicio 2 --
-----------------
-- Para testeo segun especificacion:
-- NOTE Testing depende de correctitud funciones auxiliares (listaDeAmigos, amigoDelUsuarioIngresado)
-- 1. asegura: que no haya repetidos en la respuesta de la funcion FIXME
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- Para testeo segun especificacion:
-- 1. asegura: si un usuario aparece en la respuesta, entonces existe una relacion en la red social que lo vincula con el usuario de entrada.
listaDeAmigos :: [Relacion] -> Usuario -> [Usuario]
listaDeAmigos = undefined

-- Para testeo segun especificacion:
-- 1. requiere: que el usuario este en la relacion
-- 2. asegura: si el usuario de entrada es el primer miembro de una relacion, entonces segundo miembro aparece en la respuesta
-- 3. asegura: si el usuario de entrada es el segundo miembro de una relacion, entonces el primer miembro aparece en la respuesta
amigoDelUsuarioIngresado :: Relacion -> Usuario -> Usuario
amigoDelUsuarioIngresado = undefined

-----------------
-- Ejercicio 3 --
-----------------
-- Para testeo segun especificacion:
-- NOTE Testing depende de correctitud de funcion (amigosDe) y funciones auxiliares (sumaElementosDeLista)
-- asegura: retorna la longitud de la lista de amigos de un usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- Para testeo segun especificacion:
-- asegura: retorna la longitud de una lista
sumaElementosDeLista :: [Usuario] -> Int
sumaElementosDeLista = undefined

-----------------
-- Ejercicio 4 --
-----------------
-- Para testeo segun especificacion:
-- NOTE Testing depende de correctitud funciones auxiliares (conteoDeAmigos, mayorCantidadAmigos)
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- Para testeo segun especificacion:
-- NOTE Testing depende de correctitud de funciones (cantidadDeAmigos)
-- asegura: cantidad de amigos es segundo elemento en cada tupla de la respuesta
conteoDeAmigos :: [Usuario] -> RedSocial -> [(Usuario, Int)]
conteoDeAmigos = undefined

-- Para testeo segun especificacion:
-- NOTE: depende de correctitud funcion auxiliar (mayor)
-- asegura: el primer elemento de la tupla respuesta es un usuario de la red social
-- asegura: el segundo elemento de la tupla es la cantidad de amigos del usuario
-- asegura: el segundo elemento de la tupla es mayor o igual a la cantidad de amigos de los otros usuarios
mayorCantidadAmigos :: [(Usuario, Int)] -> (Usuario, Int) -- Devuelve la tupla con la mayor segunda componente
mayorCantidadAmigos = undefined

-- Para testeo segun especificacion:
-- asegura: el segundo elemento de la tupla respuesta es el mayor entre los segundos elementos de las tuplas de entrada
mayor :: (Usuario, Int) -> (Usuario, Int) -> (Usuario, Int) -- Compara dos tuplas y devuelve la que tiene segunda componente mas grande
mayor = undefined

-----------------
-- Ejercicio 5 --
-----------------
-- Para testeo segun especificacion:
-- NOTE: depende de correctitud funcion auxiliar (mayorCantidadAmigos, conteoDeAmigos)
-- asegura: true si y solo si existe un usuario con mas de diez amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-----------------
-- Ejercicio 6 --
-----------------
-- Para testeo segun especificacion:
-- NOTE depende de correctitud funciones (filtrarPublicaciones)
-- asegura: no hay publicaciones repetidas FIXME
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- Para testeo segun especificacion:
-- NOTE depende de correctitud funciones (esLaPublicacionDe)
-- asegura: el usuario de todas las publicaciones de salida es el mismo que el usuario de entrada
-- asegura: todas las publicaciones de salida estan en la red social
filtrarPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPublicaciones = undefined

-- Para testeo segun especificacion:
-- asegura: true si y solo si el usuario de la publicacion coincide con el del parametro
esLaPublicacionDe :: Publicacion -> Usuario -> Bool
esLaPublicacionDe = undefined

-----------------
-- Ejercicio 7 --
-----------------
-- Para testeo segun especificacion:
-- NOTE correctitud depende de funciones auxiliares (publicacionesDeInteres)
-- asegura: todas las publicaciones de salida pertenecen a la red social
-- asegura: no hay publicaciones repetidas FIXME
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- Para testeo segun especificacion:
-- NOTE correctitud depende de funciones auxiliares (perteneceA)
-- asegura: para todas las publicaciones de salida, el usuario de entrada este entre los usuarios a los que les gusto la publicacion
publicacionesDeInteres :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeInteres = undefined

-- Para testeo segun especificacion:
-- asegura: true si y solo si el usuario u existe entre los usuarios de entrada
perteneceA :: Usuario -> [Usuario] -> Bool
perteneceA = undefined

-----------------
-- Ejercicio 8 --
-----------------
-- Para testeo segun especificacion:
-- NOTE correctitud depende de funciones (publicacionesQueLeGustanA)
-- NOTE no se si haskell compare dos arreglos con ==, habria que verificarlo, parece que si
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-----------------
-- Ejercicio 9 --
-----------------
-- Para testeo segun especificacion:
-- NOTE correctitud depende de funciones (publicacionesDe)
-- FIXME aqui hay que intersectar todos los usuarios que le dieron me gusta a las publicaciones del usuario y ver si hay alguno
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

------------------
-- Ejercicio 10 --
------------------
-- Para testeo segun especificacion:
-- FIXME aqui hay que buscar las permutaciones de usuarios en la red social y ver si se da la cadena de relaciones de la especificacion. Es decir, encontrar una cadena de relaciones de n miembros que pueda llevarme desde el usuario A hasta el usuario B
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
