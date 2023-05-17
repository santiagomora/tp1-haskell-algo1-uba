module TestEquipo where

import Test.HUnit
import Solucion

testsEquipo = test [ej1 , ej2, ej3, ej4, ej5, ej6, ej7, ej8, ej9, ej10]

ej1 = test [
    " Caso 1: No tiene usuarios" ~: (nombresDeUsuarios redV) ~?= [],
    " Caso 2: Tiene usuarios" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"]
 ]

ej2 = test [
    " Caso 1: La red no tiene relaciones" ~: (amigosDe redC usuario1) ~?= [],
    " Caso 2: El usuario tiene relaciones en la red" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    " Caso 3: El usuario no tiene relaciones en la red" ~: (amigosDe redB usuario5) ~?= []
 ]

ej3 = test [
    " Caso 1: La red no tiene relaciones" ~: (cantidadDeAmigos redC usuario1) ~?= 0,
    " Caso 2: El usuario tiene relaciones en la red" ~: (cantidadDeAmigos redA usuario1) ~?= 2,
    " Caso 3: El usuario no tiene relaciones en la red" ~: (cantidadDeAmigos redB usuario5) ~?= 0
 ]

ej4 = test [
    " Caso 1: No hay relaciones en la red" ~: (usuarioConMasAmigos redC) ~?= usuario1,
    " Caso 2: Hay relaciones en la red" ~: (usuarioConMasAmigos redA) ~?= usuario2
 ]

ej5 = test [
    " Caso 1: La red no tiene usuarios" ~: (estaRobertoCarlos redV) ~?= False,
    " Caso 2: En la red hay usuarios pero no relaciones" ~: (estaRobertoCarlos redC) ~?= False,
    " Caso 3: Hay usuarios y relaciones sin Roberto Carlos" ~: (estaRobertoCarlos redA) ~?= False,
    " Caso 4: Un usuario tiene mas de 10 relaciones" ~: (estaRobertoCarlos redD) ~?= True
 ]

ej6 = test [
    " Caso 1: Red sin publicaciones" ~: (publicacionesDe redC usuario1) ~?= [],
    " Caso 2: Red con publicaciones, usuario sin publicaciones" ~: (publicacionesDe redB usuario5) ~?= [],
    " Caso 3: Red y usuario con publicaciones" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2]
 ]

ej7 = test [
    " Caso 1: Red sin publicaciones" ~: (publicacionesQueLeGustanA redC usuario1) ~?= [],
    " Caso 2: Al usuario no le gusta ninguna de las publicaciones" ~: (publicacionesQueLeGustanA redA usuario3) ~?= [],
    " Caso 3: Al usuario le gusta alguna de las publicaciones" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1]
 ]

ej8 = test [
    " Caso 1: La red no tiene publicaciones" ~: (lesGustanLasMismasPublicaciones redC usuario1 usuario2) ~?= True,
    " Caso 2: No les gusta ninguna de las publicaciones" ~: (lesGustanLasMismasPublicaciones redE usuario1 usuario3) ~?= True,
    " Caso 3: No les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redA usuario1 usuario2) ~?= False,
    " Caso 4: Les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True
 ]

ej9 = test [
    " Caso 1: La red no tiene publicaciones" ~: (tieneUnSeguidorFiel redC usuario2) ~?= False,
    " Caso 2: El usuario no tiene publicaciones con me gusta" ~: (tieneUnSeguidorFiel redE usuario1) ~?= False,
    " Caso 3: Tiene seguidores, pero no fieles" ~: (tieneUnSeguidorFiel redA usuario3) ~?= False,
    " Caso 4: Tiene seguidor fiel" ~: (tieneUnSeguidorFiel redA usuario2) ~?= True
 ]

ej10 = test [
    " Caso 1: La red no tiene relaciones" ~: (existeSecuenciaDeAmigos redC usuario1 usuario2) ~?= False,
    " Caso 2: Los usuarios son amigos" ~: (existeSecuenciaDeAmigos redE usuario1 usuario3) ~?= True,
    " Caso 3: No son amigos ni tienen amigos en comun" ~: (existeSecuenciaDeAmigos redB usuario1 usuario5) ~?= False,
    " Caso 4: No son amigos pero tienen amigos en comun" ~: (existeSecuenciaDeAmigos redB usuario1 usuario3) ~?= True
 ]

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Nicolas")
usuario7 = (7, "Anabella")
usuario8 = (8, "Daniel")
usuario9 = (9, "Soledad")
usuario10 = (10, "Rosario")
usuario11 = (11, "Pablo")
usuario12 = (12, "Estefania")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1)
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)


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

usuariosC = [usuario1, usuario2]
redC = (usuariosC, [], [])

usuariosD = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesD = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11, relacion1_12]
publicacionesD = [publicacion1_3, publicacion1_4]
redD = (usuariosD, relacionesD, publicacionesD)

usuariosE = [usuario1, usuario3]
relacionesE = [relacion1_3]
publicacionesE = [publicacion1_4, publicacion3_1]
redE = (usuariosE, relacionesE, publicacionesE)

redV = ([],[],[])

-----------------
-- Ejercicio 1 --
-----------------
-- Primero se obtiene la lista de usuarios. Luego, por recursion se van obteniendo los nombres y
-- se los va agregando a una lista. Se utilizan las funciones basicas usuarios y nombreDeUsuarios.
-- Para testeo segun especificacion:
-- NOTE Testing depende de correctitud funciones auxiliares (listadoDeNombres)
-- nombresDeUsuarios :: RedSocial -> [String]
-- nombresDeUsuarios red = listadoDeNombres (usuarios red)
--
-- -- Para testeo segun especificacion:
-- -- 1. asegura: que los nombres de parametro usuario aparezcan en la respuesta
-- -- 2. asegura: que no haya repetidos en la respuesta de la funcion FIXME
-- -- 3. asegura: que los nombres en la respuesta correspondan a usuarios recibidos como parametro de entrada
-- listadoDeNombres :: [Usuario] -> [String]
-- listadoDeNombres = undefined
--
-- -----------------
-- -- Ejercicio 2 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE Testing depende de correctitud funciones auxiliares (listaDeAmigos, amigoDelUsuarioIngresado)
-- -- 1. asegura: que no haya repetidos en la respuesta de la funcion FIXME
-- amigosDe :: RedSocial -> Usuario -> [Usuario]
-- amigosDe = undefined
--
-- -- Para testeo segun especificacion:
-- -- 1. asegura: si un usuario aparece en la respuesta, entonces existe una relacion en la red social que lo vincula con el usuario de entrada.
-- listaDeAmigos :: [Relacion] -> Usuario -> [Usuario]
-- listaDeAmigos = undefined
--
-- -- Para testeo segun especificacion:
-- -- 1. requiere: que el usuario este en la relacion
-- -- 2. asegura: si el usuario de entrada es el primer miembro de una relacion, entonces segundo miembro aparece en la respuesta
-- -- 3. asegura: si el usuario de entrada es el segundo miembro de una relacion, entonces el primer miembro aparece en la respuesta
-- amigoDelUsuarioIngresado :: Relacion -> Usuario -> Usuario
-- amigoDelUsuarioIngresado = undefined
--
-- -----------------
-- -- Ejercicio 3 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE Testing depende de correctitud de funcion (amigosDe) y funciones auxiliares (sumaElementosDeLista)
-- -- asegura: retorna la longitud de la lista de amigos de un usuario
-- cantidadDeAmigos :: RedSocial -> Usuario -> Int
-- cantidadDeAmigos = undefined
--
-- -- Para testeo segun especificacion:
-- -- asegura: retorna la longitud de una lista
-- sumaElementosDeLista :: [Usuario] -> Int
-- sumaElementosDeLista = undefined
--
-- -----------------
-- -- Ejercicio 4 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE Testing depende de correctitud funciones auxiliares (conteoDeAmigos, mayorCantidadAmigos)
-- usuarioConMasAmigos :: RedSocial -> Usuario
-- usuarioConMasAmigos = undefined
--
-- -- Para testeo segun especificacion:
-- -- NOTE Testing depende de correctitud de funciones (cantidadDeAmigos)
-- -- asegura: cantidad de amigos es segundo elemento en cada tupla de la respuesta
-- conteoDeAmigos :: [Usuario] -> RedSocial -> [(Usuario, Int)]
-- conteoDeAmigos = undefined
--
-- -- Para testeo segun especificacion:
-- -- NOTE: depende de correctitud funcion auxiliar (mayor)
-- -- asegura: el primer elemento de la tupla respuesta es un usuario de la red social
-- -- asegura: el segundo elemento de la tupla es la cantidad de amigos del usuario
-- -- asegura: el segundo elemento de la tupla es mayor o igual a la cantidad de amigos de los otros usuarios
-- mayorCantidadAmigos :: [(Usuario, Int)] -> (Usuario, Int) -- Devuelve la tupla con la mayor segunda componente
-- mayorCantidadAmigos = undefined
--
-- -- Para testeo segun especificacion:
-- -- asegura: el segundo elemento de la tupla respuesta es el mayor entre los segundos elementos de las tuplas de entrada
-- mayor :: (Usuario, Int) -> (Usuario, Int) -> (Usuario, Int) -- Compara dos tuplas y devuelve la que tiene segunda componente mas grande
-- mayor = undefined
--
-- -----------------
-- -- Ejercicio 5 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE: depende de correctitud funcion auxiliar (mayorCantidadAmigos, conteoDeAmigos)
-- -- asegura: true si y solo si existe un usuario con mas de diez amigos
-- estaRobertoCarlos :: RedSocial -> Bool
-- estaRobertoCarlos = undefined
--
-- -----------------
-- -- Ejercicio 6 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE depende de correctitud funciones (filtrarPublicaciones)
-- -- asegura: no hay publicaciones repetidas FIXME
-- publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
-- publicacionesDe = undefined
--
-- -- Para testeo segun especificacion:
-- -- NOTE depende de correctitud funciones (esLaPublicacionDe)
-- -- asegura: el usuario de todas las publicaciones de salida es el mismo que el usuario de entrada
-- -- asegura: todas las publicaciones de salida estan en la red social
-- filtrarPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
-- filtrarPublicaciones = undefined
--
-- -- Para testeo segun especificacion:
-- -- asegura: true si y solo si el usuario de la publicacion coincide con el del parametro
-- esLaPublicacionDe :: Publicacion -> Usuario -> Bool
-- esLaPublicacionDe = undefined
--
-- -----------------
-- -- Ejercicio 7 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE correctitud depende de funciones auxiliares (publicacionesDeInteres)
-- -- asegura: todas las publicaciones de salida pertenecen a la red social
-- -- asegura: no hay publicaciones repetidas FIXME
-- publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
-- publicacionesQueLeGustanA = undefined
--
-- -- Para testeo segun especificacion:
-- -- NOTE correctitud depende de funciones auxiliares (perteneceA)
-- -- asegura: para todas las publicaciones de salida, el usuario de entrada este entre los usuarios a los que les gusto la publicacion
-- publicacionesDeInteres :: [Publicacion] -> Usuario -> [Publicacion]
-- publicacionesDeInteres = undefined
--
-- -- Para testeo segun especificacion:
-- -- asegura: true si y solo si el usuario u existe entre los usuarios de entrada
-- perteneceA :: Usuario -> [Usuario] -> Bool
-- perteneceA = undefined
--
-- -----------------
-- -- Ejercicio 8 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE correctitud depende de funciones (publicacionesQueLeGustanA)
-- -- NOTE no se si haskell compare dos arreglos con ==, habria que verificarlo, parece que si
-- lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
-- lesGustanLasMismasPublicaciones = undefined
--
-- -----------------
-- -- Ejercicio 9 --
-- -----------------
-- -- Para testeo segun especificacion:
-- -- NOTE correctitud depende de funciones (publicacionesDe)
-- -- FIXME aqui hay que intersectar todos los usuarios que le dieron me gusta a las publicaciones del usuario y ver si hay alguno
-- tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
-- tieneUnSeguidorFiel = undefined
--
-- ------------------
-- -- Ejercicio 10 --
-- ------------------
-- -- Para testeo segun especificacion:
-- -- FIXME aqui hay que buscar las permutaciones de usuarios en la red social y ver si se da la cadena de relaciones de la especificacion. Es decir, encontrar una cadena de relaciones de n miembros que pueda llevarme desde el usuario A hasta el usuario B
-- existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
-- existeSecuenciaDeAmigos = undefined
