module Decifrador
(
    ejecutar
)   where

import System.IO
import Data.List (sort)
import System.Random
import Data.Char



-- Nombre del Archivo al que se recurre como diccionario de palabras
archivo = "Palabras.txt"

-- Dado un string se le elimina el caracter \r en caso que lo tenga
limpiarString str | '\r' `elem` str = init(str)
                  | not( '\r' `elem` str ) = str


-- Dada una ruta retorna las lista de lineas del archivo
listaDePalabras path = do
    file <- readFile path
    let ln = lines file
    let limpio = map limpiarString ln
    return limpio

-- Esta funcion recibe como parametro la ruta al archivo
-- con las palabras de 5 letras y retorna una de estas
-- palabras al azar
obtenerPalabraAleatoria path = do
    ln <- listaDePalabras archivo
    let len = length ln
    randomIndex <- randomRIO ( 0, len-1 )
    let e = ln!!randomIndex
    return e

-- Realiza los preparativos de la ejecucion
ejecutar = do
    palabra <- obtenerPalabraAleatoria archivo
    palabras <- listaDePalabras archivo
    putStrLn ("DECIFRADOR: " ++ palabra)
    let n = 6
    modoDecifrador n "" palabra palabras 1.0

-- Bucle de Ejecucion del Juego
modoDecifrador n ans palabra palabras calificacion =
    if n > 0 && ans == "TTTTT" then do
        putStrLn "He Ganado el Juego"
    else if n > 0 && ans /= "TTTTT" then do
        putStr "MENTEMAESTRA: "
        hFlush stdout
        input <- getLine
        let newAns = map toUpper input
        let newCalificacion = calificarRespuesta newAns
        let (adivinacion, newPalabras) = (adivinarPalabra newAns palabra palabras)
        if  newCalificacion > calificacion || length adivinacion <= 0 then do
            putStrLn "Tramposo"
            hFlush stdout
        else do
            putStrLn ("DECIFRADOR: " ++ adivinacion)
            hFlush stdout
            modoDecifrador (n-1) newAns adivinacion newPalabras newCalificacion

    else if (length palabra == 0) then do
        putStrLn "Tramposo"
    else do
        putStrLn "Felicidades, Has Ganado el Juego"

-- Calificar Letra de la respuesta obtenida del usuario
calificarLetraRespuesta :: Char -> Float
calificarLetraRespuesta x   |   x=='T'  =   0.2
                            |   x=='V'  =   0.1
                            |   x=='-'  =   0.0

-- Calificar Respueta
calificarRespuesta  ::  String  ->  Float
calificarRespuesta  palabra =   1 - (sum (map calificarLetraRespuesta palabra) ) 


-- Dada una letra retorna la calificacion correspondiente a dicha letra
calificarLetra :: Char -> Float
calificarLetra x    | x `elem` "AE"      =  0.1
                    | x `elem` "INORS"   =  0.2 
                    | x `elem` "DLCTU"   =  0.3 
                    | x `elem` "BGMP"    =  0.5 
                    | x `elem` "FHQVY"   =  0.8 
                    | x `elem` "JKWXZ"   =  1.0
-- Se retorna la calificacion de una palabras
calificarPalabra :: String -> Float
calificarPalabra word = sum (map calificarLetra word)

-- Calificar Arbol y formar
calificarYOrdenarLista  ::  [String] -> [(Float, String)]
calificarYOrdenarLista palabras = sort( zip (map (calificarPalabra) palabras) palabras )

-- Se verifica si en la posicion dada la palabra cumple la restriccion de Toro
filtrarToro ::  Char -> Int -> String ->  Bool
filtrarToro p pos palabra = palabra!!pos == p

-- Se verifica si en la posicion dada la palabra cumple la restriccion de Vaca
filtrarVaca :: Char -> Int -> String -> Bool
filtrarVaca p pos palabra = ((palabra!!pos) /= p) && (p `elem` palabra)

-- Se obtiene lista de caracteres que corresponden a Toros o Vacas
obtenerTorosYVacas ans wd = fst $ unzip (filter (\(x,y) -> y == 'T' || y == 'V') (zip wd ans))

-- Se verifica si la letra asociada al guion elimina posibilidades o esta asociado a 
-- un toro o una vaca
filtrarGuion p pos ans wd palabra = ((p `elem` (obtenerTorosYVacas ans wd) ) && ( palabra!!pos /= p )) || not ( p `elem` palabra)

-- Recibo la respuesta del usuario, la palabra respondida y una palabra de la lista y verifico
-- Si la palabra actual cumple con las caracteristicas expresadas por el usuario
palabraValida  ::  String -> String -> Int -> String -> String -> String -> Bool
palabraValida [] [] _ _ _ _ = True
palabraValida (a:as) (p:ps) pos palabra ans wd |   a == 'T' = ( filtrarToro p pos palabra ) && (palabraValida as ps (pos + 1) palabra ans wd )
                                                    |   a == 'V' = ( filtrarVaca p pos palabra ) && (palabraValida as ps (pos + 1) palabra ans wd )
                                                    |   a == '-' = ( filtrarGuion p pos ans wd palabra ) && (palabraValida as ps (pos + 1) palabra ans wd )
                                        
-- Se poda el Arbol de opciones y se retorna el arbol con las opciones validas
filtrarNodos :: String -> String -> [String] -> [String]
-- filtrarNodos ans wd palabras noRepetir  =   filter( \x -> (palabraValida ans wd 0 x ans wd) && (sinRepetidos ans x noRepetir) ) palabras
filtrarNodos ans wd palabras  =   filter( \x -> (palabraValida ans wd 0 x ans wd) ) palabras

-- Se retorna palabra con mayor posibilidad de ser el correcto y la nueva lista de palabras
adivinarPalabra::  String -> String -> [String] -> (String, [String])
adivinarPalabra ans wd palabras = ( snd ( miHead (calificarYOrdenarLista (filtrarNodos ans wd palabras))), filtrarNodos ans wd palabras)


miHead [] = (0.0,"")
miHead (x:xs) = x

-- -- Se obtienen las letras que sean Toros vacas y guion al mismo tiempo
-- toroVacaYGuion ans wd = fst (unzip [ (x,y) | (x,y) <- (zip wd ans), y=='-', (any (\((a,b), (c,d)) -> a==c && a==x && b=='T' && d=='V'  ) [ (i,j) | i <- (zip wd ans), j <- (zip wd ans)]) ])

-- -- Obtenemos los elementos que son toros o vacas y - al mismo tiempo
-- obtenerNoRepetidos ans wd = fst (unzip [ (x,y) | (x,y) <- (zip wd ans), y=='-' && (any (\(a,b) -> ((a==x && b=='V') || (a==x && b=='T') && not( a `elem` (toroVacaYGuion ans wd) ) ) ) (zip wd ans) )  ])


-- -- Verifica si una palabra dada contiene o no caracteres repetidos que no se deben repetir
-- sinRepetidos ans palabra noRepetir = sum [ 1 | x <- [0..((length palabra) - 1)], y <- [0..((length palabra)-1)], x /= y, palabra!!x == palabra!!y, palabra!!x `elem` noRepetir ] == 0