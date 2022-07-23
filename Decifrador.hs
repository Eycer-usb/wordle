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
    putStr "MENTEMAESTRA: "
    hFlush stdout
    input <- getLine
    let ans = map toUpper input
    let n = 6
    let noRepetir = obtenerNoRepetidos ans palabra
    modoDecifrador n ans palabra palabras noRepetir

-- Bucle de Ejecucion del Juego
modoDecifrador n ans palabra palabras noRepetir =
    if n > 0 && ans == "TTTTT" then do
        putStrLn "He Ganado el Juego"
    else if n > 0 && ans /= "TTTTT" then do
        let (adivinacion, newPalabras) = (adivinarPalabra ans palabra palabras noRepetir)
        putStrLn ("DECIFRADOR: " ++ adivinacion)
        putStr "MENTEMAESTRA: "
        hFlush stdout
        input <- getLine
        let newAns = map toUpper input
        let noRepetirExt = noRepetir ++ (obtenerNoRepetidos newAns adivinacion)
        hFlush stdout
        hFlush stdout
        modoDecifrador (n-1) newAns adivinacion newPalabras noRepetirExt
    else do
        putStrLn "Felicidades, Has Ganado el Juego"

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
calificarPalabra word = sum $ map calificarLetra word

-- Calificar Arbol y formar
calificarYOrdenarLista  ::  [String] -> [(Float, String)]
calificarYOrdenarLista palabras = sort $ zip (map (calificarPalabra) palabras) palabras

-- Se verifica si en la posicion dada la palabra cumple la restriccion de Toro
filtrarToro ::  Char -> Int -> String ->  Bool
filtrarToro p pos palabra = length palabra > pos && palabra!!pos == p

-- Se verifica si en la posicion dada la palabra cumple la restriccion de Vaca
filtrarVaca :: Char -> Int -> String -> Bool
filtrarVaca p pos palabra = length palabra > pos && palabra!!pos /= p && p `elem` palabra


-- Recibo la respuesta del usuario, la palabra respondida y una palabra de la lista y verifico
-- Si la palabra actual cumple con las caracteristicas expresadas con el usuario
palabraValida  ::  String -> String -> Int -> String -> String -> Bool
palabraValida [] [] _ _ _ = True
palabraValida (a:as) (p:ps) pos palabra verificados |   a == 'T' = ( filtrarToro p pos palabra ) && (palabraValida as ps (pos + 1) palabra (verificados ++ [p]) )
                                                    |   a == 'V' = ( filtrarVaca p pos palabra ) && (palabraValida as ps (pos + 1) palabra (verificados ++ [p]) )
                                                    |   a == '-' = ( (not (elem p palabra)) || (elem p verificados) ) && (palabraValida as ps (pos + 1) palabra verificados)

-- Se poda el Arbol de opciones y se retorna el arbol con las opciones validas
filtrarNodos :: String -> String -> [String] -> String -> [String]
filtrarNodos ans wd palabras noRepetir  =   filter( \x -> (palabraValida ans wd 0 x []) && (sinRepetidos ans x noRepetir) ) palabras

-- Se retorna con mayor posibilidad de ser el correcto
adivinarPalabra::  String -> String -> [String] -> String -> (String, [String])
adivinarPalabra [] wd palabras noRepetir =  (wd, palabras)
adivinarPalabra ans wd palabras noRepetir = ( snd (head (calificarYOrdenarLista (filtrarNodos ans wd palabras noRepetir))), filtrarNodos ans wd palabras noRepetir)


-- Obtenemos los elementos que son toros o vacas y - al mismo tiempo
obtenerNoRepetidos ans wd = fst (unzip [ (x,y) | (x,y) <- (zip wd ans), y=='-' && (any (\(a,b) -> a==x && b=='V' ) (zip wd ans) )  ])


-- Verifica si una palabra dada contiene o no caracteres repetidos que no se deben repetir
sinRepetidos ans palabra noRepetir = sum [ 1 | x <- [0..((length palabra) - 1)], y <- [0..((length palabra)-1)], x /= y, palabra!!x == palabra!!y, ans!!x == 'V', ans!!y == '-', palabra!!x `elem` noRepetir ] == 0