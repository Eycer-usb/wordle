-- El Modo mente maestra consiste en 
-- buscar aleatoriamente obtener una palabra del archivo
-- Palabras.txt y durante 6 turnos termitirle al usuario
-- decifrar la palabra mientras se le va indicando cuales
-- de las letras de su adivinacion son toros y cuales son vacas

module MenteMaestra
(
    ejecutar
)   where

import System.IO
import System.Random
import Data.Char

-- Nombre del Archivo al que se recurre como diccionario
archivo = "Palabras.txt";

-- Se ejecuta el Modo Mente Maestra
ejecutar = do
    -- Se obtiene la palabra aleatoria del archivo
    -- Luego se inicia el el juego
    palabra <- obtenerPalabraAleatoria archivo
    -- Se inicia el juego
    let memoria = ""
    mentemaestra 6 6 palabra memoria


-- Esta funcion recibe como parametro la ruta al archivo
-- con las palabras de 5 letras y retorna una de estas
-- palabras al azar
obtenerPalabraAleatoria path = do
    ln <- listaDePalabras archivo
    let len = length ln
    randomIndex <- randomRIO ( 0, len-1 )
    let e = ln!!randomIndex
    return e


-- Esta funcion recibe una palabra al azar y un numero que representa
-- la cantidad de intentos permitidos para decifrar una palabra
-- inicia el juego en modo mente maestra
mentemaestra 0 n palabra _ = do putStrLn ("Perdiste, la palabra era: " ++ palabra )
mentemaestra i n palabra memoria = do 
    putStr "DESCIFRADOR:  "
    hFlush stdout
    input <- getLine
    let upper = map toUpper input
    isValid <- palabraValida(upper)
    if upper == palabra && isValid
    then do 
            let temp = memoria ++ "TTTTT\n"
            putStrLn ("Ganaste en el intento: " ++ show (n-i+1))
            putStrLn("Este es tu resultado:\n" ++ "\n" ++ temp)
    else if not isValid
        then do
        putStrLn "La palabra introducida no existe"
        mentemaestra i n palabra memoria
    else do
        let ans = "-----"
        resultado <- identificarTorosYVacas 5 0 palabra upper ans
        let temp = memoria ++ resultado ++ "\n"
        mentemaestra (i-1) n palabra temp


-- La siguiente funcion verifica si una palabra pertenece al
-- al archivo de palabras posibles
palabraValida palabra = do
    lista <- listaDePalabras archivo
    let ans = elem palabra lista
    return ans

-- Dado un string se le elimina el caracter \r en caso que lo tenga
limpiarString str | '\r' `elem` str = init(str)
                  | not( '\r' `elem` str ) = str

-- Dada una ruta retorna las lista de lineas del archivo
listaDePalabras path = do
    file <- readFile path
    let ln = lines file
    let limpio = map limpiarString ln
    return limpio

-- Dada una palabra retorna el string correspondiente a los toros y vacas
identificarTorosYVacas n i palabra input ans =
    if i == n 
    then do
        putStr "MENTEMAESTRA: "
        hFlush stdout
        putStrLn ans
        return ans
    else if palabra!!i == input!!i
    then do
        let ( cabeza, _:cola ) = splitAt i ans
        let aux = cabeza ++ "T" ++ cola
        identificarTorosYVacas n (i+1) palabra input aux
    else do
        auxiliar <- identificarVacas i 0 n palabra input ans
        identificarTorosYVacas n (i+1) palabra input auxiliar

-- Dada una cadena de caracteres retorna el string con 
-- unicamente las vacas correspondientes
identificarVacas i j n palabra input ans =
    if (j < n && palabra!!i == input!!j && j/=i && ans!!j == '-' )
    then do
            let ( cabeza, _:cola ) = splitAt j ans
            let aux = cabeza ++ "V" ++ cola
            return aux
    else if (j < n)
    then do
            aux <- identificarVacas i (j+1) n palabra input ans
            return aux
    else do
        return ans
