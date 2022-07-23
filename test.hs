import System.IO
import Data.List (sort)

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
filtrarNodos ans wd palabras noRepetir  =   filter( \x -> (palabraValida ans wd 0 x []) && (sinRepetidos x noRepetir) ) palabras

-- Se retorna con mayor posibilidad de ser el correcto
adivinarPalabra::  String -> String -> [String] -> String -> (String, [String])
adivinarPalabra [] wd palabras noRepetir =  (wd, palabras)
adivinarPalabra ans wd palabras noRepetir = ( snd (head (calificarYOrdenarLista (filtrarNodos ans wd palabras noRepetir))), filtrarNodos ans wd palabras noRepetir)



-- Obtenemos los elementos que son toros o vacas y - al mismo tiempo
obtenerNoRepetidos ans wd = fst (unzip [ (x,y) | (x,y) <- (zip wd ans), y=='-' && (any (\(a,b) -> a==x && b/='-' ) (zip wd ans) )  ])


-- Verifica si una palabra dada contiene o no caracteres que no se deben repetir
sinRepetidos palabra noRepetir = all (\x -> not( elem x noRepetir )) palabra