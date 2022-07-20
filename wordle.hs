-- Wordle en Haskell
-- Si, En Haskell tambien se pueden hacer juegos

import System.Environment
import MenteMaestra
import Decifrador

-- Al iniciar el programa se selecciona el modo de juego
-- estos pueden se decifrador o mentemaestra
-- y se ejecutan de la siguiente forma:

-- $ ./wordle <modo>

ejecutarModo modo
    | modo == [ "mentemaestra" ]    =   MenteMaestra.ejecutar
    | modo == [ "decifrador" ]      =   Decifrador.ejecutar

bienvenido = putStrLn "\nBIENVENIDO A WORDLE!\n"

main = do

    args <- getArgs                  -- IO [String]
    bienvenido
    ejecutarModo args