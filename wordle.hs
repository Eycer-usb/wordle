import System.Environment
import MenteMaestra
import Decifrador

ejecutarModo modo
    | modo == [ "mentemaestra" ]    =   MenteMaestra.ejecutar
    | modo == [ "decifrador" ]      =   Decifrador.ejecutar

bienvenido = putStrLn "\nBIENVENIDO A WORDLE!\n"


main = do

    args <- getArgs                  -- IO [String]
    bienvenido
    ejecutarModo args