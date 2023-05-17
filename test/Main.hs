module Main where

import Test.HUnit
import TestCatedra
import TestEquipo

main :: IO ()
main = do
	putStrLn "Corriendo pruebas"
	runTestTT testsCatedra
	runTestTT testsEquipo
	putStrLn "Pruebas finalizadas"
