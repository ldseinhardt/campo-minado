module Auxiliar where

import Data.Char
import System.Random
import System.IO.Unsafe
import System.Cmd
import GHC.IO.Exception

-- Seleciona uma posição da matriz
(!) :: [[a]] -> (Int,Int) -> a
(!) m (i,j) = (m !! i) !! j

-- Coloca um valor na matriz
set :: [[a]] -> (Int,Int) -> a -> [[a]]
set m (i,j) k = setA m (i,j) k 0
	where
	setA :: [[a]] -> (Int,Int) -> a -> Int -> [[a]]
	setA [] _ _ _ = []
	setA (am:xm) (i,j) k c
		| c < i     = am:(setA xm (i,j) k (c+1))
		| otherwise = (setB am (i,j) k 0):xm
		where
		setB :: [a] -> (Int,Int) -> a -> Int -> [a]
		setB [] _ _ _ = []
		setB (am:xm) (i,j) k c
			| c < j     = am:(setB xm (i,j) k (c+1))
			| otherwise = k:xm

-- Cria uma matriz de Strings
matriz :: (Int,Int) -> String -> [[String]]
matriz (0,_) _ = []
matriz (n,m) k = (lista m k):(matriz (n-1,m) k)
	where
	lista :: Int -> String -> [String]
	lista 0 _ = []
	lista m k = k:(lista (m-1) k)

-- Converte matriz em string
matrizToString :: [[String]] -> String
matrizToString = foldr (\ h t -> (foldr (++) [] h) ++ "\n" ++ t) []

-- Números randonicos
random :: (Int,Int) -> Int
random = unsafePerformIO . randomRIO

-- Apaga a tela
apaga :: IO GHC.IO.Exception.ExitCode
apaga = system "clear"

-- http://en.wikipedia.org/wiki/ANSI_escape_code

ansi x = "\x1b["++(show x)++"m"

reset     = 0
baseColor = 30
black     = baseColor + 0
red       = baseColor + 1
green     = baseColor + 2
yellow    = baseColor + 3
blue      = baseColor + 4
magenta   = baseColor + 5
cyan      = baseColor + 6
gray      = baseColor + 7
