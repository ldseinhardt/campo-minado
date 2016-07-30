module Campo.Minado where

import Data.Char
import System.Console.ANSI
import Auxiliar

-- Definição de tipos
type Jogador = String
type Celula  = String
type Linha   = [Celula]
type Tabela  = [Linha]
type Pos     = (Int, Int)
type Tam     = (Int, Int)
type Pontos  = Int

-- Definição de elementos do jogo
padrao, bomba, branco :: Celula
padrao = " "++(ansi red)++"*"++(ansi reset)++" "
bomba  = " "++(ansi yellow)++"@"++(ansi reset)++" "
branco = "   "

dica :: Int -> Celula
dica x = " "++(ansi cyan)++(show x)++(ansi reset)++" "

cursor :: Celula -> Celula
cursor x = (ansi green)++"["++(ansi reset)++(take ((length x)-2) (drop 1 x))++(ansi green)++"]"++(ansi reset)

-- Tamanho da matriz
tamanho :: Tam
tamanho = (10,15)

-- Matriz Principal do Jogo
tabela :: Tabela
tabela = matriz tamanho padrao

-- Pontuação
pontos :: Pontos
pontos = 0

-- Bombas
bombas :: [Pos]
bombas = geraBombas (div ((fst tamanho)*(snd tamanho)) 10)
	where
	geraBombas :: Int -> [Pos]
	geraBombas 0 = []
	geraBombas n = [(random (0, (fst tamanho)-1),random (0, (snd tamanho)-1))] ++ geraBombas (n-1)

-- Conta quantas bombas há em volta de uma posição
bombasEmVolta :: Pos -> Celula
bombasEmVolta (x,y)
	| resp == 0 = branco
	| otherwise = dica resp
		where
		resp :: Int
		resp = contaBombas [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y-1), (x,y+1), (x+1,y-1), (x+1,y), (x+1,y+1)]
		contaBombas :: [Pos] -> Int
		contaBombas [] = 0
		contaBombas (a:x) = length(filter (==a) bombas) + contaBombas x

-- Imprime tabelas
putTabela :: Tabela -> IO ()
putTabela = putStrLn . matrizToString

-- Titulo do jogo
titulo :: IO ()
titulo = putStrLn "\n *** Campo Minado *** \n"

-- Pega o nome do jogador
getJogador :: IO Jogador
getJogador = do
	putStrLn " Informe seu nome: \n"
	nome <- getLine
	if (nome /= "")
		then return nome
		else getJogador

-- Gera uma nova tabela, a partir de uma jogada
novaTabela :: Tabela -> Pos -> IO Tabela
novaTabela t p = do
		if (length(filter (==p) bombas) > 0)
			then return (set t p bomba)
			else return (set t p (bombasEmVolta p))
-- Estado do jogo
estado :: Pos -> IO Bool
estado p = do
		if (length(filter (==p) bombas) > 0)
			then return False
			else return True

-- loop do jogo
loop :: Bool -> Pos -> Jogador -> Pontos -> Tabela -> IO ()
loop jogar (x,y) jogador pontos tabela = do
	apaga
	titulo
	putStrLn (" Jogador: "++jogador++", Pontos: "++(show pontos)++"\n")
	if (jogar)
		then do
			putTabela (set tabela (x,y) (cursor (tabela ! (x,y))))
			putStrLn " Up/Down/Left/Right :: Para mover"
			putStrLn " Enter :: Seleciona uma posição"
			putStrLn " Space :: Sair\n"
			a <- getChar
			if ((ord a) == 10)
				then do
					if ((tabela ! (x,y)) /= padrao)
						then loop jogar (x,y) jogador pontos tabela
						else do
							e <- estado (x,y)
							t <- novaTabela tabela (x,y)
							if(e)
								then loop e (x,y) jogador (pontos+1) t
								else loop e (x,y) jogador pontos t
				else do
					if ((ord a) == 32)
						then putStrLn (" Jogo Finalizado.\n")
						else do
							b <- getChar
							c <- getChar
							let d = ord c
							case d of
								65 -> do -- up
									if ((x-1) < 0)
										then loop jogar (x,y) jogador pontos tabela
										else loop jogar (x-1,y) jogador pontos tabela

								66 -> do -- down
									if ((x+1) > ((fst tamanho)-1))
										then loop jogar (x,y) jogador pontos tabela
										else loop jogar (x+1,y) jogador pontos tabela
								67 -> do -- right
									if ((y+1) > ((snd tamanho)-1))
										then loop jogar (x,y) jogador pontos tabela
										else loop jogar (x,y+1) jogador pontos tabela
								68 -> do -- left
									if ((y-1) < 0)
										then loop jogar (x,y) jogador pontos tabela
										else loop jogar (x,y-1) jogador pontos tabela

								otherwise -> loop jogar (x,y) jogador pontos tabela
		else do
			putTabela tabela
			putStrLn " Fim de Jogo, você achou uma Bomba !!!\n"

-- Programa principal
main :: IO ()
main = do
	apaga
	titulo
	setTitle "Campo Minado"
	jogador <- getJogador
	loop True (0,0) jogador pontos tabela


-- Mostra a tabela com todas as bombas e dicas
showBombas :: IO ()
showBombas = putTabela (recX tabela (0,0))
	where
	recX :: Tabela -> Pos -> Tabela
	recX [] _ = []
	recX (a:l) (x,y) = (recY a (x,y)): recX l (x+1,y)
	recY :: Linha -> Pos -> Linha
	recY [] _ = []
	recY (a:l) (x,y) = (real (x,y)): recY l (x,y+1)
	real :: Pos -> Celula
	real (x,y)
		| length(filter (==(x,y)) bombas) > 0 = cursor bomba
		| otherwise = cursor (bombasEmVolta (x,y))
