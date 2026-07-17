{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : José Pedro Fernandes da Costa <a109807@alunos.uminho.pt>
              Miguel Pedro Ribeiro Correia <a111773@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

-- | Função que analisa a validade de um jogo
validaJogo :: Jogo -> Bool
validaJogo j = verificaPortais lPortais base mapa
               && verificaI lInimigos mapa lPortais
               && verificaTorres lTorres mapa
               && verificaBase base lPortais mapa
               where
                lPortais = portaisJogo j
                ondasPortais = concat (map ondasPortal lPortais)
                base = baseJogo j
                lInimigos = inimigosJogo j ++ (concat (map inimigosOnda ondasPortais))
                lTorres = torresJogo j
                mapa = mapaJogo j


-- 1
--Verificar o estado dos portais
{- | A função verifica se um portal é válido ou não, recebendo uma lista de portais,
uma base e um mapa e acabando por retornar um Booliano.
-}
verificaPortais :: [Portal] -> Base -> Mapa -> Bool
verificaPortais [] _ _ = False
verificaPortais [p] b m = portalNaTerra (posicaoPortal p) terras                 -- não está na relva pois garantimos que está na terra
                          && not (portalSobBase (posicaoPortal p) (posicaoBase b))
                          && umaOndaAtiva (ondasPortal p)
                          && existeCaminhoValido m b p
                          where
                            terras = coordenadasTerra m
verificaPortais (p:ps) b m = portalNaTerra (posicaoPortal p) terras
                          && not (portalSobBase (posicaoPortal p) (posicaoBase b))
                          && umaOndaAtiva (ondasPortal p)
                          && existeCaminhoValido m b p
                          && verificaPortais ps b m
                          where
                            terras = coordenadasTerra m
                            
-- Função que verifica se os portais estão na terra
{- | A função verifica se um portal está na terra recebendo uma posição 
e uma lista de posicões, devolvendo True se o portal estiver na terra 
-}
portalNaTerra :: Posicao -> [Posicao] -> Bool
portalNaTerra (x,y) terras = (x,y) `elem` terras

{- | Verifica se um portal está na mesma posição da Base
-}
portalSobBase :: Posicao -> Posicao -> Bool
portalSobBase x y = x == y

{- | Função que verifica se existe no máximo 1 onda ativa por portal
-}
umaOndaAtiva :: [Onda] -> Bool
umaOndaAtiva [] = True
umaOndaAtiva l = length (filter (\ o -> entradaOnda o <= 0) l) <= 1 

{- | Função que verifica se há pelo menos um caminho valido do portal à base
-}
existeCaminhoValido :: Mapa -> Base -> Portal -> Bool 
existeCaminhoValido mapa base portal = pesquisaCaminhos mapa (posicaoPortal portal) (posicaoBase base) [] 

{- | Função que recebe a posição atual , a posição final/desejada, e uma lista de posições visitadas,
verificando se existe um caminho válido entre a posição atual e a final 
-}
pesquisaCaminhos :: Mapa -> Posicao -> Posicao -> [Posicao] -> Bool
pesquisaCaminhos mapa pAtual pFinal pVisitadas
      | pAtual == pFinal = True
      | pAtual `elem` pVisitadas = False 
      | pAtual `notElem` terras = False 
      | otherwise = 
        let (x,y) = pAtual
            vizinhos = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
            nPosVisitadas = (x,y):pVisitadas
        in any (\vizinho -> pesquisaCaminhos mapa vizinho pFinal nPosVisitadas) vizinhos
       where 
        terras = coordenadasTerra mapa

--2
{- | Função que recebe uma lista de inimigos e portais, um mapa e devolve um Boliano True 
caso os inimigos estejam válidos
-}
verificaI :: [Inimigo] -> Mapa -> [Portal] -> Bool
verificaI [] _ _ = True
verificaI li mapa lportal = let (lia,lipl) = iAtivosPorLancar li
                            in  iNaTerra (map posicaoInimigo lia) (coordenadasTerra mapa)
                                && (iNoPortal (map posicaoInimigo lipl) (map posicaoPortal lportal) && all null (map projeteisInimigo lipl) && all (>= 0) (map vidaInimigo lipl))
                                && all (>= 0) (map velocidadeInimigo li)
                                && all (== True) (map verificaProjetil (map projeteisInimigo lia))

-- | Função que verifica se os inimigos estão na terra
iNaTerra :: [Posicao] -> [Posicao] -> Bool
iNaTerra [] _ = True
iNaTerra ((x,y):is) terras = (fromInteger $ floor (x+0.5) ,fromInteger $ floor (y+0.5)) `elem` terras && iNaTerra is terras

-- | Função que verifica se os inimigos estão no portal
iNoPortal :: [Posicao] -> [Posicao] -> Bool
iNoPortal [] _ = True
iNoPortal (i:is) lportal = i `elem` lportal && iNoPortal is lportal

-- | Função que dada uma lista de inimigos devolve um par de inimigos ativos e inimigos a serem lancados
iAtivosPorLancar :: [Inimigo] -> ([Inimigo],[Inimigo])
iAtivosPorLancar [] = ([],[])
iAtivosPorLancar (i:is) | velocidadeInimigo i == 0 && vidaInimigo i == vidaInicialInimigo = (ia,i:ipl)
                        | otherwise = (i:ia,ipl)
                        where (ia,ipl) = iAtivosPorLancar is

-- | Vida inicial de um inimigo
vidaInicialInimigo :: Float
vidaInicialInimigo = 30

{-| A função verifica se a lista de projéteis está normalizada (de acordo com a secção 3.2)
-}
verificaProjetil :: [Projetil] -> Bool
verificaProjetil [] = True
verificaProjetil (p:lprojetil) = notElem (tipoProjetil p) (map tipoProjetil lprojetil)
                                 && verificaTipos (p:lprojetil)
                                 && verificaProjetil lprojetil

{- | Função, que dada uma lista de projéteis, verifica se os tipos de projéteis ativos
estão normalizados, de acordo com a secção 3.2, devolvendo True se a lista estiver normalizada
-}
verificaTipos :: [Projetil] -> Bool
verificaTipos lprojetil = not ( elem Fogo (map tipoProjetil lprojetil) && elem Gelo (map tipoProjetil lprojetil))
                          && not ( elem Fogo (map tipoProjetil lprojetil) && elem Resina (map tipoProjetil lprojetil))


-- 3
{- | Função que verifica a validade das torres 
-}
verificaTorres :: [Torre] -> Mapa -> Bool
verificaTorres [] _ = True
verificaTorres (t:ts) m = torreNaRelva (posicaoTorre t) relvas
                         && alcanceRajadaPos (alcanceTorre t) (rajadaTorre t)
                         && verificaCiclo (cicloTorre t)
                         && torreNSobreposta (posicaoTorre t) lTorres
                         && verificaTorres ts m
                         where
                          relvas = coordenadasRelva m
                          lTorres = map posicaoTorre ts

{- | Verifica se uma determinada torre está sobre terra, (devolve True se estiver)
-}
torreNaRelva :: Posicao -> [Posicao] -> Bool
torreNaRelva (x,y) relvas = (x,y) `elem` relvas

{- | Verifica se o alcance e a rajada de cada torre é positivo
-}
alcanceRajadaPos :: Float -> Int -> Bool
alcanceRajadaPos alc raj = alc > 0 && raj > 0

{- | Verifica se o ciclo (o tempo entre cada disparo) 
é superior a 0
-}
verificaCiclo :: Float -> Bool                 
verificaCiclo c = c > 0

{- | Verifica se uma torre não está sobreposta a outra torre. Devolve True se não estiver
-}
torreNSobreposta :: Posicao -> [Posicao] -> Bool
torreNSobreposta p l = p `notElem` l


--4
{- | A função verifica se a base não está sobreposta a portais, 
verifica se está na terra e se os créditos são positivos
-}
verificaBase :: Base -> [Portal] -> Mapa -> Bool
verificaBase base lportal mapa = not (verificaObj (posicaoBase base) (coordenadasTerra mapa))
                                 && verificaObj (posicaoBase base) (map posicaoPortal lportal)
                                 && creditosBase base >= 0

--_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
-- Auxiliares 
{- | Função que dado um mapa, converte um mapa nas suas coordenadas, sob a forma de
uma lista de posições
-}
coordenadasMapa :: Mapa -> [Posicao]
coordenadasMapa mapa =
  [ (x, y) | (y, linha) <- zip [0..] mapa, (x, terreno) <- zip [0..] linha]

-- | Função que, dado um mapa, devolve a lista das posições onde existe terra.
coordenadasTerra :: Mapa -> [Posicao]
coordenadasTerra mapa =
  [ (x, y) | (y, linha) <- zip [0..] mapa, (x, terreno) <- zip [0..] linha, terreno == Terra]

-- | Função que, dado um mapa, devolve a lista das posições onde existe relva.
coordenadasRelva :: Mapa -> [Posicao]
coordenadasRelva mapa =
  [ (x, y) | (y, linha) <- zip [0..] mapa, (x, terreno) <- zip [0..] linha, terreno == Relva]


{- | Função que verifica se um objeto não pertence a 
uma lista de objetos (devolve True se os objetos não estiverem sobrepostos) 
-}
verificaObj :: Posicao -> [Posicao] -> Bool
verificaObj _ [] = True
verificaObj (x,y) ((xs,ys):r) = (x,y) /= (xs,ys) && verificaObj (x,y) r
--_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
