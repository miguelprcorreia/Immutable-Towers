{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : José Pedro Fernandes da Costa <a109807@alunos.uminho.pt>
              Miguel Pedro Ribeiro Correia <a111773@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa2
import Tarefa1
import Data.List (sortOn)

-- | A função @atualizaJogo@, dado um tempo, vai atualizando todas as estruturas e objetos conforme o tempo dado
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t j = j {baseJogo = bAtualizada, torresJogo = tAtualizadas, inimigosJogo = iFinais, portaisJogo = pAtualizados}
                   where base = baseJogo j
                         lPortais = portaisJogo j
                         lTorres = torresJogo j
                         mapa = mapaJogo j
                         lInimigos = inimigosJogo j
                         (bAtualizada,tAtualizadas,pAtualizados,iFinais) = atualizaInimigos t mapa base lTorres lPortais lInimigos

-- Torres 
{- | Dado um tempo, uma torre e uma lista de inimigos, a função devolve uma tupla com a torre atualizada e uma lista com os inimigos atingidos.
Caso a lista de inimigos no alcance da torre seja vazia, irá retornar a torre atualizada e a lista de inimigos, inalterados-}
atualizaTorre :: Tempo -> Torre -> [Inimigo] -> (Torre, [Inimigo])
atualizaTorre t torre inimigos
  -- Atualiza a torre sem disparar, apenas decrementa o tempo restante
  | tempoTorre torre > 0 = (torre {tempoTorre = max 0 (tempoTorre torre - t){-Assim o tempo não é negativo-}}
                          , inimigos)
  | otherwise =
      let
          inimigosAlvo = inimigosNoAlcance torre inimigos
          iForaAlcance = filter (\ i -> not (alcanceTotalTorre torre i)) inimigos
          rajada = rajadaTorre torre
          (alvos, restantes) = splitAt rajada (sortOn (\i -> distancia (posicaoTorre torre) (posicaoInimigo i)) inimigosAlvo) -- vai dividir a lista em 2 com n elementos de acordo com a rajada
          inimigosAtualizados = map (atingeInimigo torre) alvos --todos os inimigos atingidos pela torre, de acordo com a rajada
          novaTorre = torre {tempoTorre = cicloTorre torre} -- reset no coldown
      in
          (novaTorre, inimigosAtualizados ++ restantes ++ iForaAlcance) 

-- | A função atualiza Torres retorna uma tupla com a lista das torres atualizadas e a lista dos inimigos atingidos, utilizando a função @atualizaTorre@ como função auxiliar.
atualizaTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizaTorres _ [] inimigos = ([], inimigos) 
atualizaTorres t (t1:r) inimigos =
    let (t1Atualizada, iAtualizados) = atualizaTorre t t1 inimigos
        (torresAtualizadas, inimigosRestantes) = atualizaTorres t r iAtualizados
    in (t1Atualizada : torresAtualizadas, inimigosRestantes)


-- | Função que atualiza os Inimigos 
atualizaInimigos :: Tempo -> Mapa -> Base -> [Torre] -> [Portal] -> [Inimigo] -> (Base,[Torre],[Portal],[Inimigo])
atualizaInimigos t m b tr lp li = (bAtualizada,tAtualizadas,pAtualizados,iFinais)
                          where
                            inimigosMovidos = movInimigos t m li
                            (tAtualizadas,inimigosAtingidos) = atualizaTorres t tr inimigosMovidos
                            inimigosAfetados = efeitoProjetil t inimigosAtingidos
                            (baseMaisCerditos,iVivos) = rmInimigosMortos b inimigosAfetados []
                            (pAtualizados,novosInimigos) = atualizaPortais t lp iVivos
                            (bAtualizada,iFinais) = inimigosNaBase baseMaisCerditos novosInimigos []

-- | A função movInimigos, dado um tempo, um Mapa, uma Base e uma lista de inimigos, irá mover os inimigos em direção à base
movInimigos :: Tempo -> Mapa -> [Inimigo] -> [Inimigo]
movInimigos _ _ [] = []
movInimigos tempo mapa (inimigo:r)
  | null (direcoesInimigo inimigo) = (movDirecao tempo inimigo (direcaoInimigo inimigo)) : movInimigos tempo mapa r
  | direcaoInimigo inimigo == Norte && (arredonda (x+0.5,y+1)) == (fst $ head (direcoesInimigo inimigo)) =
     ((movDirecao tempo inimigo (snd $ head $ direcoesInimigo inimigo)) {direcoesInimigo = tail $ direcoesInimigo inimigo}):movInimigos tempo mapa r
  |  direcaoInimigo inimigo == Este  && (arredonda (x,y+0.5)) == (fst $ head (direcoesInimigo inimigo)) =
     ((movDirecao tempo inimigo (snd $ head $ direcoesInimigo inimigo)) {direcoesInimigo = tail $ direcoesInimigo inimigo}):movInimigos tempo mapa r
  |   direcaoInimigo inimigo == Oeste && (arredonda (x+1,y+0.5)) == (fst $ head (direcoesInimigo inimigo)) =
     ((movDirecao tempo inimigo (snd $ head $ direcoesInimigo inimigo)) {direcoesInimigo = tail $ direcoesInimigo inimigo}):movInimigos tempo mapa r
  |  direcaoInimigo inimigo == Sul && (arredonda (x+0.5,y)) == (fst $ head (direcoesInimigo inimigo)) =
     ((movDirecao tempo inimigo (snd $ head $ direcoesInimigo inimigo)) {direcoesInimigo = tail $ direcoesInimigo inimigo}):movInimigos tempo mapa r
  | otherwise = (movDirecao tempo inimigo (direcaoInimigo inimigo)) : movInimigos tempo mapa r
  where
    (x,y) = posicaoInimigo inimigo

-- | Dada uma posição, a função vai arredondá-la sempre por defeito
arredonda :: Posicao -> Posicao
arredonda (x,y) = ((fromInteger $ floor x) ::Float ,(fromInteger $ floor y) ::Float )

-- | Dado um tempo, um inimigo e uma direção, a função altera a posição do inimigo, consoante a direção
movDirecao :: Tempo -> Inimigo -> Direcao -> Inimigo
movDirecao t inimigo Este = inimigo {posicaoInimigo = (x+(velocidadeInimigo inimigo)*t,y),direcaoInimigo = Este}
                        where x = fst $ posicaoInimigo inimigo
                              y = snd $ posicaoInimigo inimigo
movDirecao t inimigo Oeste = inimigo {posicaoInimigo = (x-(velocidadeInimigo inimigo)*t,y),direcaoInimigo = Oeste}
                         where x = fst $ posicaoInimigo inimigo
                               y = snd $ posicaoInimigo inimigo
movDirecao t inimigo Norte = inimigo {posicaoInimigo = (x,y-(velocidadeInimigo inimigo)*t),direcaoInimigo = Norte}
                         where x = fst $ posicaoInimigo inimigo
                               y = snd $ posicaoInimigo inimigo
movDirecao t inimigo Sul = inimigo {posicaoInimigo = (x,y+(velocidadeInimigo inimigo)*t),direcaoInimigo = Sul}
                          where x = fst $ posicaoInimigo inimigo
                                y = snd $ posicaoInimigo inimigo

-- | A função @removeProjeteis@, dado uma lista de inimigos, remove os projetéis cuja a sua duração já acabou.
removeProjeteis :: [Inimigo] -> [Inimigo]
removeProjeteis [] = []
removeProjeteis (i:r) | null (projeteisInimigo i) = i: removeProjeteis r
                      | otherwise = i{projeteisInimigo = filter duracaoPos (projeteisInimigo i)}:removeProjeteis r

-- | Dado um projétil, a função verifica se a sua duração já expirou 
duracaoPos :: Projetil -> Bool
duracaoPos (Projetil _ (Finita d)) = d>0
duracaoPos (Projetil _ Infinita) = True

-- | Dado um tipo de projétil, a função @efeitoProjetil@ irá aplicar os efeitos do projétil, de acordo com a tarefa 3.3.2
efeitoProjetil :: Tempo ->  [Inimigo] -> [Inimigo]
efeitoProjetil _ [] = []
efeitoProjetil t li =
  let (i1:r) = removeProjeteis li
      proj = projeteisInimigo i1
  in  case proj of
      [] -> i1{velocidadeInimigo = velocidadePadrao} : efeitoProjetil t r
      [Projetil Fogo (Finita d)] -> i1{vidaInimigo = vidaInimigo i1 - danoFogo*t, projeteisInimigo = [Projetil (Fogo) (Finita (d-t))],velocidadeInimigo = velocidadePadrao}
                                    : efeitoProjetil t r
      [Projetil Gelo (Finita d)] -> i1{velocidadeInimigo = 0, projeteisInimigo = [Projetil (Gelo) (Finita (d-t))]}
                                    : efeitoProjetil t r
      [Projetil Resina Infinita] -> i1{velocidadeInimigo = velocidadeResina, projeteisInimigo = [Projetil (Resina) Infinita]}
                                    : efeitoProjetil t r
      [Projetil Gelo (Finita d), Projetil Resina Infinita] ->
                                    i1{velocidadeInimigo = 0,projeteisInimigo = [Projetil (Gelo) (Finita (d-t)), Projetil (Resina) Infinita] }
                                    : efeitoProjetil t r
      [Projetil Resina Infinita, Projetil Gelo (Finita d)] ->
                                    i1{velocidadeInimigo = 0,projeteisInimigo = [Projetil (Resina) Infinita, Projetil (Gelo) (Finita (d-t))] }
                                   : efeitoProjetil t r

-- | O dano do fogo está definido como 1 por padrão                                            
danoFogo  :: Float
danoFogo = 1.0

-- Remoção de Inimigos Mortos
{- | A função @rmInimigosMortos@ remove os inimigos mortos de uma lista de inimigos, e adiciona o seu butim aos créditos da base.
Retornando uma tupla com a base com os créditos adicionados e a lista dos inimigos vivos. -}
rmInimigosMortos :: Base -> [Inimigo] -> [Inimigo]-> (Base,[Inimigo])
rmInimigosMortos base [] li = (base,li)
rmInimigosMortos base (i:r) li | vidaInimigo i <= 0 =
                                 rmInimigosMortos (base {creditosBase = (creditosBase base) + butimInimigo i}) r li
                               | otherwise = rmInimigosMortos base r (li++[i])

-- | A função retorna uma tupla com a lista dos inimigos por atingir a base, e a base com a vida atualizada após ser atingida pelos inimigos 
inimigosNaBase :: Base -> [Inimigo] -> [Inimigo] -> (Base,[Inimigo])
inimigosNaBase base [] li = (base,li)
inimigosNaBase base (i:r) li | posicaoBase base == arredonda (x+0.5,y+0.5) = inimigosNaBase (impactoBase base i) r li
                             | otherwise = inimigosNaBase base r (li++[i])
                             where
                              (x,y) = posicaoInimigo i

-- | Dado um inimigo e uma base, supondo que o mesmo a atingiu, atualiza a vida da base de acordo com o ataque do inimigo. 
impactoBase :: Base -> Inimigo -> Base
impactoBase base i = base {vidaBase = vidaBase base - ataqueInimigo i}


-- | Dado um tempo a função @atualizaPortais@ irá atualizar todos os portais, lançando os inimigos de acordo com o ciclo e tempo da onda.
atualizaPortais :: Tempo -> [Portal] -> [Inimigo] -> ([Portal],[Inimigo])
atualizaPortais t lp li = (lPortais,lInimigos)
                         where lAtualizada = aplicaAtualizaOndas t lp li
                               lPortais = map fst lAtualizada
                               lInimigos = intersectAdd li lAtualizada

-- | A função combina uma lista de inimigos com a lista da lista de inimigos associados a um portal, removendo duplicados.
intersectAdd :: [Inimigo] -> [(Portal,[Inimigo])] -> [Inimigo]
intersectAdd li1 l2 = li1 ++ concatMap (drop (length li1)) (map snd l2)

-- | A função atualiza a primeira onda de todos os portais, de uma lista de portais
aplicaAtualizaOndas :: Tempo -> [Portal] -> [Inimigo] -> [(Portal,[Inimigo])]
aplicaAtualizaOndas t lp li = map (\ p1 -> atualizaOnda1 t p1 li) lp


-- | A função atualiza a primeira onda de um portal de acordo com um tempo dado
atualizaOnda1 :: Tempo -> Portal -> [Inimigo] -> (Portal, [Inimigo])
atualizaOnda1 t p li =
  case ondasPortal p of
       [] -> (p,li)
       [onda] -> if null (inimigosOnda onda)
                 then (p {ondasPortal = []},li)
                 else if entradaOnda onda <= 0
                      then if tempoOnda onda <= 0
                           then ativaInimigo (p{ondasPortal = [onda{tempoOnda = cicloOnda onda}]}) li
                           else (p{ondasPortal = [onda{tempoOnda = tempoOnda onda - t}]},li)
                      else (p{ondasPortal = [onda{entradaOnda = (entradaOnda onda) -t }]},li)
       (onda1:onda2:os) -> if null (inimigosOnda onda1)
                           then atualizaOnda1 t p{ondasPortal = (onda2:os)} li
                           else if entradaOnda onda1 <= 0
                                then if tempoOnda onda1 <= 0
                                     then ativaInimigo (p{ondasPortal = onda1{tempoOnda = cicloOnda onda1}:onda2:os}) li
                                     else (p {ondasPortal = (onda1{tempoOnda = tempoOnda onda1 - t}):onda2:os},li)
                                else (p{ondasPortal = (onda1{entradaOnda = (entradaOnda onda1) -t }):onda2:os},li)

