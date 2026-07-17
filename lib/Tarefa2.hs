{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : José Pedro Fernandes da Costa <a109807@alunos.uminho.pt>
              Miguel Pedro Ribeiro Correia <a111773@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where
import LI12425


--1)
-- | Função que dá a lista de inimigos dentro do alcançe de uma determinada torre
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre li = filter (\ i -> alcanceTotalTorre torre i) li

-- | Função auxiliar que diz se um determinado inimigo está dentro do alcance de uma determinada torre
alcanceTotalTorre :: Torre -> Inimigo -> Bool  
alcanceTotalTorre torre inimigo = distancia (xi+0.5,yi+0.5) (xt+0.5,yt+0.5) <= (alcanceTorre torre)
                               where (xt,yt) = posicaoTorre torre
                                     (xi,yi) = posicaoInimigo inimigo   

-- | Função que calcula a distância entre duas posições
distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

--2)
-- | Função autualiza um determinado inimigo após ser atingido por um projétil de uma determinada torre
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo = inimigo { vidaInimigo = vidaInimigo inimigo - danoTorre torre, projeteisInimigo = normalizap (projetilTorre torre) (projeteisInimigo inimigo)}
                                     
-- | Função que resolve as combinações possíveis de sinergias 
normalizap :: Projetil -> [Projetil] -> [Projetil]
normalizap p [] = [p]
normalizap p pa | tipoProjetil p == Fogo && elem Gelo ltipos && elem Resina ltipos = let Finita t = duracaoProjetil p
                                                                                     in  p{duracaoProjetil =Finita (2*t)}:retiraResina(retiraGelo pa)                 
                | tipoProjetil p == Fogo && elem Gelo ltipos = retiraGelo pa
                | tipoProjetil p == Fogo && elem Resina ltipos = let Finita t = duracaoProjetil p
                                                                 in  p{duracaoProjetil =Finita (2*t)}:retiraResina pa
                | tipoProjetil p == Fogo && elem Fogo ltipos = somaduracoes p pa 
                | tipoProjetil p == Gelo && elem Fogo ltipos = retiraFogo pa
                | tipoProjetil p == Gelo && elem Gelo ltipos = somaduracoes p pa 
                | tipoProjetil p == Resina && elem Resina ltipos = pa
                | tipoProjetil p == Resina && elem Fogo ltipos =let Finita t = duracaoProjetil (head pa) 
                                                                in [(head pa) {duracaoProjetil = Finita (2*t)}]
                | otherwise =  p:pa  
                 where 
                    ltipos = map tipoProjetil pa  

-- | Função auxilar que soma a duração de um projétil à duração do único projétil ativo no inimigo  
somaduracoes :: Projetil -> [Projetil] -> [Projetil]
somaduracoes p [] = [p]
somaduracoes p (p1:r) | tipoProjetil p == tipoProjetil p1 = p{duracaoProjetil =Finita (t1+t2)}:r 
                      | otherwise = p1:(somaduracoes p r)
                      where
                        Finita t1 = duracaoProjetil p 
                        Finita t2 = duracaoProjetil p1
    
-- | Função auxiliar que remove o projétil de Resina de uma lista de projéteis
retiraResina :: [Projetil] -> [Projetil]
retiraResina [] = []
retiraResina (p:r) | tipoProjetil p == Resina = r
                   | otherwise = p : retiraResina r 

-- | Função auxiliar que remove o projétil de Fogo de uma lista de projéteis
retiraFogo :: [Projetil] -> [Projetil]
retiraFogo [] = []
retiraFogo (p:r) | tipoProjetil p == Fogo = r
                 | otherwise = p : retiraFogo r 

-- | Função auxiliar que remove o projétil de Gelo de uma lista de projéteis
retiraGelo :: [Projetil] -> [Projetil]
retiraGelo [] = []
retiraGelo (p:r) | tipoProjetil p == Gelo = r
                 | otherwise = p : retiraGelo r 

-- 3
-- | Função que ativa um inimigo de uma onda ativa
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigosAtivos
    | null (ondasPortal portal) = (portal, inimigosAtivos) -- Sem ondas no portal
    | null (inimigosOnda onda)  = (portal, inimigosAtivos) -- Sem inimigos na onda
    | otherwise =
        let novoPortal = portal { ondasPortal = onda { inimigosOnda = inimigosRestantes } : resto }
            novosInimigos =  inimigosAtivos ++ [inimigo {velocidadeInimigo = velocidadePadrao,posicaoInimigo = posicaoPortal portal}]
        in (novoPortal, novosInimigos)
    where
        (onda:resto) = ondasPortal portal
        (inimigo:inimigosRestantes) = inimigosOnda onda

-- | A velocidade dos inimigos, por padrão é 1
velocidadePadrao :: Float 
velocidadePadrao = 1.0 

-- | Quando aplicada resina, por padrão a velocidade é 0.5
velocidadeResina :: Float 
velocidadeResina = 0.5

-- 4
-- | Função que verifica se o jogo terminou
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

-- | Função que verifica se o user ganhou o jogo
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) && all (==True) (map null (map ondasPortal (portaisJogo jogo)))

-- | Função que verifica se o user perdeu o jogo
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0
