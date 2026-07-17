module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1
import Tarefa2
import Tarefa3
import Desenhar
import Debug.Trace

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers

-- Alternar entre os estados "Jogando" e "Pausado"
reageEventos (EventKey (Char 'p') Down _ _ ) it@ImmutableTowers {estadoUI = Jogando} =
    it {estadoUI = Pausado}
reageEventos (EventKey (Char 'p') Down _ _ ) it@ImmutableTowers {estadoUI = Pausado} =
    it {estadoUI = Jogando}

-- Iniciar o jogo ao pressionar Enter no Menu
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _ ) it@ImmutableTowers {estadoUI = Menu} =
    it {estadoUI = Escolha}

-- Escolher o mapa a jogar
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it@ImmutableTowers {estadoUI = Escolha} 
  |  x >= (-735) && x <= (-135) && y >= (-275) && y <= 325  =  it {jogoAtual = Just (estadoInicialJogo1), estadoUI = Jogando, mapaSelecionado = Just Mapa1}
  |  x >= 135 && x <= 735 && y >= (-275) && y <= 325 =  it {jogoAtual = Just (estadoInicialJogo2), estadoUI = Jogando, mapaSelecionado = Just Mapa2}
  | otherwise = it

-- Atualiza a posição do cursor
reageEventos (EventMotion (x, y)) it = it { posicaoCursor = (x, y) }

-- Selecionar uma torre ao clicar no botão correspondente na loja
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it@ImmutableTowers {jogoAtual = Just jogo, estadoUI = Jogando, torreSelecionada = Nothing}
  | x >= 653 && x <= 843 && y >= 280 && y <= 460 && creditosBase (baseJogo jogo) >= custoTorreFogo = it {torreSelecionada = Just torreFogo} -- Torre de Fogo
  | x >= 653 && x <= 843 && y >= -70 && y <= 110 && creditosBase (baseJogo jogo) >= custoTorreGelo = it {torreSelecionada = Just torreGelo}   -- Torre de Gelo
  | x >= 660 && x <= 850 && y >= -437 && y <= -257 && creditosBase (baseJogo jogo) >= custoTorreResina = it {torreSelecionada = Just torreResina} -- Torre de Resina
  | otherwise = it
  where
    torreFogo = Torre { posicaoTorre = (0, 0), danoTorre = 10.0, alcanceTorre = 3.0, rajadaTorre = 1, cicloTorre = 2.0, tempoTorre = 0.0, projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 } }
    torreGelo = Torre { posicaoTorre = (0, 0), danoTorre = 10.0, alcanceTorre = 3.0, rajadaTorre = 1, cicloTorre = 2.0, tempoTorre = 0.0, projetilTorre = Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 } }
    torreResina = Torre { posicaoTorre = (0, 0), danoTorre = 10.0, alcanceTorre = 3.0, rajadaTorre = 1, cicloTorre = 2.0, tempoTorre = 0.0, projetilTorre = Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita } }

-- Colocar a torre no mapa ao clicar em uma célula de relva válida
reageEventos (EventKey (MouseButton LeftButton) Up _ (x, y)) it@ImmutableTowers {jogoAtual = Just jogo, estadoUI = Jogando, torreSelecionada = Just torre, imagens = imgs}
  | posicaoValida = 
    case tipoProjetil (projetilTorre torre) of 
     Fogo -> it {jogoAtual = Just jogo { torresJogo = torre{posicaoTorre = posicaoCalculada } : torresJogo jogo, baseJogo = base { creditosBase = (creditosBase base) - custoTorreFogo} },torreSelecionada = Nothing , imagens = imgs}
     Gelo -> it {jogoAtual = Just jogo { torresJogo = torre{posicaoTorre = posicaoCalculada } : torresJogo jogo, baseJogo = base { creditosBase = (creditosBase base) - custoTorreGelo} },torreSelecionada = Nothing , imagens = imgs}
     Resina -> it {jogoAtual = Just jogo { torresJogo = torre{posicaoTorre = posicaoCalculada } : torresJogo jogo, baseJogo = base { creditosBase = (creditosBase base) - custoTorreResina} },torreSelecionada = Nothing , imagens = imgs}
  | otherwise = it {torreSelecionada = Nothing}
  where
    base = baseJogo jogo
    posicaoCalculada = (fromInteger $ floor (((x+20) + (fromIntegral larguraTela / 2) * gradiante) / gradiante) ,fromInteger $ floor (((-y+20) + (fromIntegral alturaTela / 2) * gradiante) / gradiante))
    posicaoValida = posicaoCalculada `elem` coordenadasRelva (mapaJogo jogo) && posicaoCalculada `notElem` (map posicaoTorre (torresJogo jogo))

-- Botão de restart no fim Vitória (vai para menu de escolha). 
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it@ImmutableTowers {estadoUI = Fim (Vitoria _)} | x < 200 && x > -200 &&
                                                                                                        y < -200 && y > -300 =
      it {jogoAtual = Nothing, estadoUI = Escolha,mapaSelecionado = Nothing}      

-- Botão de restart na pausa (vai para jogo atual)
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it@ImmutableTowers {estadoUI = Pausado} |x < 150 && x > -150 &&
                                                                                                        y < -200 && y > -300 = 
    case mapaSelecionado it of 
      Just Mapa1 -> it {jogoAtual = Just estadoInicialJogo1, estadoUI = Jogando}
      Just Mapa2 -> it {jogoAtual = Just estadoInicialJogo2, estadoUI = Jogando}

-- Botão de restart na derrota (vai para o jogo atual)
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it@ImmutableTowers {estadoUI = Fim Derrota} |x < 130 && x > -130 &&
                                                                                                        y < -155 && y > -225 =
    case mapaSelecionado it of 
      Just Mapa1 -> it {jogoAtual = Just estadoInicialJogo1, estadoUI = Jogando}
      Just Mapa2 -> it {jogoAtual = Just estadoInicialJogo2, estadoUI = Jogando}


-- Outros eventos permanecem inalterados
reageEventos _ it = it