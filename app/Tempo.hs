module Tempo where

import ImmutableTowers
import LI12425
import Tarefa1
import Tarefa2
import Tarefa3


reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo _ it@ImmutableTowers {estadoUI = Menu} = it
reageTempo _ it@ImmutableTowers {estadoUI = Pausado} = it
reageTempo _ it@ImmutableTowers {estadoUI = Escolha} = it
reageTempo t it@ImmutableTowers {estadoUI = Fim (Vitoria tick)} = it {estadoUI = Fim (Vitoria (tick + t))}
reageTempo t it | perdeuJogo jogo = it {estadoUI = Fim Derrota}
                | ganhouJogo jogo = it {estadoUI = Fim (Vitoria 0)}
                | validaJogo jogo = it {jogoAtual = Just (atualizaJogo t jogo)}
                | otherwise = error "Jogo Inválido"
                where 
                    Just jogo = jogoAtual it
