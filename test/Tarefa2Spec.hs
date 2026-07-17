module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import LI12425
import Tarefa2


testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ "Inimigos No Alcance" ~: [] ~=? inimigosNoAlcance (head torresExemplo) [inimigoExemplo3,inimigoExemplo4],
        "Atinge Inimigo" ~: inimigoExemplo4 {vidaInimigo = 90, projeteisInimigo = [Projetil Fogo (Finita 3)]} ~=? atingeInimigo (head torresExemplo) inimigoExemplo4 ,
        "Ativa Inimigo" ~:((head portaisExemplo2) {ondasPortal = [ondaAtiva{inimigosOnda = []},ondaInativa]},[inimigoExemplo3,inimigoExemplo1{velocidadeInimigo = 1}]) ~=? ativaInimigo (head portaisExemplo2) [inimigoExemplo3],
        "Ganhou" ~: True ~=? ganhouJogo jogoGanho,
        "Perdeu" ~: True ~=? perdeuJogo jogoPerdido,
        "Jogo terminou (Ganhou)" ~: True ~=? terminouJogo jogoGanho,
        "Jogo terminou (Perdeu)" ~: True ~=? terminouJogo jogoPerdido
      ]

  
-- Jogo terminado (ganhou)
jogoGanho :: Jogo 
jogoGanho = Jogo { baseJogo = baseExemplo,
                    portaisJogo = portaisExemplo1,
                    torresJogo = torresExemplo,
                    mapaJogo = mapaExemplo,
                    inimigosJogo = [],
                    lojaJogo = [(20,head torresExemplo)]
                  }

jogoPerdido :: Jogo 
jogoPerdido = Jogo { baseJogo = baseExemplo {vidaBase = 0},
                    portaisJogo = portaisExemplo2,
                    torresJogo = torresExemplo,
                    mapaJogo = mapaExemplo,
                    inimigosJogo = [inimigoExemplo3,inimigoExemplo4],
                    lojaJogo = [(20,head torresExemplo)]
                  }


-- Mapa Exemplo
mapaExemplo :: Mapa
mapaExemplo = [[Terra,Terra,Terra,Relva],
               [Relva,Relva,Terra,Agua],
               [Relva,Relva,Terra,Agua],
               [Relva,Relva,Terra,Agua]
              ]

-- Base
baseExemplo :: Base
baseExemplo = Base {vidaBase = 100,
                    posicaoBase = (2,3),
                    creditosBase = 0 
                   }

-- Torres
torresExemplo :: [Torre]
torresExemplo = [ Torre {posicaoTorre = (0,3),
                         danoTorre = 10,
                         alcanceTorre = 2,
                         rajadaTorre = 3,
                         cicloTorre = 2,
                         tempoTorre = 0,
                         projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}
                        }
                ]

-- Portal
portaisExemplo1 :: [Portal]
portaisExemplo1 = [ Portal { posicaoPortal = (0,0),
                            ondasPortal = []
                          }
                 ]

portaisExemplo2 :: [Portal]
portaisExemplo2 = [ Portal { posicaoPortal = (0,0),
                            ondasPortal = [ondaAtiva,ondaInativa]
                          }
                 ]

-- Ondas
ondaAtiva :: Onda 
ondaAtiva = Onda { inimigosOnda = [inimigoExemplo1],
                   cicloOnda = 5,
                   tempoOnda = 0,
                   entradaOnda = 0
                 }

ondaInativa :: Onda 
ondaInativa = Onda { inimigosOnda = [inimigoExemplo2],
                   cicloOnda = 5,
                   tempoOnda = 5,
                   entradaOnda = 20
                   }

-- Inimigos por lançar 
inimigoExemplo1 :: Inimigo 
inimigoExemplo1 = Inimigo { posicaoInimigo = (0,0),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 0,
                            ataqueInimigo = 10,
                            butimInimigo = 10,
                            projeteisInimigo = [],
                            direcoesInimigo = [((0,0),Este),((2,0),Sul)]
                          }

inimigoExemplo2 :: Inimigo 
inimigoExemplo2 = Inimigo { posicaoInimigo = (0,0),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 0,
                            ataqueInimigo = 10,
                            butimInimigo = 10,
                            projeteisInimigo = [],
                            direcoesInimigo = [((0,0),Este),((2,0),Sul)]
                          }

-- Inimigo no mapa
inimigoExemplo3 :: Inimigo 
inimigoExemplo3 = Inimigo { posicaoInimigo = (2,2),
                            direcaoInimigo = Sul,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 10,
                            projeteisInimigo = [],
                            direcoesInimigo = []
                          }

inimigoExemplo4 :: Inimigo 
inimigoExemplo4 = Inimigo { posicaoInimigo = (1,0),
                            direcaoInimigo = Sul,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 10,
                            projeteisInimigo = [],
                            direcoesInimigo = [((2,0),Sul)]
                          }
