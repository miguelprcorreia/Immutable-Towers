module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3
import LI12425

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ "Remove InimigosMortos" ~: (baseExemplo {creditosBase = 10}, [inimigoExemplo1, inimigoExemplo2, inimigoExemplo3]) ~=? rmInimigosMortos baseExemplo [inimigoExemplo1, inimigoExemplo2, inimigoExemplo3, inimigoExemplo4Morto] [],
        "Duração positiva" ~: True ~=? duracaoPos  projetilEx,
        "Atualiza jogo" ~: jogoExemplo {baseJogo = baseExemplo {vidaBase = 90}, portaisJogo = [Portal { posicaoPortal = (0,0),ondasPortal = [ondaAtiva{inimigosOnda = [], tempoOnda = 5},ondaInativa]}], 
                                        torresJogo = [Torre {posicaoTorre = (0,3),danoTorre = 10,alcanceTorre = 2,rajadaTorre = 3,cicloTorre = 2,tempoTorre = 2,projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}}],
                                        inimigosJogo = [inimigoExemplo1{velocidadeInimigo = 1}]} ~=? atualizaJogo 1  jogoExemplo,
        "Atualiza Portais" ~: ([Portal {posicaoPortal = (0,0), ondasPortal = [ondaAtiva{inimigosOnda = [],tempoOnda = 5}, ondaInativa]}], [inimigoExemplo1{velocidadeInimigo = 1}]) ~=? atualizaPortais 1 portaisExemplo1 []
      ]

-- Jogo Exemplo
jogoExemplo :: Jogo 
jogoExemplo = Jogo { baseJogo = baseExemplo,
                    portaisJogo = portaisExemplo1,
                    torresJogo = torresExemplo,
                    mapaJogo = mapaExemplo,
                    inimigosJogo = [inimigoExemplo3],
                    lojaJogo = []
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

-- Portais
portaisExemplo1 :: [Portal]
portaisExemplo1 = [ Portal { posicaoPortal = (0,0),
                            ondasPortal = [ondaAtiva,ondaInativa]
                          }
                 ]
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
--Projétil
projetilEx :: Projetil
projetilEx = Projetil {tipoProjetil = Fogo ,duracaoProjetil = Finita 2}

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

inimigoExemplo4Morto :: Inimigo 
inimigoExemplo4Morto = Inimigo { posicaoInimigo = (1,0),
                            direcaoInimigo = Sul,
                            vidaInimigo = 0,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 10,
                            projeteisInimigo = [],
                            direcoesInimigo = [((2,0),Sul)]
                          }
