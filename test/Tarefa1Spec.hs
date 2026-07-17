
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import LI12425
import Tarefa1


testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ "Valida Base" ~: True ~=? verificaBase baseExemplo portaisExemplo mapaExemplo,
        "Valida Portais" ~: True ~=? verificaPortais portaisExemplo baseExemplo mapaExemplo,
        "Verifica Inimigos" ~: True ~=? verificaI [inimigoExemplo1,inimigoExemplo2,inimigoExemplo3] mapaExemplo portaisExemplo,
        "Verifica Torres" ~: True ~=? verificaTorres torresExemplo mapaExemplo,
        "Verifica Base" ~: True ~=? verificaBase baseExemplo portaisExemplo mapaExemplo,
        "Jogo Valido" ~: True ~=? validaJogo jogoValido,
        "Jogo Invalido (sem portais)" ~: False ~=? validaJogo jogoInvalido1,
        "Jogo Invalido (base na agua)" ~: False ~=? validaJogo jogoInvalido2
      ]

-- Jogo válido
jogoValido :: Jogo 
jogoValido = Jogo { baseJogo = baseExemplo,
                    portaisJogo = portaisExemplo,
                    torresJogo = torresExemplo,
                    mapaJogo = mapaExemplo,
                    inimigosJogo = [inimigoExemplo3],
                    lojaJogo = [(20,head torresExemplo)]
                  }

-- Jogos inválidos
jogoInvalido1 :: Jogo 
jogoInvalido1 = jogoValido {portaisJogo = []}

jogoInvalido2 :: Jogo 
jogoInvalido2 = jogoValido {baseJogo = Base {vidaBase = 100,
                                             posicaoBase = (3,3),
                                             creditosBase = 0 
                                            }
                          }

-- Mapa
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
portaisExemplo :: [Portal]
portaisExemplo = [ Portal { posicaoPortal = (0,0),
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

