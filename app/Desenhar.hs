module Desenhar where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss
import ImmutableTowers
import LI12425
import Tarefa1 
import Tarefa3 


desenha :: ImmutableTowers -> Picture
 
desenha it@ImmutableTowers {estadoUI = Jogando} = 
                          let Just jogo = jogoAtual it
                          in case mapaSelecionado it of 
                    Just Mapa1 -> Pictures ((desenhaJogo1 jogo (imagens it)) ++ (desenhaTorreSelecionada it))
                    Just Mapa2 -> Pictures ((desenhaJogo2 jogo (imagens it)) ++ (desenhaTorreSelecionada it))

desenha it@ImmutableTowers{estadoUI= Menu } = desenhaMenu (imagens it)

desenha it@ImmutableTowers {estadoUI = Fim (Vitoria _)} = desenhaVitoria it (imagens it)

desenha it@ImmutableTowers {estadoUI = Escolha} = desenhaEscolhaMapa (imagens it)
           
desenha it@ImmutableTowers {estadoUI = Fim Derrota} = desenhaDerrota (imagens it)

desenha it= desenhaPausa (imagens it) -- qualquer outro caso irá desenhar o jogo Pausado

desenhaTorreSelecionada :: ImmutableTowers -> [Picture]
desenhaTorreSelecionada it = 
    case torreSelecionada it of 
     Just t -> let (x,y) = posicaoCursor it 
               in case tipoProjetil (projetilTorre t) of 
                       Fogo -> [Translate x y (imgTorreFogo (imagens it))]
                       Gelo -> [Translate x y (imgTorreGelo (imagens it))]
                       Resina -> [Translate x y (imgTorreResina (imagens it))]
     Nothing -> []

desenhaEscolhaMapa :: Imagens -> Picture 
desenhaEscolhaMapa imgs = Pictures ([imgFundo imgs] ++ [Translate (-435) 25 (imgMapa1 imgs)] ++ [Translate 435 25 (imgMapa2 imgs)])

desenhaVitoria :: ImmutableTowers -> Imagens -> Picture
desenhaVitoria it imgs
  | (mod (floor f) 2) == 1 = imgVitoria imgs
  | otherwise = imgVitoria2 imgs
  where 
      f = retiraValor it

retiraValor :: ImmutableTowers -> Float
retiraValor it@ImmutableTowers{estadoUI = Fim (Vitoria f)} = f 


desenhaFundo :: Imagens -> [Picture]
desenhaFundo imgs = [imgFundo imgs]

desenhaDerrota :: Imagens -> Picture 
desenhaDerrota img = imgDerrota img

desenhaPausa :: Imagens -> Picture 
desenhaPausa img = imgPausa img  

desenhaMenu :: Imagens -> Picture
desenhaMenu imgs = Translate 0 0 $ imgMenu imgs

desenhaLoja :: Imagens -> [Picture]
desenhaLoja imgs =  [Translate 750 0 (imgLoja imgs)]

desenhaUI :: Imagens -> [Picture]
desenhaUI imgs = [Translate (-680) 0 $ imgUI imgs]

desenhaCreditos :: Base -> Imagens ->[Picture]
desenhaCreditos base imgs = [Pictures [Translate (-735) 70 $ Scale 1.2 1.2 $   imgMoeda imgs ,
                            Translate (-720) 50 $ Scale 0.4 0.4 $ Text (show (creditosBase base)),
                            Translate (-719) 50 $ Scale 0.4 0.4 $ Text (show (creditosBase base)),
                            Translate (-721) 50 $ Scale 0.4 0.4 $ Text (show (creditosBase base))]]

desenhaVida :: Base -> [Picture]
desenhaVida base |(vidaBase base *200)/vidabasePadrao > 40 = [Pictures [ Translate (-680) (-150) $ color white $ rectangleSolid (200) (50), 
                  Translate (-(680+(vidabasePadrao - vidaBase base))) (-150) $ color green $ rectangleSolid ((vidaBase base *200)/vidabasePadrao) (50) ] ]
                 |otherwise = [Pictures [ Translate (-680) (-150) $ color white $ rectangleSolid (200) (50),
                  Translate (-(680+(vidabasePadrao - vidaBase base))) (-150) $ color red $ rectangleSolid ((vidaBase base *200)/vidabasePadrao) (50) ] ]

desenhaJogo1 :: Jogo -> Imagens -> [Picture]
desenhaJogo1 jogo imgs =  fundo ++ mapa1 ++ torres ++ base ++ inimigos ++ portais ++ loja ++ ui ++ creditos ++ vida     
                        where mapa1 = desenhaMapa1 (mapaJogo jogo) imgs
                              torres = map (\t -> desenhaTorre t imgs) (torresJogo jogo)
                              inimigos = map (\i -> desenhaInimigo i imgs) (inimigosJogo jogo) 
                              portais = map (\p -> desenhaPortal p imgs) (portaisJogo jogo) 
                              loja = desenhaLoja imgs
                              base = desenhaBase (baseJogo jogo) imgs 
                              vida = desenhaVida (baseJogo jogo)
                              fundo = desenhaFundo imgs
                              ui = desenhaUI imgs 
                              creditos = desenhaCreditos (baseJogo jogo) imgs 

desenhaJogo2 :: Jogo -> Imagens -> [Picture]
desenhaJogo2 jogo imgs =  fundo ++ mapa2 ++ torres ++ base ++ inimigos ++ portais ++ loja ++ ui ++ creditos ++ vida     
                        where mapa2 = desenhaMapa2 (mapaJogo jogo) imgs
                              torres = map (\t -> desenhaTorre t imgs) (torresJogo jogo)
                              inimigos = map (\i -> desenhaInimigo i imgs) (inimigosJogo jogo) 
                              portais = map (\p -> desenhaPortal p imgs) (portaisJogo jogo) 
                              loja = desenhaLoja imgs
                              base = desenhaBase (baseJogo jogo) imgs 
                              vida = desenhaVida (baseJogo jogo)
                              fundo = desenhaFundo imgs
                              ui = desenhaUI imgs 
                              creditos = desenhaCreditos (baseJogo jogo) imgs 

desenhaTorre :: Torre -> Imagens -> Picture
desenhaTorre torre imgs | tipoProjetil projetil == Fogo = Translate x y (imgTorreFogo imgs)
                        | tipoProjetil projetil == Gelo = Translate x y (imgTorreGelo imgs)
                        | otherwise =  Translate x y (imgTorreResina imgs)
                         where 
                           (x,y) = matrizToMapa (posicaoTorre torre) 
                           projetil = projetilTorre torre

desenhaBase :: Base -> Imagens -> [Picture]
desenhaBase base imgs = 
    let (x,y) = matrizToMapa (posicaoBase base)  
    in  [Translate x y (imgBase imgs)]                             


desenhaInimigo :: Inimigo -> Imagens -> Picture 
desenhaInimigo inimigo imgs = 
    let (x,y) = matrizToMapa (posicaoInimigo inimigo)  
    in case direcaoInimigo inimigo of
            Norte -> let imgInimigo = case projeteisInimigo inimigo of 
                                          [] -> imgInimigoN imgs
                                          ((Projetil Fogo (_)):ps) -> imgInimigoNFogo imgs
                                          ((Projetil Gelo (_)):ps) -> imgInimigoNGelo imgs
                                          ((Projetil Resina (_)):ps) -> imgInimigoNResina imgs
                     in Translate x y imgInimigo
            Sul   -> let imgInimigo = case projeteisInimigo inimigo of 
                                          [] -> imgInimigoS imgs
                                          ((Projetil Fogo (_)):ps) -> imgInimigoSFogo imgs
                                          ((Projetil Gelo (_)):ps) -> imgInimigoSGelo imgs
                                          ((Projetil Resina (_)):ps) -> imgInimigoSResina imgs
                     in Translate x y imgInimigo
            Este  -> let imgInimigo = case projeteisInimigo inimigo of 
                                          [] -> imgInimigoE imgs
                                          ((Projetil Fogo (_)):ps) -> imgInimigoEFogo imgs
                                          ((Projetil Gelo (_)):ps) -> imgInimigoEGelo imgs
                                          ((Projetil Resina (_)):ps) -> imgInimigoEResina imgs
                     in Translate x y imgInimigo
            Oeste -> let imgInimigo = case projeteisInimigo inimigo of 
                                          [] -> imgInimigoO imgs
                                          ((Projetil Fogo (_)):ps) -> imgInimigoOFogo imgs
                                          ((Projetil Gelo (_)):ps) -> imgInimigoOGelo imgs
                                          ((Projetil Resina (_)):ps) -> imgInimigoOResina imgs
                     in Translate x y imgInimigo
                                   
                                  
desenhaPortal :: Portal -> Imagens -> Picture
desenhaPortal portal imgs =
    let (x,y) = matrizToMapa (posicaoPortal portal)
    in Translate x y (imgPortal imgs)

desenhaMapa1 :: Mapa -> Imagens -> [Picture]
desenhaMapa1 mapa imgs = map (\t -> desenhaTerreno1 t imgs) mapaCoordenadas
                   where mapaCoordenadas = zip (concat mapa) (coordenadasMapa mapa) 

desenhaTerreno1 :: (Terreno, Posicao) -> Imagens -> Picture
desenhaTerreno1 (terreno, (x, y)) imgs =
  let -- Calcula as coordenadas ajustadas para o centro da tela
      (coordX,coordY) = matrizToMapa (x,y)
  in case terreno of
    Terra  -> Translate coordX coordY $ imgTerra imgs
    Relva  -> Translate coordX coordY $ imgRelva imgs
    Agua   -> Translate coordX coordY $ imgAgua imgs

desenhaMapa2 :: Mapa -> Imagens -> [Picture]
desenhaMapa2 mapa imgs = map (\t -> desenhaTerreno2 t imgs) mapaCoordenadas
                   where mapaCoordenadas = zip (concat mapa) (coordenadasMapa mapa) 

desenhaTerreno2 :: (Terreno, Posicao) -> Imagens -> Picture
desenhaTerreno2 (terreno, (x, y)) imgs =
  let -- Calcula as coordenadas ajustadas para o centro da tela
      (coordX,coordY) = matrizToMapa (x,y)
  in case terreno of
    Terra  -> Translate coordX coordY $ imgCaminho imgs
    Relva  -> Translate coordX coordY $ imgPedra imgs
    Agua   -> Translate coordX coordY $ imgLava imgs

matrizToMapa :: Posicao -> Posicao
matrizToMapa (x,y) = ((x - (fromIntegral larguraTela)/2) * gradiante,(-y + (fromIntegral alturaTela)/2)  * gradiante)


-- Tamanhos da tela em pixels
larguraTela :: Int
larguraTela = length (head mapaInicial1)  

alturaTela :: Int
alturaTela = length mapaInicial1 

gradiante :: Float
gradiante = 40

-- Custo das torres
custoTorreFogo :: Int 
custoTorreFogo = 50

custoTorreGelo :: Int 
custoTorreGelo = 50 

custoTorreResina :: Int 
custoTorreResina = 50

vidabasePadrao :: Float
vidabasePadrao = 100


--_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

-- Estado inicial 1 do jogo
estadoInicialJogo1 :: Jogo
estadoInicialJogo1 = Jogo {
    baseJogo = baseInicial1,
    portaisJogo = [portalInicial1],
    torresJogo = [],
    mapaJogo = mapaInicial1,
    inimigosJogo = [],
    lojaJogo = lojaInicial
}

-- Base inicial
baseInicial1 :: Base
baseInicial1 = Base {
    vidaBase = vidabasePadrao,
    posicaoBase = (18,0), -- Posição no mapa
    creditosBase = 100     -- Créditos iniciais
}

-- Mapa inicial 1
mapaInicial1 :: Mapa
mapaInicial1 =[
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r],
    [a, a, a, a, a, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r],
    [r, r, r, r, a, a, a, r, r, r, r, r, r, r, r, r, r, r, t, r],
    [r, r, r, r, r, r, a, r, r, r, r, r, r, r, r, r, r, r, t, r],
    [r, r, r, t, t, t, t, t, t, t, r, r, r, r, r, r, r, r, t, r],
    [r, r, r, t, r, r, a, r, r, t, r, r, r, r, r, r, r, r, t, r],
    [r, r, r, t, r, r, a, r, r, t, r, r, r, r, r, r, r, r, t, r],
    [r, r, r, t, r, r, a, r, r, t, t, t, t, r, r, r, r, r, t, r],
    [r, r, r, t, r, r, a, r, r, r, r, r, t, r, r, r, r, r, t, r],
    [r, r, r, t, r, r, a, r, r, r, r, r, t, r, r, r, r, r, t, r],
    [r, r, r, t, r, r, a, r, r, r, r, r, t, t, t, t, r, r, t, r],
    [r, t, t, t, r, r, a, r, r, r, r, r, r, r, r, t, r, r, t, r],
    [r, t, r, r, r, r, a, r, r, r, r, r, r, r, r, t, r, r, t, r],
    [r, t, r, r, r, r, a, r, r, r, r, r, r, r, r, t, r, r, t, r],
    [r, t, r, r, r, r, a, r, r, r, r, r, r, r, r, t, t, t, t, r],
    [r, t, r, r, r, r, a, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, t, r, r, r, r, a, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, t, r, r, r, r, a, a, a, a, a, a, a, a, a, a, a, a, a, a],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r] 
     ]
     where 
        t = Terra
        r = Relva
        a = Agua    
    
-- Portal inicial
portalInicial1 :: Portal
portalInicial1 = Portal {
    posicaoPortal = (0, 17), -- Posição no mapa
    ondasPortal = ondaInicial1 : (concat $ replicate 3 $ [ondaInicial1{entradaOnda = 2}])
}

-- Onda inicial de inimigos
ondaInicial1 :: Onda
ondaInicial1 = Onda {
    inimigosOnda = concat $ replicate 10 $ [inimigoInicial1] ,
    cicloOnda = 2.0,      -- Tempo entre inimigos
    tempoOnda = 2.0,      -- Tempo restante para o próximo inimigo
    entradaOnda = 0.0     -- Tempo restante para a onda começar
}

-- Inimigo inicial
inimigoInicial1 :: Inimigo
inimigoInicial1 = Inimigo {
    posicaoInimigo = (0, 17), 
    direcaoInimigo = Este,    -- Direção inicial
    vidaInimigo = 30.0,      -- Vida do inimigo
    velocidadeInimigo = 0, -- Velocidade de movimento
    ataqueInimigo = 5.0,     -- Dano à base
    butimInimigo = 10,       -- Créditos concedidos ao ser derrotado
    projeteisInimigo = [],   -- Nenhum projétil ativo
    direcoesInimigo =  [((1,17),Norte),((1,11),Este),((3,11),Norte),((3,4),Este),((9,4),Sul),((9,7),Este),((12,7),Sul),((12,10),Este),((15,10),Sul),((15,14),Este),((18,14),Norte)] 
}


-- Loja inicial
lojaInicial :: Loja
lojaInicial = [(custoTorreFogo, Torre {posicaoTorre = (0, 0), danoTorre = 5.0,alcanceTorre = 1,rajadaTorre = 1,cicloTorre = 2.0,tempoTorre = 0.0,projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 2.5 }}),
               (custoTorreGelo, Torre {posicaoTorre = (0, 0), danoTorre = 5.0,alcanceTorre = 1,rajadaTorre = 2,cicloTorre = 2.0,tempoTorre = 0.0,projetilTorre = Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }}),   
               (custoTorreResina, Torre {posicaoTorre = (0, 0), danoTorre = 5.0,alcanceTorre = 1,rajadaTorre = 3,cicloTorre = 2.0,tempoTorre = 0.0,projetilTorre = Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }})  
                  ]

--_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

-- Estado inicial 2 do jogo
estadoInicialJogo2 :: Jogo
estadoInicialJogo2 = Jogo {
    baseJogo = baseInicial2,
    portaisJogo = [portalInicial2_1,portalInicial2_2],
    torresJogo = [],
    mapaJogo = mapaInicial2,
    inimigosJogo = [],
    lojaJogo = lojaInicial
}

-- Base inicial
baseInicial2 :: Base
baseInicial2 = Base {
    vidaBase = vidabasePadrao,
    posicaoBase = (0,9), -- Posição no mapa
    creditosBase = 100     -- Créditos iniciais
}

-- Mapa inicial 2
mapaInicial2 :: Mapa
mapaInicial2 =[
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, a, r, r, r, r],
    [a, a, a, a, a, r, r, r, r, r, r, r, r, r, r, a, r, r, r, r],
    [r, r, r, r, a, a, a, r, r, r, r, r, a, a, a, a, r, r, t, t],
    [r, r, r, r, r, r, a, r, r, r, r, r, a, r, r, r, r, r, t, r],
    [r, r, r, t, t, t, t, t, t, t, r, r, a, r, r, r, r, r, t, r],
    [r, r, r, t, r, r, a, r, r, t, r, r, a, a, a, a, a, r, t, r],
    [r, r, r, t, r, r, a, r, r, t, r, r, r, r, r, r, a, r, t, r],
    [r, r, r, t, r, r, a, r, r, t, t, t, t, t, t, t, t, t, t, r],
    [r, r, r, t, r, r, a, r, r, r, r, r, t, r, r, r, a, r, r, r],
    [t, t, t, t, r, r, a, r, r, r, r, r, t, r, r, r, a, r, r, r],
    [r, r, r, r, r, r, a, r, r, r, r, r, t, t, t, r, a, a, a, a],
    [r, r, r, r, r, r, a, r, r, r, r, r, r, r, t, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, t, r, r, r, r, r],
    [r, r, r, a, r, r, a, r, r, r, r, r, r, r, t, r, r, r, r, r],
    [r, r, r, a, r, r, a, r, r, r, r, r, r, r, t, r, r, r, r, r],
    [r, a, a, a, r, r, a, r, r, r, r, r, r, r, t, r, r, r, r, r],
    [r, a, r, r, r, r, a, r, r, r, r, r, r, r, t, r, r, r, r, r],
    [r, a, r, r, r, r, a, a, a, a, a, a, a, a, t, a, a, a, a, a],
    [r, a, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r],
    [r, a, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r]
  ]
    where
        t=Terra
        r=Relva
        a=Agua
    
-- Portal inicial
portalInicial2_1 :: Portal
portalInicial2_1 = Portal {
    posicaoPortal = (19, 2), -- Posição no mapa
    ondasPortal = ondaInicial2_1 :(concat $ replicate 4 $ [ondaInicial2_1{entradaOnda = 2}])
}

portalInicial2_2 :: Portal
portalInicial2_2 = Portal {
    posicaoPortal = (14, 19), -- Posição no mapa
    ondasPortal = ondaInicial2_2 : (concat $ replicate 4 $ [ondaInicial2_2{entradaOnda = 2}])
}

-- Onda inicial de inimigos
ondaInicial2_2 :: Onda
ondaInicial2_2 = Onda {
    inimigosOnda = concat $ replicate 15 $ [inimigoInicial2_2],
    cicloOnda = 2.0,      -- Tempo entre inimigos
    tempoOnda = 2.4,      -- Tempo restante para o próximo inimigo
    entradaOnda = 0.0     -- Tempo restante para a onda começar
}

ondaInicial2_1 :: Onda
ondaInicial2_1 = Onda {
    inimigosOnda = concat $ replicate 15 $ [inimigoInicial2_1],
    cicloOnda = 2.0,      -- Tempo entre inimigos
    tempoOnda = 1.7,      -- Tempo restante para o próximo inimigo
    entradaOnda = 0.0     -- Tempo restante para a onda começar
}

-- Inimigo inicial

inimigoInicial2_1 :: Inimigo
inimigoInicial2_1 = Inimigo {
    posicaoInimigo = (19,2), 
    direcaoInimigo = Oeste,    -- Direção inicial
    vidaInimigo = 30.0,      -- Vida do inimigo
    velocidadeInimigo = 0, -- Velocidade de movimento
    ataqueInimigo = 5.0,     -- Dano à base
    butimInimigo = 10,       -- Créditos concedidos ao ser derrotado
    projeteisInimigo = [],   -- Nenhum projétil ativo
    direcoesInimigo =  [((18,2),Sul),((18,7),Oeste),((9,7),Norte),((9,4),Oeste),((3,4),Sul),((3,9),Oeste)] 
}
inimigoInicial2_2 :: Inimigo
inimigoInicial2_2 = Inimigo {
    posicaoInimigo = (14,19), 
    direcaoInimigo = Norte,    -- Direção inicial
    vidaInimigo = 30.0,      -- Vida do inimigo
    velocidadeInimigo = 0, -- Velocidade de movimento
    ataqueInimigo = 5.0,     -- Dano à base
    butimInimigo = 10,       -- Créditos concedidos ao ser derrotado
    projeteisInimigo = [],   -- Nenhum projétil ativo
    direcoesInimigo =  [((14,10),Oeste),((12,10),Norte),((12,7),Oeste),((9,7),Norte),((9,4),Oeste),((3,4),Sul),((3,9),Oeste)] 
}

--_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-