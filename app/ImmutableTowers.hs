module ImmutableTowers where

import Graphics.Gloss
import LI12425


data ImmutableTowers = ImmutableTowers {
    jogoAtual :: Maybe Jogo,
    estadoUI :: EstadoUI,
    mapaSelecionado :: Maybe MapaSelecionado,
    torreSelecionada :: Maybe Torre,
    posicaoCursor :: Posicao,
    imagens :: Imagens
                            }

data EstadoUI = Menu | Escolha | Jogando | Pausado | Fim FimJogo 
                deriving (Show,Eq)

data FimJogo = Vitoria Float | Derrota deriving (Eq, Show)

data MapaSelecionado = Mapa1 | Mapa2

data Imagens = Imagens {
    imgTorreGelo :: Picture,
    imgTorreFogo :: Picture,
    imgTorreResina :: Picture,
    imgLoja :: Picture,
    imgBase :: Picture,
    imgPortal :: Picture,
    imgInimigoN :: Picture,
    imgInimigoNFogo :: Picture,
    imgInimigoNGelo :: Picture,
    imgInimigoNResina :: Picture,
    imgInimigoS :: Picture,
    imgInimigoSFogo :: Picture,
    imgInimigoSGelo :: Picture,
    imgInimigoSResina :: Picture,
    imgInimigoE :: Picture,
    imgInimigoEFogo :: Picture,
    imgInimigoEGelo :: Picture,
    imgInimigoEResina :: Picture,
    imgInimigoO :: Picture,
    imgInimigoOFogo :: Picture,
    imgInimigoOGelo :: Picture,
    imgInimigoOResina :: Picture,
    imgTerra :: Picture,
    imgRelva :: Picture,
    imgAgua :: Picture,
    imgMoeda :: Picture,
    imgMenu :: Picture,
    imgFundo :: Picture,
    imgVitoria :: Picture,
    imgVitoria2 :: Picture,
    imgPausa :: Picture,
    imgDerrota :: Picture,
    imgUI :: Picture,
    imgLava :: Picture,
    imgPedra :: Picture,
    imgCaminho :: Picture,
    imgMapa1 :: Picture,
    imgMapa2 :: Picture
}

