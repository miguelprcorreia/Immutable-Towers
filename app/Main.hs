module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import Graphics.Gloss 

janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

fundo :: Color
fundo = greyN 0.7 

fr :: Int
fr = 60

main :: IO ()
main = do
  imgs <- carregarImagens
  let imagensCarregadas = imgs
  putStrLn "Hello from Immutable Towers!"
  play janela fundo fr ImmutableTowers {jogoAtual = Nothing, estadoUI = Menu, mapaSelecionado = Nothing, torreSelecionada = Nothing, posicaoCursor = (0,0), imagens = imagensCarregadas} desenha reageEventos reageTempo


carregarImagens :: IO Imagens
carregarImagens = do
    imgTorreGelo <- loadBMP "assets/torreGelo.bmp"
    imgTorreFogo <- loadBMP "assets/torreFogo.bmp"
    imgTorreResina <- loadBMP "assets/torreResina.bmp"
    imgBase <- loadBMP "assets/base.bmp"
    imgPortal <- loadBMP "assets/portal.bmp"
    imgLoja <- loadBMP "assets/loja.bmp"
    imgTerra <- loadBMP "assets/terra.bmp"
    imgRelva <- loadBMP "assets/relva.bmp"
    imgAgua <- loadBMP "assets/agua.bmp"
    imgMoeda <- loadBMP "assets/moeda.bmp"
    imgMenu <- loadBMP "assets/menu.bmp"
    imgVitoria <- loadBMP "assets/menuVitoria.bmp"
    imgVitoria2 <- loadBMP "assets/menuVitoria2.bmp"
    imgFundo <- loadBMP "assets/fundo.bmp"
    imgInimigoN <- loadBMP "assets/inimigoNorte.bmp"
    imgInimigoNFogo <- loadBMP "assets/inimigoNorteFogo.bmp"
    imgInimigoNGelo <- loadBMP "assets/inimigoNorteGelo.bmp"
    imgInimigoNResina <- loadBMP "assets/inimigoNorteResina.bmp"
    imgInimigoS <- loadBMP "assets/inimigoSul.bmp"
    imgInimigoSFogo <- loadBMP "assets/inimigoSulFogo.bmp"
    imgInimigoSGelo <- loadBMP "assets/inimigoSulGelo.bmp"
    imgInimigoSResina <- loadBMP "assets/inimigoSulResina.bmp"
    imgInimigoE <- loadBMP "assets/inimigoEste.bmp"
    imgInimigoEFogo <- loadBMP "assets/inimigoEsteFogo.bmp"
    imgInimigoEGelo <- loadBMP "assets/inimigoEsteGelo.bmp"
    imgInimigoEResina <- loadBMP "assets/inimigoEsteResina.bmp"
    imgInimigoO <- loadBMP "assets/inimigoOeste.bmp"
    imgInimigoOFogo <- loadBMP "assets/inimigoOesteFogo.bmp"
    imgInimigoOGelo <- loadBMP "assets/inimigoOesteGelo.bmp"
    imgInimigoOResina <- loadBMP "assets/inimigoOesteResina.bmp"
    imgPausa <- loadBMP "assets/pausa.bmp"
    imgDerrota <- loadBMP "assets/derrota.bmp"
    imgUI <- loadBMP "assets/ui.bmp"
    imgMapa1 <- loadBMP "assets/mapa1.bmp"
    imgMapa2 <- loadBMP "assets/mapa2.bmp"
    imgLava <- loadBMP "assets/lava.bmp"
    imgCaminho <- loadBMP "assets/caminho.bmp"
    imgPedra <- loadBMP "assets/pedra.bmp"

    return Imagens {
        imgTorreGelo = imgTorreGelo,
        imgTorreFogo = imgTorreFogo,
        imgTorreResina = imgTorreResina,
        imgBase = imgBase,
        imgPortal = imgPortal,
        imgInimigoN = imgInimigoN,
        imgInimigoNFogo = imgInimigoNFogo,
        imgInimigoNGelo = imgInimigoNGelo,
        imgInimigoNResina = imgInimigoNResina,
        imgInimigoS = imgInimigoS,
        imgInimigoSFogo = imgInimigoSFogo,
        imgInimigoSGelo = imgInimigoSGelo,
        imgInimigoSResina = imgInimigoSResina,
        imgInimigoE = imgInimigoE,
        imgInimigoEFogo = imgInimigoEFogo,
        imgInimigoEGelo = imgInimigoEGelo,
        imgInimigoEResina = imgInimigoEResina,
        imgInimigoO = imgInimigoO,
        imgInimigoOFogo = imgInimigoOFogo,
        imgInimigoOGelo = imgInimigoOGelo,
        imgInimigoOResina = imgInimigoOResina,
        imgLoja = imgLoja,
        imgTerra = imgTerra,
        imgRelva = imgRelva,
        imgAgua = imgAgua,
        imgMoeda = imgMoeda,
        imgMenu = imgMenu,
        imgVitoria = imgVitoria,
        imgVitoria2 = imgVitoria2,
        imgFundo = imgFundo,
        imgPausa = imgPausa,
        imgDerrota = imgDerrota,
        imgUI = imgUI,
        imgMapa1 = imgMapa1,
        imgMapa2 = imgMapa2,
        imgLava = imgLava,
        imgCaminho = imgCaminho,
        imgPedra = imgPedra
        }