{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-} -- AR
module Main where

import Control.Monad
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

import SDL.Input.Mouse
import qualified SDL.Input.Mouse as MO

import System.Random as R

import Lib

import qualified Data.Map.Strict as M

import qualified Data.Map as MapData

import qualified Data.List as List

import SDL.Font
import qualified SDL.Font as Font
import SDL.Video

import qualified Data.Text as DT

import Data.Maybe as May

-- Magic numbers: --
-- | Taille de toutes les cases du jeu
tailleCase :: Integer
tailleCase = 50

carteEx :: Carte
carteEx = Carte (M.fromList ([(C 0 0, Herbe)
                   , (C 0 1, Eau)
                   , (C 0 2, Herbe)
                   , (C 0 3, Herbe)
                   , (C 0 4, Herbe)
                   , (C 1 0, Eau)
                   , (C 1 1, Herbe)
                   , (C 1 2, Ressource 9)
                   , (C 1 3, Herbe)
                   , (C 1 4, Ressource 8)
                   , (C 2 0, Herbe)
                   , (C 2 1, Herbe)
                   , (C 2 2, Ressource 4)
                   , (C 2 3, Herbe)
                   , (C 2 4, Eau)
                   , (C 3 0, Herbe)
                   , (C 3 1, Herbe)
                   , (C 3 2, Herbe)
                   , (C 3 3, Eau)
                   , (C 3 4, Herbe)
                   , (C 4 0, Eau)
                   , (C 4 1, Herbe)
                   , (C 4 2, Ressource 2)
                   , (C 4 3, Herbe)
                   , (C 4 4, Herbe)]))

randomNb :: Integer -> [Integer]
randomNb seed = iterate (\x -> (25210345917 * x + 11) `mod` (2^48)) seed

generateCarte :: Carte
generateCarte = Carte $ M.fromList [((C x y), getTerrain x y) | x <- [0..34], y <- [0..16]]
  where
    getTerrain x y
      | x == 17 && y == 8 = Eau -- milieu de la carte
      | x < 5 && y < 5 = Ressource 10
      | x > 29 && y < 5 = Ressource 10
      | x < 5 && y > 11 = Ressource 10
      | x > 29 && y > 11 = Ressource 10
      | otherwise = Herbe



genCarte :: Integer -> Carte
genCarte nb = Carte $ M.fromList [((C x y), getTerrain x y) | x <- [0..30], y <- [0..16]]
  where
    getTerrain x y
      | r < 0.2 = Ressource 1
      | r < 0.3 = Eau
      | otherwise = Herbe
      where
        r = fromIntegral (randomNb nb !! (x * 17 + y)) / fromIntegral (2^48)

generateCarteR :: Carte
generateCarteR = Carte $ M.fromList [((C x y), getTerrain x y) | x <- [0..34], y <- [0..16]]
  where
    getTerrain x y
      | x `elem` [16, 17, 18] && y `elem` [7, 8, 9] = Eau -- centre
      | x `elem` [0, 0, 34, 34] && y `elem` [0, 16, 0, 16] = Ressource 10 -- coins
      | otherwise = Herbe


player1 :: Joueur
player1 = Joueur "0" (JoueurId 0) 100

player2 :: Joueur
player2 = Joueur "1" (JoueurId 1) 100

player3 :: Joueur
player3 = Joueur "2" (JoueurId 2) 100

uniteCombattant :: Unite
uniteCombattant = Unite "Combattant" (C 4 3) (JoueurId 0)

uniteEtudiant :: Unite
uniteEtudiant = Unite "Collecteur" (C 0 3) (JoueurId 1)

cuve_valide_pleine :: Cuve
cuve_valide_pleine = CuvePleine 10


combattant_valide :: Combattant
combattant_valide = Combattant (UniteId 0) uniteCombattant 1 [Deplacer (C 4 3)] (Deplacer (C 1 4))

collecteur_valide :: Collecteur
collecteur_valide = Collecteur (UniteId 1) uniteEtudiant cuve_valide_pleine 3 [Collecter (C 4 4)] (Deplacer (C 4 2))

-- envRes_combattant_collecteur :: Environnement
-- envRes_combattant_collecteur = Environnement [player1, player2, player3] genCarte (M.fromList [((UniteId 2), (get_unite_Combattant combattant_valide)), ((UniteId 1), (get_unite_Collecteur collecteur_valide))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1), (BatId 4, r1), (BatId 5, c1), (BatId 6, c2)])
-- envRes_combattant_collecteur = Environnement [player1, player2, player3] genCarte (M.fromList [((UniteId 2), (get_unite_Combattant combattant_valide)), ((UniteId 1), (get_unite_Collecteur collecteur_valide))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1), (BatId 4, r1), (BatId 5, c1), (BatId 6, c2)])

qr1 :: Batiment
qr1 = Batiment "QG" 0 (C 1 2) (JoueurId 0)

qr2 :: Batiment
qr2 = Batiment "QG" 0 (C 3 0) (JoueurId 1)

qr3 :: Batiment
qr3 = Batiment "QG" 0 (C 1 3) (JoueurId 2)

u1 :: Batiment
u1 = Batiment "Usine" 0 (C 4 4) (JoueurId 0)

r1 :: Batiment
r1 = Batiment "Raffinerie" 0 (C 20 4) (JoueurId 0)

c1 :: Batiment
c1 = Batiment "Centrale" 0 (C 25 16) (JoueurId 0)

c2 :: Batiment
c2 = Batiment "Centrale" 0 (C 30 16) (JoueurId 0)


----------------------------------------------------------------- Carte --------------------------------------------------------------------------------------
-- | Fonction qui va charger les cases du jeu en fonction des coordonnées et du ID Texture et Sprite
load_carte :: Renderer -> TextureMap -> SpriteMap -> [(Coord, Terrain)] -> Int -> IO (TextureMap, SpriteMap)
load_carte rdr tmap smap listeCT cpt
    | (cpt == length listeCT) = return (tmap, smap)
    | otherwise = do
        let (C x y, terrain) = listeCT !! cpt
            textureSpriteID = "" ++ show x ++ " " ++ show y
        case terrain of
            Ressource _ -> do
                tmap' <- TM.loadTexture rdr "assets/ressource.bmp" (TextureId textureSpriteID) tmap
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                    smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                load_carte rdr tmap' smap' listeCT (cpt+1)
            Eau -> do
                tmap' <- TM.loadTexture rdr "assets/eau.bmp" (TextureId textureSpriteID) tmap
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                    smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                load_carte rdr tmap' smap' listeCT (cpt+1)
            ortherwise -> do
                tmap' <- TM.loadTexture rdr "assets/herbe.bmp" (TextureId textureSpriteID) tmap
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                    smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                load_carte rdr tmap' smap' listeCT (cpt+1)

display_carte :: [(Coord, Terrain)]  -> Renderer -> TextureMap -> SpriteMap -> Int -> IO ()
display_carte listeCT rdr tmap smap cpt
    | (cpt == length listeCT) = return ()
    | otherwise = do
        let (C x y, terrain) = listeCT !! cpt
            textureSpriteID = "" ++ show x ++ " " ++ show y
        S.displaySprite rdr tmap (SM.fetchSprite (SpriteId textureSpriteID) smap)
        display_carte listeCT rdr tmap smap (cpt+1)

----------------------------------------------------------------- Batiments --------------------------------------------------------------------------------------
-- data Batiment = Batiment {bNom :: String, prix :: Int, batCoord :: Coord, batProprio :: JoueurId} deriving (Eq, Show)
-- |
load_batiments :: Renderer -> TextureMap -> SpriteMap -> [(BatId, Batiment)] -> Int -> IO (TextureMap, SpriteMap)
load_batiments rdr tmap smap listeBats cpt
    | (cpt == length listeBats) = return (tmap, smap)
    | otherwise = do
        let (BatId batId) = fst (listeBats !! cpt)
            (Batiment bNom _ batCoord _) = snd (listeBats !! cpt)
            (C x y) = batCoord
            textureSpriteID = "B" ++ (show batId)
        case bNom of
            "QG" -> do
                tmap' <- if (M.member (TextureId textureSpriteID) tmap)
                  then do
                    let res = M.delete (TextureId textureSpriteID) tmap
                    TM.loadTexture rdr "assets/QG.bmp" (TextureId textureSpriteID) res
                  else
                    TM.loadTexture rdr "assets/QG.bmp" (TextureId textureSpriteID) tmap
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                    smap' = if (M.member (SpriteId textureSpriteID) smap) then do
                      SM.changeSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                      else
                        SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            "Raffinerie" ->  do
                tmap' <- if (M.member (TextureId textureSpriteID) tmap)
                then do
                  let res = M.delete (TextureId textureSpriteID) tmap
                  TM.loadTexture rdr "assets/Raffinerie.bmp" (TextureId textureSpriteID) res
                else
                  TM.loadTexture rdr "assets/Raffinerie.bmp" (TextureId textureSpriteID) tmap
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                    smap' = if (M.member (SpriteId textureSpriteID) smap) then do
                      SM.changeSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                      else
                        SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            "Usine" -> do
                tmap' <- if (M.member (TextureId textureSpriteID) tmap)
                  then do
                    let res = M.delete (TextureId textureSpriteID) tmap
                    TM.loadTexture rdr "assets/Usine.bmp" (TextureId textureSpriteID) res
                  else
                    TM.loadTexture rdr "assets/Usine.bmp" (TextureId textureSpriteID) tmap
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                    smap' = if (M.member (SpriteId textureSpriteID) smap) then do
                        SM.changeSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                        else
                          SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            "Centrale" -> do
                tmap' <- if (M.member (TextureId textureSpriteID) tmap)
                  then do
                    let res = M.delete (TextureId textureSpriteID) tmap
                    TM.loadTexture rdr "assets/Centrale.bmp" (TextureId textureSpriteID) res
                  else
                    TM.loadTexture rdr "assets/Centrale.bmp" (TextureId textureSpriteID) tmap
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                    smap' = if (M.member (SpriteId textureSpriteID) smap) then do
                      SM.changeSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                      else
                        SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            otherwise -> do
                return (tmap, smap)

display_batiments :: [(BatId, Batiment)] -> Renderer -> TextureMap -> SpriteMap -> Int -> IO ()
display_batiments listeBats rdr tmap smap cpt
    | (cpt == length listeBats) = return ()
    | otherwise = do
        let (BatId batId) = fst (listeBats !! cpt)
            batt@(Batiment bNom _ batCoord _) = snd (listeBats !! cpt)
            (C x y) = batCoord
            textureSpriteID = "B" ++ (show batId)
        S.displaySprite rdr tmap (SM.fetchSprite (SpriteId textureSpriteID) smap)
        drawRectBatiment batt rdr
        display_batiments listeBats rdr tmap smap (cpt+1)

----------------------------------------------------------------- Unite --------------------------------------------------------------------------------------
-- data Unite = Unite {uNom :: String, unitCoord :: Coord, unitProprio :: JoueurId} deriving (Eq, Show, Ord)
load_unites :: Renderer -> TextureMap -> SpriteMap -> [(UniteId, Unite)] -> Int -> IO (TextureMap, SpriteMap)
load_unites rdr tmap smap listUnite cpt
    | (cpt == length listUnite) = return (tmap, smap)
    | otherwise = do
        let (Unite uNom uCoord _ ) = snd (listUnite !! cpt)
            (UniteId id) = fst (listUnite !! cpt)
            (C x y) = uCoord
            textureSpriteID = "U" ++ show id
        case uNom of
          "Collecteur" -> do
              tmap' <- if (M.member (TextureId textureSpriteID) tmap) then do
                    let res = M.delete (TextureId textureSpriteID) tmap
                    TM.loadTexture rdr "assets/Collector_face.bmp" (TextureId textureSpriteID) res
                  else
                    TM.loadTexture rdr "assets/Collector_face.bmp" (TextureId textureSpriteID) tmap
              let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                  smap' = if (M.member (SpriteId textureSpriteID) smap) then do
                      SM.changeSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                      else
                        SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
              load_unites rdr tmap' smap' listUnite (cpt+1)
          "Combattant" -> do
              tmap' <- if (M.member (TextureId textureSpriteID) tmap)
                    then do
                      let res = M.delete (TextureId textureSpriteID) tmap
                      TM.loadTexture rdr "assets/Soldat_face.bmp" (TextureId textureSpriteID) res
                    else
                      TM.loadTexture rdr "assets/Soldat_face.bmp" (TextureId textureSpriteID) tmap
              let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase))
                  smap' = if (M.member (SpriteId textureSpriteID) smap) then do
                      SM.changeSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
                      else
                        SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap
              load_unites rdr tmap' smap' listUnite (cpt+1)
          otherwise -> do
              return (tmap, smap)

display_unite :: [(UniteId, Unite)]  -> Renderer -> TextureMap -> SpriteMap -> Int -> IO ()
display_unite listeU rdr tmap smap cpt
    | (cpt == length listeU) = return ()
    | otherwise = do
        let unitt@(Unite uNom uCoord _ ) = snd (listeU !! cpt)
            (UniteId id) = fst (listeU !! cpt)
            (C x y) = uCoord
            textureSpriteID = "U" ++ show id
        S.displaySprite rdr tmap (SM.fetchSprite (SpriteId textureSpriteID) smap)
        Main.drawRectUnite unitt rdr
        display_unite listeU rdr tmap smap (cpt+1)
------------------------------------------------------------------- Menu ----------------------------------------------------------------------------------------
load_menu :: String -> Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
load_menu textureSpriteID rdr tmap smap = do
  tmap' <- TM.loadTexture rdr ("assets/" ++ textureSpriteID ++ ".bmp") (TextureId textureSpriteID) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 190 200)
  let smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite 1550 0) smap
  return (tmap', smap')


display_collector_meu :: String -> Renderer -> TextureMap -> SpriteMap -> IO ()
display_collector_meu textureSpriteID rdr tmap smap = S.displaySprite rdr tmap (SM.fetchSprite (SpriteId textureSpriteID) smap) -- peut etre a changer

load_decor :: String -> Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
load_decor textureSpriteID rdr tmap smap = do
  tmap' <- TM.loadTexture rdr ("assets/" ++ textureSpriteID ++ ".bmp") (TextureId textureSpriteID) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 190 200)
  let smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite 1550 650) smap
  return (tmap', smap')

gerer_collecteurs_ordres :: [Collecteur] -> TextureMap -> SpriteMap -> Int -> (TextureMap, SpriteMap)
gerer_collecteurs_ordres listeDiff tmap smap cpt =
  if cpt == length listeDiff then (tmap, smap)
  else
    let collec@(Collecteur (UniteId num) (Unite _ (C x y) _) _ _ _ _) = listeDiff !! cpt
    in
      let spriteAncienne = SM.fetchSprite (SpriteId ("U"++(show num))) smap
      in
        let spriteNouvelle = S.moveTo spriteAncienne (fromIntegral (x*50)) (fromIntegral (y*50))
        in
          gerer_collecteurs_ordres listeDiff tmap (SM.changeSprite (SpriteId ("U"++(show num))) spriteNouvelle smap) (cpt+1)


gerer_combattants_ordres :: [Combattant] -> TextureMap -> SpriteMap -> Int -> (TextureMap, SpriteMap)
gerer_combattants_ordres listeDiff tmap smap cpt =
  if cpt == length listeDiff then (tmap, smap)
  else
    let combat@(Combattant (UniteId num) (Unite _ (C x y) _) _ _ _) = listeDiff !! cpt
    in
      let spriteAncienne = SM.fetchSprite (SpriteId ("U"++(show num))) smap
      in
        let spriteNouvelle = S.moveTo spriteAncienne (fromIntegral (x*50)) (fromIntegral (y*50))
        in
          gerer_combattants_ordres listeDiff tmap (SM.changeSprite (SpriteId ("U"++(show num))) spriteNouvelle smap) (cpt+1)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------

getMouseLocation :: Event -> Maybe (V2 Int)
getMouseLocation event =
    case eventPayload event of
        MouseButtonEvent mouseButton ->
            let P coordinates = fmap fromIntegral (mouseButtonEventPos mouseButton) in
                Just coordinates
        _ -> Nothing

-- | Verification si un evenement est un clic de souris
isMouseButtonEvent :: SDL.Event -> Bool
isMouseButtonEvent SDL.Event { SDL.eventPayload = SDL.MouseButtonEvent _ } = True
isMouseButtonEvent _ = False

isMouseButtonEventPressed :: SDL.Event -> Bool
isMouseButtonEventPressed SDL.Event { SDL.eventPayload = SDL.MouseButtonEvent eventData } =
    SDL.mouseButtonEventMotion eventData == SDL.Pressed
isMouseButtonEventPressed _ = False

processMouseEventBatiments :: Int -> Int -> [(BatId, Batiment)] -> SDL.Event -> Int -> (String, Maybe JoueurId, Maybe Coord)
processMouseEventBatiments xMouse yMouse listeB e@(SDL.Event _ (SDL.MouseButtonEvent eventData)) cpt
  | (cpt == length listeB) = if (xMouse >= (1740-190)) && (yMouse <= 200) then ("", Nothing, Nothing) else ("Menu_Default", Nothing, Nothing)
  | otherwise =
    let (Batiment bNom _ batCoord bJoueurID) = snd (listeB !! cpt)
        (C x y) = batCoord
        (BatId batId) = fst (listeB !! cpt) in
    if ((xMouse >= x*50) && (xMouse <= x*50 + (fromInteger tailleCase))) && ((yMouse >= y*50) && (yMouse <= y*50+(fromInteger tailleCase))) then
      if (bNom == "QG") then ("QG_Menu", Just bJoueurID, Just (C x y))
      else if (bNom == "Usine") then ("Usine_Menu", Just bJoueurID, Just (C x y))
      else if (bNom == "Raffinerie") then ("Raffinerie_Menu", Just bJoueurID, Just (C x y))
      else if (bNom == "Centrale") then ("Centrale_Menu", Just bJoueurID, Just (C x y))
      else ("Error Menu", Nothing, Nothing)
    else
      (processMouseEventBatiments xMouse yMouse listeB e (cpt+1))

-- | Traitement d'un clic de souris sur une des Unites ou un des Batiments
processMouseEvent :: Int -> Int -> [(UniteId, Unite)] -> [(BatId, Batiment)] -> SDL.Event -> Int -> (String, Maybe JoueurId, Maybe Coord)
processMouseEvent xMouse yMouse listeU listB e@(SDL.Event _ (SDL.MouseButtonEvent eventData)) cpt
  | (cpt == length listeU) =
      processMouseEventBatiments xMouse yMouse listB e 0
  | otherwise =
      let (Unite uNom uCoord uProprio) = snd (listeU !! cpt)
          (UniteId id) = fst (listeU !! cpt)
          (C x y) = uCoord in
      if ((xMouse >= x*50) && (xMouse <= x*50 + (fromInteger tailleCase))) && ((yMouse >= y*50) && (yMouse <= y*50+(fromInteger tailleCase))) then
        if (uNom == "Combattant") then ("Combattant_Menu", Just uProprio, Just (C x y))
        else if (uNom == "Collecteur") then ("Collecteur_Menu", Just uProprio, Just (C x y))
        else ("Error Menu", Nothing, Nothing)
      else
        processMouseEvent xMouse yMouse listeU listB e (cpt+1)
  -- processMouseEvent _ _ _ _ _ = return ()
  -- putStrLn $ "Mouse button " ++ show (SDL.mouseButtonEventButton eventData) ++ " pressed || X mouse test: " <> (show x) <> " Y mouse: " <> (show y)


processMouseEventMenus :: Int -> Int -> String -> SDL.Event -> String
processMouseEventMenus  xMouse yMouse menuID e@(SDL.Event _ (SDL.MouseButtonEvent eventData)) =
  case menuID of
    "QG_Menu" ->
      if (((xMouse >= (1740-190)+22) && (xMouse <= (1740-190)+170)) && ((yMouse >= 60) && (yMouse <= 82))) then "QG_Raffinerie"
      else if (((xMouse >= (1740-190)+53) && (xMouse <=  (1740-190)+131)) && ((yMouse >= 97) && (yMouse <= 120))) then "QG_Usine"
      else if (((xMouse >= (1740-190)+27) && (xMouse <= (1740-190)+155)) && ((yMouse >= 135) && (yMouse <= 155))) then "QG_Centrale"
      else menuID
    "QG_Raffinerie" ->
      if (((xMouse >= (1740-190)+22) && (xMouse <= (1740-190)+170)) && ((yMouse >= 60) && (yMouse <= 82))) then "QG_Raffinerie"
      else if (((xMouse >= (1740-190)+53) && (xMouse <=  (1740-190)+131)) && ((yMouse >= 97) && (yMouse <= 120))) then "QG_Usine"
      else if (((xMouse >= (1740-190)+27) && (xMouse <= (1740-190)+155)) && ((yMouse >= 135) && (yMouse <= 155))) then "QG_Centrale"
      else "QG_Raffinerie"
    "QG_Usine" ->
      if (((xMouse >= (1740-190)+22) && (xMouse <= (1740-190)+170)) && ((yMouse >= 60) && (yMouse <= 82))) then "QG_Raffinerie"
      else if (((xMouse >= (1740-190)+53) && (xMouse <=  (1740-190)+131)) && ((yMouse >= 97) && (yMouse <= 120))) then "QG_Usine"
      else if (((xMouse >= (1740-190)+27) && (xMouse <= (1740-190)+155)) && ((yMouse >= 135) && (yMouse <= 155))) then "QG_Centrale"
      else "QG_Usine"
    "QG_Centrale" ->
      if (((xMouse >= (1740-190)+22) && (xMouse <= (1740-190)+170)) && ((yMouse >= 60) && (yMouse <= 82))) then "QG_Raffinerie"
      else if (((xMouse >= (1740-190)+53) && (xMouse <=  (1740-190)+131)) && ((yMouse >= 97) && (yMouse <= 120))) then "QG_Usine"
      else if (((xMouse >= (1740-190)+27) && (xMouse <= (1740-190)+155)) && ((yMouse >= 135) && (yMouse <= 155))) then "QG_Centrale"
      else "QG_Centrale"

    "Usine_Menu"->
      if (((xMouse >= (1740-190)+24) && (xMouse <= (1740-190)+171)) && ((yMouse >= 56) && (yMouse <= 83))) then "Usine_Collecteur"
      else if (((xMouse >= (1740-190)+21) && (xMouse <= (1740-190)+179)) && ((yMouse >= 88) && (yMouse <= 113))) then "Usine_Combattant"
      else "Usine_Menu"
    "Usine_Collecteur"->
      if (((xMouse >= (1740-190)+24) && (xMouse <= (1740-190)+171)) && ((yMouse >= 56) && (yMouse <= 83))) then "Usine_Collecteur"
      else if (((xMouse >= (1740-190)+21) && (xMouse <= (1740-190)+179)) && ((yMouse >= 88) && (yMouse <= 113))) then "Usine_Combattant"
      else "Usine_Collecteur"
    "Usine_Combattant"->
      if (((xMouse >= (1740-190)+24) && (xMouse <= (1740-190)+171)) && ((yMouse >= 56) && (yMouse <= 83))) then "Usine_Collecteur"
      else if (((xMouse >= (1740-190)+21) && (xMouse <= (1740-190)+179)) && ((yMouse >= 88) && (yMouse <= 113))) then "Usine_Combattant"
      else "Usine_Combattant"

    "Collecteur_Menu"->
      if (((xMouse >= (1740-190)+35) && (xMouse <= (1740-190)+135)) && ((yMouse >= 80) && (yMouse <= 108))) then "Collecteur_Move"
      else if (((xMouse >= (1740-190)+35) && (xMouse <= (1740-190)+165)) && ((yMouse >= 118) && (yMouse <= 145))) then "Collecteur_Collect"
      else "Collecteur_Menu"
    "Collecteur_Move"->
      if (((xMouse >= (1740-190)+35) && (xMouse <= (1740-190)+135)) && ((yMouse >= 80) && (yMouse <= 108))) then "Collecteur_Move"
      else if (((xMouse >= (1740-190)+35) && (xMouse <= (1740-190)+165)) && ((yMouse >= 118) && (yMouse <= 145))) then "Collecteur_Collect"
      else "Collecteur_Move"
    "Collecteur_Collect"->
      if (((xMouse >= (1740-190)+35) && (xMouse <= (1740-190)+135)) && ((yMouse >= 80) && (yMouse <= 108))) then "Collecteur_Move"
      else if (((xMouse >= (1740-190)+35) && (xMouse <= (1740-190)+165)) && ((yMouse >= 118) && (yMouse <= 145))) then "Collecteur_Collect"
      else "Collecteur_Collect"

    "Combattant_Menu"->
      if (((xMouse >= (1740-190)+15) && (xMouse <= (1740-190)+165)) && ((yMouse >= 83) && (yMouse <= 108))) then "Combattant_Patrouiller"
      else if (((xMouse >= (1740-190)+25) && (xMouse <= (1740-190)+140)) && ((yMouse >= 118) && (yMouse <= 143))) then "Combattant_Deplacer"
      else "Combattant_Menu"
    "Combattant_Patrouiller"->
       if (((xMouse >= (1740-190)+15) && (xMouse <= (1740-190)+165)) && ((yMouse >= 83) && (yMouse <= 108))) then "Combattant_Patrouiller"
      else if (((xMouse >= (1740-190)+25) && (xMouse <= (1740-190)+140)) && ((yMouse >= 118) && (yMouse <= 143))) then "Combattant_Deplacer"
      else "Combattant_Patrouiller"
    "Combattant_Deplacer"->
      if (((xMouse >= (1740-190)+15) && (xMouse <= (1740-190)+165)) && ((yMouse >= 83) && (yMouse <= 108))) then "Combattant_Patrouiller"
      else if (((xMouse >= (1740-190)+25) && (xMouse <= (1740-190)+140)) && ((yMouse >= 118) && (yMouse <= 143))) then "Combattant_Deplacer"
      else "Combattant_Deplacer"

    otherwise -> menuID


-- TODO a voir si on peut pas faire mieux
processMouseEventAchat :: Int -> Int -> String -> SDL.Event -> String
processMouseEventAchat xMouse _ menuID e@(SDL.Event _ (SDL.MouseButtonEvent eventData)) =
  case menuID of
    "QG_Raffinerie" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "QG_Usine" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "QG_Centrale" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "Usine_Collecteur" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "Usine_Combattant" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "Combattant_Deplacer" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "Collecteur_Move" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "Combattant_Patrouiller" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    "Collecteur_Collect" ->
      if (xMouse < 1740-190) then  "Carte"
      else ""
    otherwise -> ""



loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadPerso2 :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso2 rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "virus") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "virus") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "virus") sprite smap
  return (tmap', smap')

loadBackground :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1740 850)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

drawBlackRect :: SDL.Renderer -> IO ()
drawBlackRect renderer = do
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 173 140 118 255
  SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 1550 200) (SDL.V2 190 650))

-- | Dessine une carre creux de couleur differente en fonction de l'ID du joueur pour les Unites
drawRectUnite :: Unite -> SDL.Renderer -> IO ()
drawRectUnite (Unite uNom (C x y) (JoueurId num)) renderer = do
    if (num == 0) then do -- rouge
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255
      SDL.drawRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral (x*50)) (fromIntegral (y*50))) (SDL.V2 50 50))
    else do -- bleu, que 2 joueurs
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
      SDL.drawRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral (x*50)) (fromIntegral (y*50))) (SDL.V2 50 50))

-- | Dessine une carre creux de couleur differente en fonction de l'ID du joueur pour les Batiment
drawRectBatiment :: Batiment -> SDL.Renderer -> IO ()
drawRectBatiment (Batiment _ _ (C x y) (JoueurId num)) renderer = do
    if (num == 0) then do -- rouge
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255
      SDL.drawRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral (x*50)) (fromIntegral (y*50))) (SDL.V2 50 50))
    else do -- bleu, que 2 joueurs
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
      SDL.drawRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral (x*50)) (fromIntegral (y*50))) (SDL.V2 50 50))

drawBlackLIne1 :: SDL.Renderer -> IO ()
drawBlackLIne1 renderer = do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 1550 200) (SDL.V2 4 650))

drawBlackLIne2 :: SDL.Renderer -> IO ()
drawBlackLIne2 renderer = do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 1736 200) (SDL.V2 4 650))

-- | Renvoie le JoueurId du Batiment/Unite clique
get_joueurID_from_coord :: Environnement -> Coord -> JoueurId
get_joueurID_from_coord (Environnement joueurs mapp unites batiments) coordClick =
  let resBats = M.elems (M.filter (\(Batiment _ _ coords _) -> coords == coordClick) batiments)
  in
    if (resBats /= []) then bproprio (head resBats)
    else -- on cherche dans les unites
      uproprio (head (M.elems (M.filter (\(Unite _ coords _) -> coords == coordClick) unites)))


-- | Retourne le numero ID du JoueurId
get_id_JoueurId :: JoueurId -> Int
get_id_JoueurId (JoueurId num) = num


-- | Retourne vrai si un Batiment/Unite que le JoueurId a clique lui appartient
is_my_object :: Environnement -> Coord -> JoueurId -> Bool
is_my_object (Environnement joueurs mapp unites batiments) (C x y) joueurID =
  let inBat = M.elems (M.filter (\(Batiment _ _ (C xBat yBat) bProprio) -> (C xBat yBat) == (C x y) && bProprio == joueurID) batiments)
  in
    if (inBat == []) then
      (List.length (M.elems (M.filter (\(Unite _ uCoords uProprio) -> uCoords == (C x y) && uProprio == joueurID) unites)) > 0)
    else
      True

get_Combattant_from_list :: Coord -> [Combattant] -> Combattant
get_Combattant_from_list coord listeCombattant =
  head (List.filter (\(Combattant _ (Unite _ uCoord _) _ _ _) -> uCoord == coord) listeCombattant)

get_Collecteur_from_list :: Coord -> [Collecteur] -> Collecteur
get_Collecteur_from_list coord listeCollecteur =
  head (List.filter (\(Collecteur _ (Unite _ uCoord _) _ _ _ _) -> uCoord == coord) listeCollecteur)

get_Unite_from_list_ID :: UniteId -> [Combattant] -> [Collecteur] -> Unite
get_Unite_from_list_ID (UniteId num) listeCombattants listeCollecteurs =
  let resCombattans = List.filter (\(Combattant (UniteId uniID) _ _ _ _) -> uniID == num) listeCombattants
  in
    if (resCombattans /= []) then
      get_unite_Combattant (head resCombattans)
    else
      get_unite_Collecteur (head (List.filter (\(Collecteur (UniteId uniID) _ _ _ _ _) -> uniID == num) listeCollecteurs))

get_PV_Batiment :: Batiment -> Int
get_PV_Batiment (Batiment _ pv _ _) = pv

get_PV_from_Batiments :: Coord -> M.Map BatId Batiment -> Int
get_PV_from_Batiments coord bats =
  let res = (M.elems (M.filter (\(Batiment _ _ bCoord _) -> coord == bCoord) bats))
  in
    if (length res > 0) then get_PV_Batiment (head res) else 0

-- | Renvoie le PV de l'unite situe a une coordonnee precise
get_PV_from_Unites :: Coord -> [Combattant] -> [Collecteur] -> Int
get_PV_from_Unites coord listeCombat listeCollect =
  let resCombat = List.filter (\(Combattant _ (Unite _ uCoord _) _ _ _) -> uCoord == coord) listeCombat
  in
    if (resCombat /= []) then
      let (Combattant _ _ pvv _ _) =  head resCombat
      in
        pvv
    else
      let resCollect = List.filter (\(Collecteur _ (Unite _ uCoord _) _ _ _ _) -> uCoord == coord) listeCollect
      in
        if (length resCollect == 0) then 0
        else
          let (Collecteur _ _ _ pvv _ _) = head resCollect
          in
            pvv

-- | Renvoie le PV de l'unite situe a une coordonnee precise
get_Cuve_Collecteur :: Coord -> [Collecteur] -> Int
get_Cuve_Collecteur coord listeCollect =
  let resCollect = List.filter (\(Collecteur _ (Unite _ uCoord _) _ _ _ _) -> uCoord == coord) listeCollect
  in
    if (length resCollect == 0) then 0
    else
      let (Collecteur _ _ cuve _ _ _) = head resCollect
      in
        fromIntegral (getCuveVal cuve)


-----------------------------------------------------------------------------------------------------------
-------------------------------------------------- Main ---------------------------------------------------
-----------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Dune II" $ defaultWindow { windowInitialSize = V2 1740 850 }
  renderer <- createRenderer window (-1) defaultRenderer


  let carteEnv = genCarte 2
  let environnement = envSmart carteEnv [(C 1 1), (C 27 15)]
  if (prop_inv_Environnement environnement) then return ()
  else error "[Invariant] Mauvaise generation de carte"

  let (Environnement joueurs mapp unites batiments) = environnement

  (environnement, listeCombattants) <- return (set_combattant environnement (C 26 15) (getJoueurByJoueurID (JoueurId 1) joueurs) [])
  
  -- if (prop_pre_set_combattant environnement (C 26 15) (getJoueurByJoueurID (JoueurId 1) joueurs) [])  &&
  --                                       (prop_post_set_combattant environnement (C 26 15) (getJoueurByJoueurID (JoueurId 1) joueurs) []) then do
  --                                         let (resEnv, resListeCombattants) = (set_combattant environnement (C 26 15) (getJoueurByJoueurID (JoueurId 1) joueurs) [])
  --                                         putStrLn $ "Achat Combattant PC"
  --                                         return (resEnv, resListeCombattants)
  --                                       else error "pas bien ici"


   -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/herbe.bmp" TM.createTextureMap SM.createSpriteMap
  
  (tmap1, smap1) <- load_menu "QG_Menu" renderer tmap smap
  (tmap2, smap2) <- load_menu "QG_Raffinerie" renderer tmap1 smap1
  (tmap3, smap3) <- load_menu "QG_Usine" renderer tmap2 smap2
  (tmap4, smap4) <- load_menu "QG_Centrale" renderer tmap3 smap3
  (tmap5, smap5) <- load_menu "Collecteur_Menu" renderer tmap4 smap4

  (tmap6, smap6) <- load_carte renderer tmap5 smap5 (M.toList (carte carteEnv)) 0
  (tmap7, smap7) <- load_batiments renderer tmap6 smap6 (M.toList batiments) 0
  (tmap8, smap8) <- load_unites renderer tmap7 smap7 (M.toList unites) 0

  (tmap9, smap9) <- load_menu "Usine_Menu" renderer tmap8 smap8
  (tmap10, smap10) <- load_menu "Raffinerie_Menu" renderer tmap9 smap9
  (tmap11, smap11) <- load_menu "Centrale_Menu" renderer tmap10 smap10
  (tmap12, smap12) <- load_menu "Menu_Default" renderer tmap11 smap11
  (tmap13, smap13) <- load_menu "Combattant_Menu" renderer tmap12 smap12
  (tmap14, smap14) <- load_menu "Usine_Combattant" renderer tmap13 smap13
  (tmap15, smap15) <- load_menu "Usine_Collecteur" renderer tmap14 smap14
  (tmap16, smap16) <- load_decor "Decor" renderer tmap15 smap15

  (tmap17, smap17) <- load_menu "Collecteur_Collect" renderer tmap16 smap16
  (tmap18, smap18) <- load_menu "Collecteur_Move" renderer tmap17 smap17

  (tmap19, smap19) <- load_menu "Combattant_Deplacer" renderer tmap18 smap18
  (tmap20, smap20) <- load_menu "Combattant_Patrouiller" renderer tmap19 smap19

  -- Ajout des objets pour le PC 
  (Environnement joueurs mapp unites batiments) <- return environnement
  (tmap00, smap00) <- load_unites renderer tmap20 smap20 [(M.toList unites) !! ((length (M.toList unites)) - 1)]  0  
  listeCombattants <- return (set_ordre_pattrouiller_combattant (get_Combattant_from_list (C 26 15) listeCombattants) listeCombattants (C 26 10))



  -- initialisation de l'état du jeu
  let gameState_perso1 = M.initGameState
  virusX <- randomRIO (0, 640-100) :: IO Int
  virusY <- randomRIO (0, 480-100) :: IO Int
  let gameState_perso2 = M.GameState virusX virusY 0
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop

  Font.initialize


  gameLoop 30 renderer tmap00 smap00 kbd [gameState_perso1, gameState_perso2] "Menu_Default" environnement [] listeCombattants Nothing Nothing

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> [GameState] -> String -> Environnement -> [Collecteur] -> [Combattant] -> Maybe JoueurId -> Maybe Coord -> IO ()
gameLoop frameRate renderer tmap smap kbd [gameState_perso1, gameState_perso2] menuID envRes_combattant_collecteur listeCollecteurs listeCombattants joueurIdCourant uniteStocke = do
  startTime <- time
  --- ensemble des events
  events <- pollEvents
  --- events du clavier
  let kbd' = K.handleEvents events kbd
  --- events souris
  let mouseButtonEvents = filter isMouseButtonEventPressed events

  clear renderer


  let (Environnement joueurs mapp unites batiments) = envRes_combattant_collecteur


 --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap) -- peut etre a changer
  display_carte (M.toList (carte mapp)) renderer tmap smap 0
  display_unite (M.toList unites) renderer tmap smap 0
  display_collector_meu menuID renderer tmap smap
  display_batiments (M.toList batiments) renderer tmap smap 0


 --- location de la souris
  test <- MO.getAbsoluteMouseLocation
  let SDL.P (SDL.V2 x1 y1) = test
  let mousePos = SDL.P (SDL.V2 x1 y1)
      x = case mousePos of
            SDL.P (SDL.V2 x _) -> fromIntegral x
      y = case mousePos of
            SDL.P (SDL.V2 _ y) -> fromIntegral y

  let fmapRes = (fmap (\event -> processMouseEvent x y (M.toList unites) (M.toList batiments) event 0) mouseButtonEvents)
  let (res, resJoueurID, resCoordUnite) = if (length fmapRes > 0) then head fmapRes else ("", Nothing, Nothing)
  uniteStocke <- if (length fmapRes > 0) && (menuID /= "Combattant_Deplacer") && (menuID /= "Combattant_Menu") && (menuID /= "Collecteur_Menu") &&
    (menuID /= "Collecteur_Move") && (menuID /= "Combattant_Patrouiller") && (menuID /= "Collecteur_Collect") then return resCoordUnite else return uniteStocke

  ---- affichage credits
  drawBlackRect renderer
  font <- Font.load "04B_30__.TTF" 24
  white <- pure $ V4 255 255 255 255
  blue <- pure $ V4 0 0 255 255
  red <- pure $ V4 255 0 0 255
  couleur <- return white
  couleur <- if (isJust joueurIdCourant) && (get_id_JoueurId (May.fromJust joueurIdCourant)) == 0 then return red else return blue
  if (isJust joueurIdCourant) then do
    surface <- Font.solid font white (DT.pack ("Credits:" <> (show (get_player_credit (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs)))))
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 250) (V2 175 50))
    surface <- Font.solid font couleur (DT.pack ("Joueur ID:" <> (show (get_id_JoueurId (May.fromJust joueurIdCourant)))))
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 450) (V2 175 50))
  else if (menuID  == "QG_Raffinerie") then do
    surface <- Font.solid font white (DT.pack ("Prix:" <> (show prixRaffinerie) ))
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 350) (V2 175 50))
    pure ()
  else if (menuID  == "QG_Usine") then do
    surface <- Font.solid font white (DT.pack ("Prix:" <> (show prixUsine)))
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 350) (V2 175 50))
  else if (menuID  == "QG_Centrale") then do
    surface <- Font.solid font white (DT.pack ("Prix:" <> (show prixCentrale)))
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 350) (V2 175 50))
    pure ()
  else if (menuID  == "Usine_Combattant") then do
    surface <- Font.solid font white (DT.pack ("Prix:" <> (show prixCombattant)))
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 350) (V2 175 50))
    pure ()
  else if (menuID  == "Usine_Collecteur") then do
    surface <- Font.solid font white (DT.pack ("Prix:" <> (show prixCollecteur)))
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 350) (V2 175 50))
    pure ()
  else pure ()

  display_collector_meu "Decor" renderer tmap smap
  drawBlackLIne1 renderer -- ligne menu gauche
  drawBlackLIne2 renderer -- ligne menu droite

  -- if (isJust uniteStocke) then putStrLn $ "is est laaa, menuID: "<>menuID else putStrLn $ "il est PAS LA, menuID: " <> menuID

  menuID <- if ((res /= "") && (menuID /= "QG_Raffinerie") && (menuID /= "QG_Usine") &&
    (menuID /= "QG_Centrale") && (menuID /= "Usine_Collecteur") && (menuID /= "Usine_Combattant") && (menuID /= "Combattant_Deplacer") && (menuID /= "Collecteur_Move")
    && (menuID /= "Combattant_Patrouiller") && (menuID /= "Collecteur_Collect")) then do
      putStrLn $ res
      return res
    else do return (menuID)
  joueurIdCourant <- if (isJust resJoueurID) && (menuID /= "Usine_Collecteur") && (menuID /= "Usine_Combattant") then return resJoueurID else return joueurIdCourant

---------------------------------------------------changement de menu--------------------------------------------------------------------------------
  -- Changement de menu en fonction d'ou l'utilisateur clique et dans quel menu il est deja
  menuID <- if ((menuID == "Menu_Default") || (menuID == "Raffinerie_Menu") || (menuID == "Centrale_Menu")) then do
    if (isJust uniteStocke) && (menuID /= "Menu_Default") then do
          surface <- Font.solid font white (DT.pack ("PV:" <> (show (get_PV_from_Batiments (May.fromJust uniteStocke) batiments))))
          surfaceTexture <- createTextureFromSurface renderer surface
          copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 550) (V2 175 50))
          return menuID
    else return menuID
    return menuID
    else do
      if (menuID  == "QG_Menu") then do
        if (isJust uniteStocke) then do
          surface <- Font.solid font white (DT.pack ("PV:" <> (show (get_PV_from_Batiments (May.fromJust uniteStocke) batiments))))
          surfaceTexture <- createTextureFromSurface renderer surface
          copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 550) (V2 175 50))
        else return ()
        let res2 = fmap (\event -> processMouseEventMenus x y "QG_Menu" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "QG_Raffinerie") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "QG_Raffinerie" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "QG_Usine") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "QG_Usine" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "QG_Centrale") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "QG_Centrale" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Usine_Menu") then do
        if (isJust uniteStocke) then do
          surface <- Font.solid font white (DT.pack ("PV:" <> (show (get_PV_from_Batiments (May.fromJust uniteStocke) batiments))))
          surfaceTexture <- createTextureFromSurface renderer surface
          copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 550) (V2 175 50))
        else return ()
        let res2 = fmap (\event -> processMouseEventMenus x y "Usine_Menu" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Usine_Collecteur") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Usine_Collecteur" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Usine_Combattant") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Usine_Combattant" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Collecteur_Menu") then do
        if (isJust uniteStocke) then do
          surface <- Font.solid font white (DT.pack ("PV:" <> (show (get_PV_from_Unites (May.fromJust uniteStocke) listeCombattants listeCollecteurs))))
          surfaceTexture <- createTextureFromSurface renderer surface
          copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 550) (V2 175 50))
          surface <- Font.solid font white (DT.pack ("Cuve:" <> (show (get_Cuve_Collecteur (May.fromJust uniteStocke) listeCollecteurs)) <> "/" <> show cuveMax))
          surfaceTexture <- createTextureFromSurface renderer surface
          copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 650) (V2 175 50))
        else return ()
        let res2 = fmap (\event -> processMouseEventMenus x y "Collecteur_Menu" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Collecteur_Collect") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Collecteur_Collect" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Collecteur_Move") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Collecteur_Move" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Combattant_Menu") then do
        if (isJust uniteStocke) then do
          surface <- Font.solid font white (DT.pack ("PV:" <> (show (get_PV_from_Unites (May.fromJust uniteStocke) listeCombattants listeCollecteurs))))
          surfaceTexture <- createTextureFromSurface renderer surface
          copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+5) 550) (V2 175 50))
        else return ()
        let res2 = fmap (\event -> processMouseEventMenus x y "Combattant_Menu" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Combattant_Patrouiller") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Combattant_Patrouiller" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Combattant_Deplacer") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Combattant_Deplacer" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else return (menuID)
  -----------------------------------------------------------------------------------------------------------------------------------------------------

  ----------------------------------------------------- Achats Batiments --------------------------------------------------------------------------------------------------
  (envRes_combattant_collecteur, menuID) <- if (menuID  == "QG_Raffinerie") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "QG_Raffinerie" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        if (prop_pre_set_raffinerie envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs)) &&
          (prop_post_set_raffinerie envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs)) then do
            let resEnv = (set_raffinerie envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs))
            return (resEnv, "Menu_Default")
        else
          return (envRes_combattant_collecteur, "Menu_Default")
      else return (envRes_combattant_collecteur, menuID)
    else if (menuID  == "QG_Usine") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "QG_Usine" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        if (prop_pre_set_usine envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs)) &&
          (prop_post_set_usine envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs)) then do
            let resEnv = (set_usine envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs))
            return (resEnv, "Menu_Default")
        else return (envRes_combattant_collecteur, "Menu_Default")
      else return (envRes_combattant_collecteur, menuID)
    else if (menuID  == "QG_Centrale") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "QG_Centrale" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        if (prop_pre_set_centrale envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs)) &&
          (prop_post_set_centrale envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs)) then do
            let resEnv = (set_centrale envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs))
            return (resEnv, "Menu_Default")
        else return (envRes_combattant_collecteur, "Menu_Default")
      else return (envRes_combattant_collecteur, menuID)
    else return (envRes_combattant_collecteur, menuID)

  -- TODO voir pour utiliser un Either listeCollecteurs listeCombattants
  (envRes_combattant_collecteur, listeCollecteurs, menuID) <- if (menuID  == "Usine_Collecteur") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "Usine_Collecteur" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        if (is_my_object envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (May.fromJust joueurIdCourant)) then do
          if (prop_pre_set_collecteur envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs) listeCollecteurs) &&
            (prop_post_set_collecteur envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs) listeCollecteurs) then do
              let (resEnv, resListeCollecteurs) = (set_collecteur envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs) listeCollecteurs)
              putStrLn $ "Achat Collecteur"
              return (resEnv, resListeCollecteurs, "Menu_Default")
          else do 
            putStrLn $ "Achat Collecteur post/pre pb"
            return (envRes_combattant_collecteur, listeCollecteurs, "Menu_Default")
        else return (envRes_combattant_collecteur, listeCollecteurs, "Menu_Default")
      else return (envRes_combattant_collecteur, listeCollecteurs, menuID)
    else return (envRes_combattant_collecteur, listeCollecteurs, menuID)


  (envRes_combattant_collecteur, listeCombattants, menuID) <- if (menuID  == "Usine_Combattant") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "Usine_Combattant" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        if (is_my_object envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (May.fromJust joueurIdCourant)) then do
          if (prop_pre_set_combattant envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs) listeCombattants) &&
            (prop_post_set_combattant envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs) listeCombattants) then do
              let (resEnv, resListeCombattants) = (set_combattant envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (getJoueurByJoueurID (May.fromJust joueurIdCourant) joueurs) listeCombattants)
              putStrLn $ "Achat Combattant"
              return (resEnv, resListeCombattants, "Menu_Default")
          else do 
            putStrLn $ "Achat Combattant post/pre pb"
            return (envRes_combattant_collecteur, listeCombattants, "Menu_Default")
        else return (envRes_combattant_collecteur, listeCombattants, "Menu_Default")
      else return (envRes_combattant_collecteur, listeCombattants, menuID)
    else return (envRes_combattant_collecteur, listeCombattants, menuID)

  let (Environnement _ _ unitesApres batimentsApres) = envRes_combattant_collecteur -- nouveau
  let bats = (M.toList batimentsApres)
  let units = (M.toList unitesApres)

  let tmap2 = tmap
      smap2 = smap

  (tmap2', smap2') <- if (length bats /= length batiments)
                    then load_batiments renderer tmap smap [bats !! ((length bats) - 1)] 0
                    else if (length units /= length unites) then do
                      putStrLn $ "ajoute" <> show (units !! ((length units) - 1))
                      load_unites renderer tmap smap [units !! ((length units) - 1)] 0 -- voir pour last
                    else return (tmap2, smap2)

  -----------------------------------------------------------------------------------------------------------------------------------------------------
  --------------------------------------------------------- Ordres Unites------------------------------------------------------------------------------
  -- action deplacer pour collecteur
  (listeCollecteurs, menuID) <- if (menuID  == "Collecteur_Move") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "Collecteur_Move" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        putStrLn $ "Set Collecteur Deplacer dans res3"
        if (isJust uniteStocke) then do
          putStrLn $ "Set Collecteur Deplacer"
          return ((set_ordre_deplacer_collecteur (get_Collecteur_from_list (May.fromJust uniteStocke) listeCollecteurs) listeCollecteurs (C (x `div` 50) (y `div` 50))), "Menu_Default")
        else return (listeCollecteurs, menuID)
      else return (listeCollecteurs, menuID)
    else return (listeCollecteurs, menuID)

--action collecter pour collecteur
  (listeCollecteurs, menuID) <- if (menuID  == "Collecteur_Collect") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "Collecteur_Collect" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        putStrLn $ "Set Collecteur Collecter dans res3"
        if (isJust uniteStocke) then do
          putStrLn $ "Set Collecteur Collecter"
          return ((set_ordre_collect_collecteur (get_Collecteur_from_list (May.fromJust uniteStocke) listeCollecteurs) listeCollecteurs (C (x `div` 50) (y `div` 50))), "Menu_Default")
        else return (listeCollecteurs, menuID)
      else return (listeCollecteurs, menuID)
    else return (listeCollecteurs, menuID)

-- action deplacer pour combattant
  (listeCombattants, menuID) <- if (menuID  == "Combattant_Deplacer") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "Combattant_Deplacer" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        if (isJust uniteStocke) then do
          putStrLn $ "Set Combattant Deplacer"
          return ((set_ordre_deplacer_combattant (get_Combattant_from_list (May.fromJust uniteStocke) listeCombattants) listeCombattants (C (x `div` 50) (y `div` 50))), "Menu_Default")
        else return (listeCombattants, menuID)
      else return (listeCombattants, menuID)
    else return (listeCombattants, menuID)

-- |action patrouiller
  (listeCombattants, menuID) <- if (menuID  == "Combattant_Patrouiller") then do
      let res3 = fmap (\event -> processMouseEventAchat x y "Combattant_Patrouiller" event) mouseButtonEvents
      if (((length res3) > 0) && ((head res3) == "Carte")) then do
        if (isJust uniteStocke) then do
          putStrLn $ "Set Combattant Patrouiller"
          return ((set_ordre_pattrouiller_combattant (get_Combattant_from_list (May.fromJust uniteStocke) listeCombattants) listeCombattants (C (x `div` 50) (y `div` 50))), "Menu_Default")
        else return (listeCombattants, menuID)
      else return (listeCombattants, menuID)
    else return (listeCombattants, menuID)

  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime

  -- location de la souris
  -- test <- MO.getAbsoluteMouseLocation
  -- let SDL.P (SDL.V2 x y) = test
  -- -- putStrLn $ "Location mouse: " <> (show x) <> " " <> (show y)

  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState_perso1 kbd' deltaTime
  ---
  -- let (M.GameState pX pY sp) = gameState'
  -- if (pX > x+100) && (pX < x-100) then putStrLn $ "toucheee" else putStrLn $ "non"
  -- putStrLn $ "AVANT ETAPE"
    -- if MO.getAbsoluteMouseLocation
  let (newEnv@(Environnement _ _ unitess _), newListCollecteurs, newListCombattants) = etape envRes_combattant_collecteur listeCollecteurs listeCombattants
  
  -- if (length newListCombattants == 2) then error ("ah ouais: " <> show newListCombattants) else pure ()

-- |Change the sprite pour unite
  (tmap2', smap2') <- if (listeCombattants /= newListCombattants)
                  then do
                    return (gerer_combattants_ordres (newListCombattants List.\\ listeCombattants) tmap2' smap2' 0)
                  else return (tmap2', smap2')


  (tmap2', smap2') <- if (listeCollecteurs /= newListCollecteurs)
                then do
                  return (gerer_collecteurs_ordres (newListCollecteurs List.\\ listeCollecteurs) tmap2' smap2' 0)
                else return (tmap2', smap2')

  -- Changement d'un Terrain Ressource en Herbe si plus de ressources dedans
  (tmap2', smap2') <- if (get_carte newEnv /= mapp)
                then do
                  let newEnvCarteTuple = M.toList (carte (get_carte newEnv))
                      mappCarteTuple = M.toList (carte mapp)
                      changement = newEnvCarteTuple List.\\ mappCarteTuple
                      herbe = List.filter (\((C x y), terrain) ->
                          case terrain of
                            Herbe -> True
                            otherwise -> False
                        ) changement

                  if (length herbe > 0) then do
                    let ((C xx yy), _) = head herbe
                        res = M.delete (TextureId ("" ++ show xx ++ " " ++ show yy)) tmap2'
                    tmapRes <- TM.loadTexture renderer "assets/herbe.bmp" (TextureId ("" ++ show xx ++ " " ++ show yy)) res
                    return (tmapRes, smap2')
                  else return (tmap2', smap2')
                else return (tmap2', smap2')

  -- (tmap2', smap2') <- if (length units /= length unites)
  --                 then do
  --                   putStrLn $ "ajoute" <> show ((M.toList unitess) !! ((length (M.toList unitess)) - 1))
  --                   load_unites renderer tmap2' smap2' [(M.toList unitess) !! ((length (M.toList unitess)) - 1)] 0
  --                 else return (tmap2', smap2')

  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap2' smap2' kbd' [gameState', gameState_perso2] menuID newEnv newListCollecteurs newListCombattants joueurIdCourant uniteStocke)


