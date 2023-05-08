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

import qualified Data.List as List

import SDL.Font 
import qualified SDL.Font as Font
import SDL.Video

import qualified Data.Text as DT


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

-- genCarte :: Carte
-- genCarte = Carte $ M.fromList [((C i j), getCase i j) | i <- [0..34], j <- [0..16]]
--   where
--     getCase i j
--       | r < 40 = let r = ((head (randomNb 42)) `mod` 100) in Ressource 10
--       | r < 50 = let r = ((head (randomNb 42)) `mod` 100) in Herbe
--       | otherwise = Eau
--       where r = ((head (randomNb 42)) `mod` 100)
        
genCarte :: Integer -> Carte
genCarte nb = Carte $ M.fromList [((C x y), getTerrain x y) | x <- [0..30], y <- [0..16]]
  where
    getTerrain x y
      | r < 0.2 = Ressource 10
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
                tmap' <- TM.loadTexture rdr "assets/QG.bmp" (TextureId textureSpriteID) tmap 
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase)) 
                    smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap 
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            "Raffinerie" ->  do
                tmap' <- TM.loadTexture rdr "assets/Raffinerie.bmp" (TextureId textureSpriteID) tmap 
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase)) 
                    smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap 
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            "Usine" -> do
                tmap' <- TM.loadTexture rdr "assets/Usine.bmp" (TextureId textureSpriteID) tmap 
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase)) 
                    smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap 
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            "Centrale" -> do
                tmap' <- TM.loadTexture rdr "assets/Centrale.bmp" (TextureId textureSpriteID) tmap 
                let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase)) 
                    smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap 
                load_batiments rdr tmap' smap' listeBats (cpt+1)
            otherwise -> do 
                return (tmap, smap)

display_batiments :: [(BatId, Batiment)] -> Renderer -> TextureMap -> SpriteMap -> Int -> IO ()
display_batiments listeBats rdr tmap smap cpt 
    | (cpt == length listeBats) = return ()
    | otherwise = do 
        let (BatId batId) = fst (listeBats !! cpt)
            (Batiment bNom _ batCoord _) = snd (listeBats !! cpt)
            (C x y) = batCoord
            textureSpriteID = "B" ++ (show batId) 
        if (length listeBats == 8)&&(textureSpriteID == "B7") then putStrLn $ ("ici 8 " ++ bNom ++ " " ++ show x ++ " " ++ show y) else return ()
        S.displaySprite rdr tmap (SM.fetchSprite (SpriteId textureSpriteID) smap)
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
              tmap' <- TM.loadTexture rdr "assets/Collector_face.bmp" (TextureId textureSpriteID) tmap 
              let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase)) 
                  smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap 
              load_unites rdr tmap' smap' listUnite (cpt+1)
          "Combattant" -> do
              tmap' <- TM.loadTexture rdr "assets/Soldat_face.bmp" (TextureId textureSpriteID) tmap 
              let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId textureSpriteID) (S.mkArea 0 0 (fromInteger tailleCase) (fromInteger tailleCase)) 
                  smap' = SM.addSprite (SpriteId textureSpriteID) (S.moveTo sprite ((fromIntegral x)*(fromInteger tailleCase)) ((fromIntegral y)*(fromInteger tailleCase))) smap 
              load_unites rdr tmap' smap' listUnite (cpt+1)
          otherwise -> do
              return (tmap, smap)

display_unite :: [(UniteId, Unite)]  -> Renderer -> TextureMap -> SpriteMap -> Int -> IO ()
display_unite listeU rdr tmap smap cpt 
    | (cpt == length listeU) = return ()
    | otherwise = do 
        let (Unite uNom uCoord _ ) = snd (listeU !! cpt)
            (UniteId id) = fst (listeU !! cpt)
            (C x y) = uCoord
            textureSpriteID = "U" ++ show id
        S.displaySprite rdr tmap (SM.fetchSprite (SpriteId textureSpriteID) smap)
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

processMouseEventBatiments :: Int -> Int -> [(BatId, Batiment)] -> SDL.Event -> Int -> String
processMouseEventBatiments xMouse yMouse listeB e@(SDL.Event _ (SDL.MouseButtonEvent eventData)) cpt 
  | (cpt == length listeB) = if (xMouse >= (1740-190)) && (yMouse <= 200) then "" else "Menu_Default"
  | otherwise = 
    let (Batiment bNom _ batCoord _) = snd (listeB !! cpt)
        (C x y) = batCoord
        (BatId batId) = fst (listeB !! cpt) in 
    if ((xMouse >= x*50) && (xMouse <= x*50 + (fromInteger tailleCase))) && ((yMouse >= y*50) && (yMouse <= y*50+(fromInteger tailleCase))) then 
      if (bNom == "QG") then "QG_Menu" 
      else if (bNom == "Usine") then "Usine_Menu"
      else if (bNom == "Raffinerie") then "Raffinerie_Menu"
      else if (bNom == "Centrale") then "Centrale_Menu"
      else "Error Menu"
    else 
      (processMouseEventBatiments xMouse yMouse listeB e (cpt+1))

-- | Traitement d'un clic de souris
processMouseEvent :: Int -> Int -> [(UniteId, Unite)] -> [(BatId, Batiment)] -> SDL.Event -> Int -> String
processMouseEvent xMouse yMouse listeU listB e@(SDL.Event _ (SDL.MouseButtonEvent eventData)) cpt 
  | (cpt == length listeU) = 
      processMouseEventBatiments xMouse yMouse listB e 0
  | otherwise = 
      let (Unite uNom uCoord _ ) = snd (listeU !! cpt)
          (UniteId id) = fst (listeU !! cpt)
          (C x y) = uCoord in 
      if ((xMouse >= x*50) && (xMouse <= x*50 + (fromInteger tailleCase))) && ((yMouse >= y*50) && (yMouse <= y*50+(fromInteger tailleCase))) then 
        if (uNom == "Combattant") then "Combattant_Menu"
        else if (uNom == "Collecteur") then "Collecteur_Menu"
        else "Error Menu"
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
    otherwise -> menuID


processMouseEventAchatBat :: Int -> Int -> String -> SDL.Event -> String
processMouseEventAchatBat xMouse _ menuID e@(SDL.Event _ (SDL.MouseButtonEvent eventData)) = 
  case menuID of 
    "QG_Raffinerie" ->  
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

drawWhiteRect :: SDL.Renderer -> IO ()
drawWhiteRect renderer = do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 173 140 118 255 
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 1550 200) (SDL.V2 190 650))


-----------------------------------------------------------------------------------------------------------
-------------------------------------------------- Main ---------------------------------------------------
-----------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Dune II" $ defaultWindow { windowInitialSize = V2 1740 850 }
  renderer <- createRenderer window (-1) defaultRenderer

 
  let carteEnv = genCarte 1
  let environnement = envSmart carteEnv [(C 10 5)]
  if (prop_inv_Environnement environnement) then return ()
  else error "Mauvaise generation de carte"

  let (Environnement joueurs mapp unites batiments) = environnement



--   -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/herbe.bmp" TM.createTextureMap SM.createSpriteMap
  -- TODO les stocker dans une liste puis les passer par liste a 'load_menu'
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


--   (tmap1, smap1) <- load 100 0 "CaseRessource" renderer "assets/ressource.bmp" tmap smap
--   (tmap2, smap2) <- load 0 0 "CaseEau" renderer "assets/eau.bmp" tmap1 smap1
--   -- chargement du personnage 1
--   (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
--    -- chargement du personnage 2
--   (tmap2, smap2) <- loadPerso2 renderer "assets/virus.bmp" tmap' smap'
  -- initialisation de l'état du jeu
  let gameState_perso1 = M.initGameState
  virusX <- randomRIO (0, 640-100) :: IO Int
  virusY <- randomRIO (0, 480-100) :: IO Int
  let gameState_perso2 = M.GameState virusX virusY 0
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
 
  Font.initialize
  
  
  gameLoop 60 renderer tmap15 smap15 kbd [gameState_perso1, gameState_perso2] "Menu_Default" environnement

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> [GameState] -> String -> Environnement -> IO ()
gameLoop frameRate renderer tmap smap kbd [gameState_perso1, gameState_perso2] menuID envRes_combattant_collecteur = do
  startTime <- time
  --- ensemble des events
  events <- pollEvents
  --- events du clavier
  let kbd' = K.handleEvents events kbd
  --- events souris
  let mouseButtonEvents = filter isMouseButtonEventPressed events

  clear renderer


  let isCollision = ((fromIntegral (M.persoX gameState_perso1)) + 50 >= (fromIntegral (M.persoX gameState_perso2)) &&
                   (fromIntegral (M.persoX gameState_perso1)) + 50 <= (fromIntegral (M.persoX gameState_perso2)+100) &&
                   (fromIntegral (M.persoY gameState_perso1)) + 50 >= (fromIntegral (M.persoY gameState_perso2)) &&
                   (fromIntegral (M.persoY gameState_perso1)) + 50 <= (fromIntegral (M.persoY gameState_perso2)+100))

  gameState_perso2 <- if isCollision
            then do
              putStrLn "Collision detected, reomving virus."
              return $ M.GameState (-300) (-300) 0
            else return gameState_perso2

  -- when (((fromIntegral (M.persoX gameState_perso1)) + 50 >= (fromIntegral (M.persoX gameState_perso2)) && (fromIntegral (M.persoX gameState_perso1)) + 50 <= (fromIntegral (M.persoX gameState_perso2)+100))  &&
  --   ((fromIntegral (M.persoY gameState_perso1)) + 50 >= (fromIntegral (M.persoY gameState_perso2)) && (fromIntegral (M.persoY gameState_perso1)) + 50 <= (fromIntegral (M.persoY gameState_perso2)+100))) 
  --   $ do (SM.removeSprite (SpriteId "virus") smap)

  let (Environnement joueurs mapp unites batiments) = envRes_combattant_collecteur
   

 

  ---- affichage credits
  drawWhiteRect renderer
  font <- Font.load "arial.ttf" 24
  white <- pure $ V4 255 255 255 255 -- blanc
  surface <- Font.solid font white (DT.pack ("Credits: " <> (show (get_player_credit (head joueurs))))) 
  surfaceTexture <- createTextureFromSurface renderer surface
  copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+25) 250) (V2 100 50))
  if (menuID  == "QG_Raffinerie") then do
    surface <- Font.solid font white (DT.pack ("Prix raffinerie: " <> (show prixRaffinerie))) 
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+25) 350) (V2 100 50))
    pure ()
  else if (menuID  == "QG_Usine") then do 
    surface <- Font.solid font white (DT.pack ("Prix usine: " <> (show prixUsine))) 
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+25) 350) (V2 100 50))
    pure ()
  else if (menuID  == "QG_Centrale") then do 
    surface <- Font.solid font white (DT.pack ("Prix centrale: " <> (show prixCentrale))) 
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+25) 350) (V2 100 50))
    pure ()
  else if (menuID  == "Usine_Combattant") then do 
    surface <- Font.solid font white (DT.pack ("Prix combattant: " <> (show prixCombattant))) 
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+25) 350) (V2 150 50))
    pure ()
  else if (menuID  == "Usine_Collecteur") then do 
    surface <- Font.solid font white (DT.pack ("Prix collecteur: " <> (show prixCollecteur))) 
    surfaceTexture <- createTextureFromSurface renderer surface
    copy renderer surfaceTexture Nothing (Just $ SDL.Rectangle (P $ V2 (1740-190+25) 350) (V2 150 50))
    pure ()
  else pure ()

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
 
  -- menuID <- if ((menuID == "Menu_Default") || (menuID == "Raffinerie_Menu") || (menuID == "Centrale_Menu")) then return menuID
  --   else do 
  --     case menuID of 
  --       "QG_Menu" -> do 
  --         if (((x >= (1740-190+25)) && (x <= (1740-190+75))) && ((y >= 55) && (y <= 80))) then return "QG_Raffinerie" else return menuID
  --       otherwise -> return menuID 



  let res = fmap (\event -> processMouseEvent x y (M.toList unites) (M.toList batiments) event 0) mouseButtonEvents
  menuID <- if (((length res) > 0) && ((head res) /= "") && (menuID /= "QG_Raffinerie")) then do 
    putStrLn $ (head res) 
    return (head res)
    else do return (menuID)

  menuID <- if ((menuID == "Menu_Default") || (menuID == "Raffinerie_Menu") || (menuID == "Centrale_Menu")) then return menuID
    else do 
      if (menuID  == "QG_Menu") then do
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
        let res2 = fmap (\event -> processMouseEventMenus x y "Usine_Menu" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Usine_Collecteur") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Usine_Collecteur" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else if (menuID  == "Usine_Combattant") then do
        let res2 = fmap (\event -> processMouseEventMenus x y "Usine_Combattant" event) mouseButtonEvents
        if (((length res2) > 0) && ((head res2) /= "")) then return (head res2) else return menuID
      else return (menuID)
  

  --- Achats Batiments
  envRes_combattant_collecteur <- if (menuID  == "QG_Raffinerie") then do
      let res3 = fmap (\event -> processMouseEventAchatBat x y "QG_Raffinerie" event) mouseButtonEvents 
      if (((length res3) > 0) && (head res3) == "Carte") then do 
        if (prop_pre_set_raffinerie envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (head joueurs)) then do 
          let resEnv = (set_raffinerie envRes_combattant_collecteur (C (x `div` 50) (y `div` 50)) (head joueurs)) 
          return resEnv
        else return envRes_combattant_collecteur
      else return envRes_combattant_collecteur
    else return envRes_combattant_collecteur

  let (Environnement _ _ _ batimentsApres) = envRes_combattant_collecteur 
  let bats = (M.toList batimentsApres)

  menuID <- if (length bats /= length batiments) then return "Menu_Default" else return menuID

  let tmap2 = tmap
      smap2 = smap

  -- when (length bats /= length batiments) $ do
  --   (tmap2', smap2') <- load_batiments renderer tmap smap [bats !! ((length bats) - 1)] 0
  --   putStrLn $ "ouais"
  --   S.displaySprite renderer tmap2' (SM.fetchSprite (SpriteId "B6") smap2')
  --   putStrLn $ "il est la"
  --   let tmap2 = tmap2'
  --   let smap2 = smap2'
  --   if (M.notMember (SpriteId "B6") smap2) then putStrLn $ "[IN] no" else putStrLn $ "[OUT] yes" 
  --   return ()

  (tmap2', smap2') <- if (length bats /= length batiments) 
                    then load_batiments renderer tmap smap [bats !! ((length bats) - 1)] 0
                    else return (tmap2, smap2)

  if (length bats /= length batiments) then do putStrLn $ "yes " ++ "ICI: " ++ menuID
  else return ()
    
  -- S.displaySprite renderer tmap2 (SM.fetchSprite (SpriteId "B6") smap2)
  -- (tmap2, smap2) <- if (length bats /= length batiments) then do return (load_batiments renderer tmap smap [bats !! ((length bats) - 1)] 0) else return (tmap, smap)

  -- let (tmap2, smap2) = (if (length bats /= length batiments) then do 
  --     resTmap, resSmap <- (load_batiments renderer tmap smap [bats !! ((length bats) - 1)] 0)
  --     return ress
  --   else return (tmap, smap))








      -- if (prop_pre_set_raffinerie envRes_combattant_collecteur (C x y) (JoueurId 0)) then do
      -- else return envRes_combattant_collecteur
        
        
        -- && 
          -- (prop_post_set_raffinerie envRes_combattant_collecteur (C x y) (JoueurId 0))) then do 
      --       if (prop_inv_Environnement resEnv) then return resEnv
      --       else return envRes_combattant_collecteur


  --- display perso 1
--   S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap) (fromIntegral (M.persoX gameState_perso1)) (fromIntegral (M.persoY gameState_perso1)))
  --- display perso 2
--   S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "virus") smap) (fromIntegral (M.persoX gameState_perso2)) (fromIntegral (M.persoY gameState_perso2)))

 
  ---
  -- putStrLn $ "X perso: " <> (show (M.persoX gameState))
  -- putStrLn $ "Y perso: " <> (show (M.persoY gameState))

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

    -- if MO.getAbsoluteMouseLocation
  
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap2' smap2' kbd' [gameState', gameState_perso2] menuID envRes_combattant_collecteur)
