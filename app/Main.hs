{-# LANGUAGE OverloadedStrings #-}
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


generateCarte :: Carte
generateCarte = Carte $ M.fromList [((C x y), getTerrain x y) | x <- [0..34], y <- [0..16]]
  where
    getTerrain x y
      | x == 17 && y == 8 = Eau -- milieu de la carte
      | x < 5 && y < 5 = Ressource 10 -- coins
      | x > 29 && y < 5 = Ressource 10
      | x < 5 && y > 11 = Ressource 10
      | x > 29 && y > 11 = Ressource 10
      | otherwise = Herbe

genCarte :: Carte
genCarte = Carte $ M.fromList [((C i j), getCase i j) | i <- [0..34], j <- [0..16]]
  where
    getCase i j
      | r < 0.2 = Ressource 10
      | r < 0.3 = Eau
      | otherwise = Herbe
      where
        r = fromIntegral (i * 34 + j) / fromIntegral (34 * 16)

-- generateCarteR :: Carte
-- generateCarteR = Carte $ M.fromList [((C x y), getTerrain x y) | x <- [0..34], y <- [0..16]]
--   where
--     getTerrain x y
--       | x `elem` [16, 17, 18, 20, 21, 22, 23, 14] && y `elem` [5, 6, 7, 8, 9, 10, 11] = Eau -- centre
--       | x `elem` [0, 0, 1, 1, 1, 2, 2, 1 34, 34] && y `elem` [0, 16, 0, 16] = Ressource 10 -- coins
--       | otherwise = Herbe


player1 :: Joueur
player1 = Joueur "0" (JoueurId 0) 100

player2 :: Joueur
player2 = Joueur "1" (JoueurId 1) 100

player3 :: Joueur
player3 = Joueur "2" (JoueurId 2) 100

uniteCombattant :: Unite 
uniteCombattant = Unite "Combattant" (C 4 4) (JoueurId 0) 

uniteEtudiant :: Unite
uniteEtudiant = Unite "Collecteur" (C 0 3) (JoueurId 1) 

cuve_valide_pleine :: Cuve
cuve_valide_pleine = CuvePleine 10
 

combattant_valide :: Combattant
combattant_valide = Combattant (UniteId 0) uniteCombattant 1 [Deplacer (C 4 3)] (Deplacer (C 1 4))

collecteur_valide :: Collecteur
collecteur_valide = Collecteur (UniteId 1) uniteEtudiant cuve_valide_pleine 3 [Collecter (C 4 4)] (Deplacer (C 4 2)) 

envRes_combattant_collecteur :: Environnement
envRes_combattant_collecteur = Environnement [player1, player2, player3] genCarte (M.fromList [((UniteId 2), (get_unite_Combattant combattant_valide)), ((UniteId 1), (get_unite_Collecteur collecteur_valide))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

qr1 :: Batiment
qr1 = Batiment "QG" 0 (C 0 2) (JoueurId 0)

qr2 :: Batiment
qr2 = Batiment "QG" 0 (C 3 0) (JoueurId 1)

qr3 :: Batiment
qr3 = Batiment "QG" 0 (C 1 3) (JoueurId 2)

u1 :: Batiment
u1 = Batiment "Usine" 0 (C 4 4) (JoueurId 0)

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
            "QG" -> do
                tmap' <- TM.loadTexture rdr "assets/Usine.bmp" (TextureId textureSpriteID) tmap 
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
--------------------------------------------------------------------------------------------------------------------------------------------------------------

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

-- | Traitement d'un clic de souris
processMouseEvent :: Int -> Int -> Int -> Int -> SDL.Event -> IO ()
processMouseEvent xMouse yMouse xPerso yPerso (SDL.Event _ (SDL.MouseButtonEvent eventData)) =
  if ((xMouse >= xPerso) && (xMouse <= xPerso+100)) && ((yMouse >= yPerso) && (yMouse <= yPerso+100)) then 
    putStrLn $ "Touche perso avec" ++ show (SDL.mouseButtonEventButton eventData)  
  else putStrLn $ "PAS touche"
  -- putStrLn $ "Mouse button " ++ show (SDL.mouseButtonEventButton eventData) ++ " pressed || X mouse test: " <> (show x) <> " Y mouse: " <> (show y)
processMouseEvent _ _ _ _ _ = return ()

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1750 850)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

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

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 1740 880 }
  renderer <- createRenderer window (-1) defaultRenderer
  
  let (Environnement joueurs mapp unites batiments) = envRes_combattant_collecteur



--   -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/herbe.bmp" TM.createTextureMap SM.createSpriteMap
  (tmap2, smap2) <- load_carte renderer tmap smap (M.toList (carte genCarte)) 0
  (tmap3, smap3) <- load_batiments renderer tmap2 smap2 (M.toList batiments) 0
  (tmap4, smap4) <- load_unites renderer tmap3 smap3 (M.toList unites) 0
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
  gameLoop 60 renderer tmap4 smap4 kbd [gameState_perso1, gameState_perso2] 1 envRes_combattant_collecteur

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> [GameState] -> Int -> Environnement -> IO ()
gameLoop frameRate renderer tmap smap kbd [gameState_perso1, gameState_perso2] affichage envRes_combattant_collecteur = do
  startTime <- time
  --- ensemble des events
  events <- pollEvents
  --- events du clavier
  let kbd' = K.handleEvents events kbd
  --- events souris
  let mouseButtonEvents = filter isMouseButtonEvent events

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
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap) -- peut etre a changer 
  display_carte (M.toList (carte genCarte)) renderer tmap smap 0
  display_batiments (M.toList batiments) renderer tmap smap 0
  display_unite (M.toList unites) renderer tmap smap 0

  --- display perso 1
--   S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap) (fromIntegral (M.persoX gameState_perso1)) (fromIntegral (M.persoY gameState_perso1)))
  --- display perso 2
--   S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "virus") smap) (fromIntegral (M.persoX gameState_perso2)) (fromIntegral (M.persoY gameState_perso2)))

  --- location de la souris
  test <- MO.getAbsoluteMouseLocation
  let SDL.P (SDL.V2 x1 y1) = test
  let mousePos = SDL.P (SDL.V2 x1 y1) 
      x = case mousePos of
            SDL.P (SDL.V2 x _) -> fromIntegral x
      y = case mousePos of
            SDL.P (SDL.V2 _ y) -> fromIntegral y
 
  mapM_ (\event -> processMouseEvent x y (M.persoX gameState_perso1) (M.persoY gameState_perso1) event) mouseButtonEvents
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
  
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' [gameState', gameState_perso2] affichage envRes_combattant_collecteur)
