
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import SDL.Input.Mouse
import qualified SDL.Input.Mouse as MO

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)

-- data MousePosition = MousePosition Int Int
--   deriving (Eq, Show)


initGameState :: GameState
initGameState = GameState 200 300 4

moveLeft :: GameState -> GameState
moveLeft gs@(GameState pX pY sp) = if (pX - sp > 0) then gs{persoX = pX - sp} else gs

moveRight :: GameState -> GameState
moveRight gs@(GameState pX pY sp) = if (pX + sp + 100 < 640) then gs{persoX = pX + sp} else gs
  
moveUp :: GameState -> GameState
moveUp gs@(GameState pX pY sp) = if (pY - sp > 0) then gs{persoY = pY - sp} else gs

moveDown :: GameState -> GameState
moveDown gs@(GameState pX pY sp) = if (pY + sp + 100 < 480) then gs{persoY = pY + sp} else gs

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)
              

  in modif gstate 

-- compareMousePerson :: MousePosition -> GameState -> Bool
-- compareMousePerson (SDL.P (SDL.V2 x y)) gs@(GameState pX pY sp) = px <= x && x <= (px + 100) && py <= y && y <= (py + 100)

-- handleEventMouse :: Event -> GameState -> Bool
-- handleEventMouse event gs@(GameState pX pY _) =
--   case eventPayload event of
--     KeyboardEvent keyboardEvent -> (if keyboardEventKeyMotion keyboardEvent == Pressed
--       then ( let (SDL.P (SDL.V2 x y) ) =  mouseMotionEventPos in
--               x == pX && y == pY 
--       ) else False )  
--     _ -> False
