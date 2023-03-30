module CarteTest where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib

import Test.Hspec -- a voir

let carte = M.empty
carte = M.insert (0, 0) Herbe carte
carte = M.insert (0, 1) Eau carte
carte = M.insert (0, 2) Herbe carte
carte = M.insert (0, 3) Herbe carte
carte = M.insert (0, 4) Herbe carte
carte = M.insert (1, 0) Eau carte
carte = M.insert (1, 1) Herbe carte
carte = M.insert (1, 2) Ressource 9 carte
carte = M.insert (1, 3) Herbe carte
carte = M.insert (1, 4) Ressource 8 carte
carte = M.insert (2, 0) Herbe carte
carte = M.insert (2, 1) Herbe carte
carte = M.insert (2, 2) Ressource 4 carte
carte = M.insert (2, 3) Herbe carte
carte = M.insert (2, 4) Eau carte
carte = M.insert (3, 0) Herbe carte
carte = M.insert (3, 1) Herbe carte
carte = M.insert (3, 2) Herbe carte
carte = M.insert (3, 3) Eau carte
carte = M.insert (3, 4) Herbe carte
carte = M.insert (4, 0) Eau carte
carte = M.insert (4, 1) Herbe carte
carte = M.insert (4, 2) Ressource 2 carte
carte = M.insert (4, 3) Herbe carte
carte = M.insert (4, 4) Herbe carte

prop_inv_TerrainSpec = do
  describe "prop_inv_Terrain" $ do

    it "returns all secret pegs unmarked" $ do
      prop_inv_Terrain $ fromJust (M.lookup (4,4) carte)
        `shouldBe` True
    
      
   
engineSpec = do 
    Show Carte 
