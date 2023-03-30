module CarteTest where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib 


import Test.Hspec -- a voir

import Data.Maybe as May

carte = M.fromList [ (C 0 0, Herbe)
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
                   , (C 2 2, Ressource (-4))
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
                   , (C 4 4, Herbe)]


prop_inv_TerrainSpec = do
   describe "prop_inv_Terrain" $ do

    it "returns all secret pegs unmarked" $ do
      prop_inv_Terrain (May.fromJust (M.lookup (C 0 0) carte))
        `shouldBe` True

    it "returns all secret pegs unmarked" $ do
      prop_inv_Terrain ( May.fromJust (M.lookup (C 4 2) carte))
        `shouldBe` True
    
    it "returns all secret pegs unmarked" $ do
      prop_inv_Terrain ( May.fromJust (M.lookup (C 2 2) carte))
        `shouldBe` False
  
    
   
engineSpec = do 
    prop_inv_TerrainSpec
