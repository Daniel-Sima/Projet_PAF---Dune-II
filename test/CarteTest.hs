module CarteTest (engineSpec) where

import Data.Set (Set)
import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib 


import Test.Hspec 

import Data.Maybe as May

carteEx :: Carte
carteEx = Carte (M.fromList [ (C 0 0, Herbe)
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
                   , (C 2 2, Ressource (4))
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
                   , (C 4 4, Herbe)])
  
carteFaux :: Carte
carteFaux = Carte (M.fromList [ (C 0 0, Herbe)
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
                   , (C 2 2, Ressource (4))
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
                   , (C 4 4, Herbe)
                   , (C 5 5, Herbe)])


prop_inv_TerrainSpec = do
   describe "prop_inv_Terrain" $ do

    it "returns true because is Herbe" $ do
      prop_inv_Terrain (May.fromJust (M.lookup (C 0 0) (carte carteEx) ))
        `shouldBe` True

    it "returns true because the Ressource is positive" $ do
      prop_inv_Terrain ( May.fromJust (M.lookup (C 4 2) (carte carteEx)))
        `shouldBe` True
    
    it "returns false because the Ressource is negative " $ do
      prop_inv_Terrain ( May.fromJust (M.lookup (C 2 2) (carte (Carte (M.fromList [(C 2 2, Ressource (-4))])) )))
        `shouldBe` False
  
prop_inv_CarteSpec = do
   describe "prop_inv_Carte" $ do

    it "returns true because there is always a way to arrive to others cases" $ do
      prop_inv_Carte carteEx
        `shouldBe` True
    
    it "returns false because there is no way to arrive (C 5 5)" $ do
      prop_inv_Carte carteFaux
        `shouldBe` False

prop_pre_collecteCaseSpec = do 
  describe "prop_pre_collecteCase" $ do
    
    it "returns true because (C 0 0, Herbe) exists in carteEx and r=1 is positive" $ do
      prop_pre_collecteCase (C 0 0) 1 carteEx
        `shouldBe` True
    
    it "returns true because (C 4 2, Ressource 2) exists in carteEx and r=1 is positive" $ do
      prop_pre_collecteCase (C 4 2) 1 carteEx
        `shouldBe` True
    
    it "returns false because (C 5 5) doesn't exists in carteEx" $ do
      prop_pre_collecteCase (C 5 5) 1 carteEx
        `shouldBe` False
    
    it "returns false because r=-4 is negative" $ do
      prop_pre_collecteCase (C 4 2) (-4) carteEx
        `shouldBe` False

      

collecteCaseSpec = do
  describe "collecteCase" $ do
    
    it "returns the new Carte with (C 1 4, Ressource 2) because r=6 and before we had (C 1 4, Ressource 8)" $ do
      collecteCase (C 1 4) 6 carteEx
        `shouldBe` (6, Carte (M.fromList [ (C 0 0, Herbe)
                                          , (C 0 1, Eau)
                                          , (C 0 2, Herbe)
                                          , (C 0 3, Herbe)
                                          , (C 0 4, Herbe)
                                          , (C 1 0, Eau)
                                          , (C 1 1, Herbe)
                                          , (C 1 2, Ressource 9)
                                          , (C 1 3, Herbe)
                                          , (C 1 4, Ressource 2)
                                          , (C 2 0, Herbe)
                                          , (C 2 1, Herbe)
                                          , (C 2 2, Ressource (4))
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
                                          , (C 4 4, Herbe)])
                      )
                      
    it "returns the new Carte with (C 4 2, Herbe) because r=2 and before we had (C 4 2, Ressource 2)" $ do
      collecteCase (C 4 2) 2 carteEx
        `shouldBe` (2, Carte (M.fromList [ (C 0 0, Herbe)
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
                                          , (C 2 2, Ressource (4))
                                          , (C 2 3, Herbe)
                                          , (C 2 4, Eau)
                                          , (C 3 0, Herbe)
                                          , (C 3 1, Herbe)
                                          , (C 3 2, Herbe)
                                          , (C 3 3, Eau)
                                          , (C 3 4, Herbe)
                                          , (C 4 0, Eau)
                                          , (C 4 1, Herbe)
                                          , (C 4 2, Herbe)
                                          , (C 4 3, Herbe)
                                          , (C 4 4, Herbe)])
                      )
                      
    it "returns the quantity of ressource the terrain has and the new Carte that the position we pick becomes Herbe" $ do
      collecteCase (C 1 4) 9 carteEx
        `shouldBe` (8, Carte (M.fromList [ (C 0 0, Herbe)
                                          , (C 0 1, Eau)
                                          , (C 0 2, Herbe)
                                          , (C 0 3, Herbe)
                                          , (C 0 4, Herbe)
                                          , (C 1 0, Eau)
                                          , (C 1 1, Herbe)
                                          , (C 1 2, Ressource 9)
                                          , (C 1 3, Herbe)
                                          , (C 1 4, Herbe)
                                          , (C 2 0, Herbe)
                                          , (C 2 1, Herbe)
                                          , (C 2 2, Ressource (4))
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
                                          , (C 4 4, Herbe)])
                      )
                      
    it "returns 0 and the Carte because we try to collect ressource in grass" $ do
      collecteCase (C 1 3) 9 carteEx
        `shouldBe` (0, carteEx)
    


prop_post_collecteCaseSpec = do
   describe "prop_post_collecteCase" $ do
    
    it "returns True because the new Carte have (C 1 4, Ressource 2) instead of (C 1 4, Ressource 8) because r=6 AND others cases didn't change" $ do
      prop_post_collecteCase (C 1 4) 6 carteEx
      `shouldBe` True
    
    it "returns True because the new Carte have (C 4 2, Herbe) instead of (C 4 2, Ressource 2) because r=2 AND others cases didn't change" $ do
      prop_post_collecteCase (C 4 2) 2 carteEx
      `shouldBe` True
    
    it "returns True because the quantity of ressource the terrain has and the new Carte that the position we pick becomes Herbe AND others cases didn't change" $ do
      prop_post_collecteCase (C 1 4) 9 carteEx
      `shouldBe` True
      
    it "returns True because the we try to pick ressourse in grass and the Carte after picked is still the same" $ do
      prop_post_collecteCase (C 1 3) 9 carteEx
        `shouldBe` True

   
engineSpec = do 
    prop_inv_TerrainSpec
    prop_inv_CarteSpec
    prop_pre_collecteCaseSpec
    collecteCaseSpec
    prop_post_collecteCaseSpec
