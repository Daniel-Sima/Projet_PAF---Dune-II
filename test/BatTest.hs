module BatTest (engineSpec) where

import Data.Set (Set)
import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib 

import Test.Hspec -- a voir

import Data.Maybe as May

import Control.Exception (evaluate)


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
                   , (C 4 4, Herbe)])

listCoordEx = [(C 0 2), (C 3 0), (C 1 3)]
listCoordIllegalEau = [(C 3 3), (C 3 0), (C 1 3)]
listCoordIllegalMemeCoor = [(C 3 0), (C 3 0), (C 1 3)]

player1 :: Joueur
player1 = Joueur "0" (JoueurId 0) 100

player2 :: Joueur
player2 = Joueur "1" (JoueurId 1) 100

player3 :: Joueur
player3 = Joueur "2" (JoueurId 2) 100

qr1 :: Batiment
qr1 = Batiment "QG" 0 (C 0 2) (JoueurId 0)

qr2 :: Batiment
qr2 = Batiment "QG" 0 (C 3 0) (JoueurId 1)

qr3 :: Batiment
qr3 = Batiment "QG" 0 (C 1 3) (JoueurId 2)

r1 :: Batiment
r1 = Batiment "Raffinerie" 0 (C 3 2) (JoueurId 0)

u1 :: Batiment
u1 = Batiment "Usine" 0 (C 4 4) (JoueurId 0)

c1 :: Batiment
c1 = Batiment "Centrale" 0 (C 3 4) (JoueurId 0)

envRes :: Environnement
envRes = Environnement [player1, player2, player3] carteEx (M.empty) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3)])

envResApres :: Environnement
envResApres = Environnement [player2, player3] carteEx (M.empty) (M.fromList [(BatId 1, qr2), (BatId 2, qr3)])

envRes_raffinerie :: Environnement
envRes_raffinerie = Environnement [player1, player2, player3] carteEx (M.empty) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, r1)])

envRes_usine :: Environnement
envRes_usine = Environnement [player1, player2, player3] carteEx (M.empty) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_centrale :: Environnement
envRes_centrale = Environnement [player1, player2, player3] carteEx (M.empty) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, c1)])


envSmartSpec = do
   describe "envSmart" $ do

    it "returns the new environement for carteEx and listCoordEx of players coord" $ do
        envSmart carteEx listCoordEx
            `shouldBe` envRes

----------------------------------------------------------QG test-------------------------------------------------------------------------------------------

prop_pre_destructionQG_Spec = do
   describe "prop_pre_destructionQG" $ do

    it "returns True because 'player1' is present in envRes and envRes invariant has worked" $ do
        prop_pre_destructionQG envRes player1
            `shouldBe` True
            

prop_post_destructionQG_Spec = do
   describe "prop_post_destructionQG" $ do

    it "returns True because 'player1' is no more present in envRes and the others fields haven't changed" $ do
        prop_post_destructionQG envRes player1
            `shouldBe` True


destructionQG_Spec = do
   describe "destructionQG" $ do

    it "returns the new enviroment which the player1 have been deleted" $ do
        destructionQG envRes player1
            `shouldBe` envResApres

---------------------------------------------------------test raffinerie----------------------------------------------------------------------------------------
prop_pre_set_raffinerie_Spec = do 
   describe "prop_pre_set_raffinerie" $ do
        
    it "returns False because '(C 3 3)' case is not Herbe" $ do
        prop_pre_set_raffinerie envRes (C 3 3) player1
            `shouldBe` False

    it "returns True because '(C 3 2)' case is Herbe, 'envRes' invariant has worked and 'player1' is present in 'envRes'." $ do
        prop_pre_set_raffinerie envRes (C 3 2) player1
            `shouldBe` True


set_raffinerie_Spec = do 
    describe "set_raffinerie" $ do

        it "returns the enviroment which the raffinerie has benn built" $ do
            set_raffinerie envRes (C 3 2) player1 
                `shouldBe` envRes_raffinerie

prop_post_set_raffinerie_Spec = do 
    describe "prop_post_set_raffinerie" $ do

        it "returns True because the raffinerie has been built and the other things is not changed " $ do
            prop_post_set_raffinerie envRes (C 3 2) player1
                `shouldBe` True

prop_pre_destruction_raffinerie_Spec = do
    describe "prop_pre_destruction_raffinerie" $ do

        it "returns False because the raffinerie in '(C 3 2)' doesn't belong to player2." $ do
            prop_pre_destruction_raffinerie envRes_raffinerie (C 3 2) player2
                `shouldBe` False
        
        it "returns True because the raffinerie in '(C 3 2)' exists in 'envRes_raffinerie' for 'player1'." $ do
            prop_pre_destruction_raffinerie envRes_raffinerie (C 3 2) player1
                `shouldBe` True
        
        it "returns False because the batiment in '(C 0 2)' is not an usine but an QG." $ do
            prop_pre_destruction_raffinerie envRes_raffinerie (C 0 2) player1
                `shouldBe` False
        
destruction_raffinerie_Spec = do 
    describe "destruction_raffinerie" $ do

        it "returns the enviroment which the raffinerie has been removed" $ do
            destruction_raffinerie envRes_raffinerie (C 3 2) player1 
                `shouldBe` envRes

prop_post_destruction_raffinerie_Spec = do
    describe "prop_post_destruction_raffinerie" $ do

        it "returns True because the raffinerie has been removed and the other things not changed " $ do
            prop_post_destruction_raffinerie envRes_raffinerie (C 3 2) player1
                `shouldBe` True

--------------------------------------------------- Usine tests -----------------------------------------------------------------------------------------------

prop_pre_set_usine_Spec = do 
   describe "prop_pre_set_usine" $ do
        
    it "returns False because '(C 4 2)' case is not Herbe" $ do
        prop_pre_set_usine envRes (C 4 2) player1
            `shouldBe` False

    it "returns True because '(C 4 4)' case is Herbe, 'envRes' invariant has worked and 'player1' is present in 'envRes'." $ do
        prop_pre_set_usine envRes (C 4 4) player1
            `shouldBe` True

set_usine_Spec = do 
    describe "set_usine" $ do

        it "returns the enviroment which the usine has benn built" $ do
            set_usine envRes (C 4 4) player1 
                `shouldBe` envRes_usine

prop_post_set_usine_Spec = do 
    describe "prop_post_set_usine" $ do

        it "returns True because the usine has been built and the other things is not changed " $ do
            prop_post_set_usine envRes (C 4 4) player1
                `shouldBe` True
                
prop_pre_destruction_usine_Spec = do
    describe "prop_pre_destruction_usine" $ do

        it "returns True because the usine in '(C 4 4)' exists in 'envRes_usine' for 'player1" $ do
            prop_pre_destruction_usine envRes_usine (C 4 4) player1
                `shouldBe` True
        
        it "returns False because the batiment in '(C 0 2)' is not an usine but an QG." $ do
            prop_pre_destruction_usine envRes_usine (C 0 2) player1
                `shouldBe` False

destruction_usine_Spec = do 
    describe "destruction_usine" $ do

        it "returns the enviroment which the usine has been removed" $ do
            destruction_usine envRes_usine (C 4 4) player1 
                `shouldBe` envRes

prop_post_destruction_usine_Spec = do
    describe "prop_post_destruction_usine" $ do

        it "returns True because the usine has been removed and the other things not changed " $ do
            prop_post_destruction_usine envRes_usine (C 4 4) player1
                `shouldBe` True

--------------------------------------------------- Centrale tests -----------------------------------------------------------------------------------------------

prop_pre_set_centrale_Spec = do 
   describe "prop_pre_set_centrale" $ do
        
    it "returns False because '(C 4 0)' case is not Herbe but Eau" $ do
        prop_pre_set_centrale envRes (C 4 0) player1
            `shouldBe` False

    it "returns True because '(C 3 4)' case is Herbe, 'envRes' invariant has worked and 'player1' is present in 'envRes'." $ do
        prop_pre_set_centrale envRes (C 3 4) player1
            `shouldBe` True

set_centrale_Spec = do 
    describe "set_centrale" $ do

        it "returns the enviroment which the centrale has been built" $ do
            set_centrale envRes (C 3 4) player1 
                `shouldBe` envRes_centrale

prop_post_set_centrale_Spec = do 
    describe "prop_post_set_centrale" $ do

        it "returns True because the centrale has been built and the other things is not changed " $ do
            prop_post_set_centrale envRes (C 3 4) player1
                `shouldBe` True

prop_pre_destruction_centrale_Spec = do
    describe "prop_pre_destruction_centrale" $ do

        it "returns True because the Centrale in '(C 3 4)' exists in 'envRes_centrale' for 'player1" $ do
            prop_pre_destruction_centrale envRes_centrale (C 3 4) player1
                `shouldBe` True
        
        it "returns False because the batiment in '(C 3 0)' is not an usine but an QG." $ do
            prop_pre_destruction_centrale envRes_centrale (C 3 0) player1
                `shouldBe` False

destruction_centrale_Spec = do 
    describe "destruction_centrale" $ do

        it "returns the enviroment which the centrale has been removed" $ do
            destruction_centrale envRes_centrale (C 3 4) player1 
                `shouldBe` envRes

prop_post_destruction_centrale_Spec = do
    describe "prop_post_destruction_centrale" $ do

        it "returns True because the centrale has been removed and the other things not changed " $ do
            prop_post_destruction_centrale envRes_centrale (C 3 4) player1
                `shouldBe` True


engineSpec = do 
    envSmartSpec
    prop_pre_destructionQG_Spec
    prop_post_destructionQG_Spec
    destructionQG_Spec
    prop_pre_set_raffinerie_Spec
    set_raffinerie_Spec
    prop_post_set_raffinerie_Spec
    prop_pre_destruction_raffinerie_Spec
    destruction_raffinerie_Spec
    prop_post_destruction_raffinerie_Spec
    prop_pre_set_usine_Spec
    set_usine_Spec
    prop_post_set_usine_Spec
    prop_pre_destruction_usine_Spec
    destruction_usine_Spec
    prop_post_destruction_usine_Spec
    prop_pre_set_centrale_Spec
    set_centrale_Spec
    prop_post_set_centrale_Spec
    prop_pre_destruction_centrale_Spec
    destruction_centrale_Spec
    prop_post_destruction_centrale_Spec