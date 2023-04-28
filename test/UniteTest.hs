module UniteTest (engineSpec) where

import Data.Set (Set)
import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib 

import Test.Hspec -- a voir

import Data.Maybe as May

import Control.Exception (evaluate)

------------------------------------------------------------------------------------------------------------------------------------------------
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

u1 :: Batiment
u1 = Batiment "Usine" 0 (C 4 4) (JoueurId 0)

envRes :: Environnement
envRes = Environnement [player1, player2, player3] carteEx (M.empty) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_sansUsine :: Environnement
envRes_sansUsine = Environnement [player1, player2, player3] carteEx (M.empty) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3)])

envRes_collecteur :: Environnement
envRes_collecteur = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_valide))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_combattant :: Environnement
envRes_combattant = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Combattant combattant_valide))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

----------------------------------------------------------cuve--------------------------------------------------------------------------------------
cuve_valide_pleine :: Cuve
cuve_valide_pleine = CuvePleine 10
 
cuve_valide_normale :: Cuve
cuve_valide_normale = Cuve 1 10

cuve_valide_vide :: Cuve
cuve_valide_vide = CuveVide 10

cuve_invalide :: Cuve
cuve_invalide = Cuve 10 1

-------------------------------------------------------------ordre----------------------------------------------------------------------------------

collecter :: Ordre
collecter = Collecter (C 1 1)

ordreRien :: Ordre
ordreRien = Rien

ordreDeplacer :: Ordre
ordreDeplacer = Deplacer (C 9 9)

ordre_valide_patrouiller :: Ordre 
ordre_valide_patrouiller = Patrouiller (C 0 0) (C 1 1)

ordre_invalide_patrouiller :: Ordre 
ordre_invalide_patrouiller = Patrouiller (C 0 0) (C 0 0)

-------------------------------------------------------------unite-----------------------------------------------------------------------------------------

uniteEtudiant :: Unite
uniteEtudiant = Unite "Collecteur" (C 4 4) (JoueurId 1) 

uniteProf :: Unite
uniteProf = Unite "Prof" (C 1 2) (JoueurId 2) 

uniteCombattant :: Unite 
uniteCombattant = Unite "Combattant" (C 4 4) (JoueurId 1) 

------------------------------------------------------------collecteur--------------------------------------------------------------------------------
collecteur_valide :: Collecteur
collecteur_valide = Collecteur (UniteId 1) uniteEtudiant cuve_valide_pleine 3 [] ordreRien 

collecteur_invalide_pv_negatif :: Collecteur
collecteur_invalide_pv_negatif = Collecteur (UniteId 1)  uniteProf cuve_valide_normale (-5) [] ordreRien

collecteur_invalide_mauvais_ordre :: Collecteur
collecteur_invalide_mauvais_ordre = Collecteur (UniteId 1) uniteEtudiant cuve_valide_vide 10 [ordre_valide_patrouiller] ordreDeplacer

collecteurs_partie :: [Collecteur]
collecteurs_partie = []

collecteurs_partie_apres :: [Collecteur]
collecteurs_partie_apres = [collecteur_valide]

-----------------------------------------------------------combattant---------------------------------------------------------------------------------

combattant_valide :: Combattant
combattant_valide = Combattant (UniteId 1)  uniteCombattant 3 [] ordreRien

combattant_invalide_pv_negatif :: Combattant
combattant_invalide_pv_negatif = Combattant (UniteId 1)  uniteProf (-5) [ordre_valide_patrouiller] ordreRien

combattant_invalide_mauvais_ordre1 :: Combattant
combattant_invalide_mauvais_ordre1 = Combattant (UniteId 1) uniteProf 10 [collecter] collecter

combattant_invalide_mauvais_ordre2 :: Combattant
combattant_invalide_mauvais_ordre2 = Combattant (UniteId 1) uniteProf 10 [ordre_invalide_patrouiller] ordre_invalide_patrouiller

combattants_partie :: [Combattant]
combattants_partie = []

combattants_partie_apres :: [Combattant]
combattants_partie_apres = [combattant_valide]

prop_inv_Cuve_Spec = do
   describe "prop_inv_Cuve" $ do

    it "returns True because in 'cuve_valide_pleine' 10 > 0 " $ do
      prop_inv_Cuve cuve_valide_pleine
        `shouldBe` True

    it "returns True because in 'cuve_valide_normale' 1 > 0 and 1 < 10 " $ do
      prop_inv_Cuve cuve_valide_normale
        `shouldBe` True
    
    it "returns True because in 'cuve_valide_vide' 10 > 0 " $ do
      prop_inv_Cuve cuve_valide_vide
        `shouldBe` True

prop_inv_Ordre_Spec = do
    describe "prop_inv_Ordre" $ do

        it "returns True because in 'ordre_valide_patrouiller' Patrouiller has (C 0 0) != (C 1 1)" $ do
            prop_inv_Ordre ordre_valide_patrouiller
                `shouldBe` True
        
        it "returns False because in 'ordre_invalide_patrouiller' Patrouiller has (C 0 0) == (C 0 0)" $ do
            prop_inv_Ordre ordre_invalide_patrouiller
                `shouldBe` False

prop_inv_Collecteur_Spec = do 
    describe "prop_inv_Collecteur" $ do

        it "returns True because in 'combattant_valide' 'uniteEtudiant', 'collecter', 'ordreDeplacer', 'ordreRien' have True invariants, pv >= 0 and all the ordres are OK for Collecteur" $ do
            prop_inv_Collecteur collecteur_valide
                `shouldBe` True
        
        it "returns False because in 'collecteur_invalide_pv_negatif' pv < 0" $ do
            prop_inv_Collecteur collecteur_invalide_pv_negatif
                `shouldBe` False
        
        it "returns False because in 'collecteur_invalide_mauvais_ordre' has 'ordre_valide_patrouiller' in his list of ordres and the Collecteur cannot do this type of ordre" $ do
            prop_inv_Collecteur collecteur_invalide_mauvais_ordre
                `shouldBe` False

prop_inv_Combattant_Spec = do 
    describe "prop_inv_Combattant" $ do

        it "returns True because in 'combattant_valide' 'uniteProf', 'ordre_valide_patrouiller', 'ordreDeplacer', 'ordreRien' have True invariants, pv >= 0 and all the ordres are OK for Combattant" $ do
            prop_inv_Combattant combattant_valide 
                `shouldBe` True
        
        it "returns False because in 'combattant_invalide_pv_negatif' pv < 0" $ do
            prop_inv_Combattant combattant_invalide_pv_negatif
                `shouldBe` False
        
        it "returns False because in 'combattant_invalide_mauvais_ordre1' has 'Collecter' in his list of ordres and the Combattant cannot do this type of ordre" $ do
            prop_inv_Combattant combattant_invalide_mauvais_ordre1
                `shouldBe` False

        it "returns False because in 'combattant_invalide_mauvais_ordre2' has an ordre Pattrouiller which is invalid" $ do
            prop_inv_Combattant combattant_invalide_mauvais_ordre2
                `shouldBe` False


prop_pre_set_collecteur_Spec = do
   describe "prop_pre_set_collecteur" $ do

    it "returns True because in 'envRes' invariant is OK, 'player1' has an usine in '(C 4 4)' and enough credits" $ do
      prop_pre_set_collecteur envRes (C 4 4) player1 collecteurs_partie
        `shouldBe` True

    it "returns False because in 'envRes_sansUsine' hasn't an usine" $ do
      prop_pre_set_collecteur envRes_sansUsine (C 4 4) player1 collecteurs_partie
        `shouldBe` False
    
    it "returns False because in 'player2' hasn't an usine in 'envRes'" $ do
      prop_pre_set_collecteur envRes (C 4 4) player2 collecteurs_partie
        `shouldBe` False
    
set_collecteur_Spec = do
   describe "set_collecteur" $ do

    it "returns the enviroment that the collecteur is build" $ do
      set_collecteur envRes (C 4 4) player1 collecteurs_partie
        `shouldBe` (envRes_collecteur, collecteurs_partie_apres)

prop_post_set_collecteur_Spec = do
   describe "prop_post_set_collecteur" $ do

    it "returns True because the enviroment has a new collecteur, the credit of the player has been diminue but other things didn't changed" $ do
      prop_post_set_collecteur envRes (C 4 4) player1 collecteurs_partie
        `shouldBe` True

prop_pre_set_combattant_Spec = do
   describe "prop_pre_set_combattant" $ do

    it "returns True because in 'envRes' invariant is OK, 'player1' has an usine in '(C 4 4)' and enough credits" $ do
      prop_pre_set_combattant envRes (C 4 4) player1 combattants_partie
        `shouldBe` True

    it "returns False because in 'envRes_sansUsine' hasn't an usine" $ do
      prop_pre_set_combattant envRes_sansUsine (C 4 4) player1 combattants_partie
        `shouldBe` False
    
    it "returns False because in 'player2' hasn't an usine in 'envRes'" $ do
      prop_pre_set_combattant envRes (C 4 4) player2 combattants_partie
        `shouldBe` False
    
set_combattant_Spec = do
   describe "set_combattant" $ do

    it "returns the enviroment that the collecteur is build" $ do
      set_combattant envRes (C 4 4) player1 combattants_partie
        `shouldBe` (envRes_collecteur, combattants_partie_apres)

prop_post_set_combattant_Spec = do
   describe "prop_post_set_collecteur" $ do

    it "returns True because in 'envRes' invariant is OK, 'player1' has an usine in '(C 4 4)' and enough credits" $ do
      prop_post_set_combattant envRes (C 4 4) player1 combattants_partie
        `shouldBe` True



engineSpec = do 
    prop_inv_Cuve_Spec
    prop_inv_Ordre_Spec
    prop_inv_Collecteur_Spec
    prop_inv_Combattant_Spec
    prop_pre_set_collecteur_Spec
    set_collecteur_Spec
    prop_post_set_collecteur_Spec
    prop_pre_set_combattant_Spec
    set_combattant_Spec
    prop_post_set_combattant_Spec

