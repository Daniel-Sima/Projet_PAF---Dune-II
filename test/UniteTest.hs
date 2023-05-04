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

player1_buying_collecteur :: Joueur
player1_buying_collecteur = Joueur "0" (JoueurId 0) 90

player1_buying_combattant :: Joueur
player1_buying_combattant = Joueur "0" (JoueurId 0) 80

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
envRes_collecteur = Environnement [player1_buying_collecteur, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_valide0))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_combattant :: Environnement
envRes_combattant = Environnement [player1_buying_combattant, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Combattant combattant_valide_nouveau))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_combattant_collecteur :: Environnement
envRes_combattant_collecteur = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 2), (get_unite_Combattant combattant_valide)), ((UniteId 1), (get_unite_Collecteur collecteur_valide))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_collecteur_invalide :: Environnement
envRes_collecteur_invalide = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_invalide_pv_0))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_collecteur_invalide_apres :: Environnement
envRes_collecteur_invalide_apres = Environnement [player1, player2, player3] carteEx (M.empty) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_combattant_pv_negatif:: Environnement
envRes_combattant_pv_negatif = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_valide2)), ((UniteId 1), (get_unite_Combattant combattant_invalide_pv_negatif))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_collecteur_pv_negatif_apres :: Environnement
envRes_collecteur_pv_negatif_apres = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_valide2))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_combattant_collecteur_apres_deplacer :: Environnement
envRes_combattant_collecteur_apres_deplacer = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Combattant combattant_valide_apres_deplacer)), ((UniteId 1), (get_unite_Collecteur collecteur_valide_apres_deplacer))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_combattant_collecteur_test_meet_water :: Environnement
envRes_combattant_collecteur_test_meet_water = Environnement [player1, player2, player3] carteEx (M.fromList [ ((UniteId 1), (get_unite_Collecteur collecteur_valide_by_water))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_combattant_collecteur_test_meet_water_apres_deplacer :: Environnement
envRes_combattant_collecteur_test_meet_water_apres_deplacer = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 1), (get_unite_Collecteur collecteur_valide_by_water_apres_deplacer))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_collecteur_bloque :: Environnement
envRes_collecteur_bloque = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_bloque))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_collecteur_bloque_apres :: Environnement
envRes_collecteur_bloque_apres = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_bloque_apres))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_collecteur_1ordre :: Environnement
envRes_collecteur_1ordre = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_1ordre))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])

envRes_collecteur_0ordre :: Environnement
envRes_collecteur_0ordre = Environnement [player1, player2, player3] carteEx (M.fromList [((UniteId 0), (get_unite_Collecteur collecteur_0ordre))]) (M.fromList [(BatId 0, qr1), (BatId 1, qr2), (BatId 2, qr3), (BatId 3, u1)])


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

uniteBloque :: Unite
uniteBloque = Unite "Collecteur" (C 3 4) (JoueurId 1) 

uniteEtudiant :: Unite
uniteEtudiant = Unite "Collecteur" (C 4 4) (JoueurId 1) 

uniteEtudiant0 :: Unite
uniteEtudiant0 = Unite "Collecteur" (C 4 4) (JoueurId 0) 

uniteEtudiant_by_water_apres_deplacer :: Unite
uniteEtudiant_by_water_apres_deplacer = Unite "Collecteur" (C 4 2) (JoueurId 1) 

uniteEtudiant_by_water :: Unite
uniteEtudiant_by_water = Unite "Collecteur" (C 3 2) (JoueurId 1) 

uniteEtudiant2 :: Unite
uniteEtudiant2 = Unite "Collecteur" (C 0 2) (JoueurId 1) 

uniteEtudiant_apres_deplacer :: Unite
uniteEtudiant_apres_deplacer = Unite "Collecteur" (C 4 3) (JoueurId 1) 

uniteProf :: Unite
uniteProf = Unite "Prof" (C 1 2) (JoueurId 2) 

uniteCombattant :: Unite 
uniteCombattant = Unite "Combattant" (C 4 4) (JoueurId 0) 

uniteCombattant_apres_deplacer :: Unite 
uniteCombattant_apres_deplacer = Unite "Combattant" (C 3 4) (JoueurId 0)

------------------------------------------------------------collecteur--------------------------------------------------------------------------------

collecteur_bloque:: Collecteur
collecteur_bloque = Collecteur (UniteId 0) uniteBloque cuve_valide_pleine 3 [] (Deplacer (C 1 1)) 

collecteur_bloque_apres:: Collecteur
collecteur_bloque_apres = Collecteur (UniteId 0) uniteBloque cuve_valide_pleine 3 [] (Rien) 

collecteur_valide :: Collecteur
collecteur_valide = Collecteur (UniteId 1) uniteEtudiant cuve_valide_pleine 3 [Collecter (C 4 4)] (Deplacer (C 4 2)) 

collecteur_valide0 :: Collecteur
collecteur_valide0 = Collecteur (UniteId 0) uniteEtudiant0 cuve_valide_vide 3 [] (ordreRien) 

collecteur_valide_by_water :: Collecteur
collecteur_valide_by_water = Collecteur (UniteId 1) uniteEtudiant_by_water cuve_valide_pleine 3 [Collecter (C 4 4)] (Deplacer (C 3 4))

collecteur_valide_by_water_apres_deplacer :: Collecteur
collecteur_valide_by_water_apres_deplacer = Collecteur (UniteId 1) uniteEtudiant_by_water_apres_deplacer cuve_valide_pleine 3 [Collecter (C 4 4)] (Deplacer (C 3 4))

collecteur_valide2 :: Collecteur
collecteur_valide2 = Collecteur (UniteId 0) uniteEtudiant2 cuve_valide_pleine 10 [Collecter (C 4 4)] (Deplacer (C 4 2)) 

collecteur_1ordre :: Collecteur
collecteur_1ordre = Collecteur (UniteId 0) uniteEtudiant2 cuve_valide_pleine 10 [Collecter (C 4 4)] (Deplacer (C 0 2)) 

collecteur_0ordre :: Collecteur
collecteur_0ordre = Collecteur (UniteId 0) uniteEtudiant2 cuve_valide_pleine 10 [] (Collecter (C 4 4)) 

collecteur_valide_apres_deplacer :: Collecteur
collecteur_valide_apres_deplacer = Collecteur (UniteId 1) uniteEtudiant_apres_deplacer cuve_valide_pleine 3 [Collecter (C 4 4)] (Deplacer (C 4 2))

collecteur_invalide_pv_negatif :: Collecteur
collecteur_invalide_pv_negatif = Collecteur (UniteId 1)  uniteProf cuve_valide_normale (-5) [] ordreRien

collecteur_invalide_pv_0 :: Collecteur
collecteur_invalide_pv_0 = Collecteur (UniteId 0)  uniteProf cuve_valide_normale 0 [] ordreRien

collecteur_invalide_mauvais_ordre :: Collecteur
collecteur_invalide_mauvais_ordre = Collecteur (UniteId 1) uniteEtudiant cuve_valide_vide 10 [ordre_valide_patrouiller] ordreDeplacer

collecteurs_partie :: [Collecteur]
collecteurs_partie = []

collecteurs_partie_apres :: [Collecteur]
collecteurs_partie_apres = [collecteur_valide]

collecteurs_partie_apres_by_water_apres_deplacer :: [Collecteur]
collecteurs_partie_apres_by_water_apres_deplacer = [collecteur_valide_by_water_apres_deplacer]

collecteurs_partie_apres_by_water :: [Collecteur]
collecteurs_partie_apres_by_water = [collecteur_valide_by_water]

collecteurs_partie_apres_deplacer :: [Collecteur]
collecteurs_partie_apres_deplacer = [collecteur_valide_apres_deplacer]

-----------------------------------------------------------combattant---------------------------------------------------------------------------------

combattant_valide :: Combattant
combattant_valide = Combattant (UniteId 0) uniteCombattant 1 [Deplacer (C 4 3)] (Deplacer (C 1 4))

combattant_valide_nouveau :: Combattant
combattant_valide_nouveau = Combattant (UniteId 0) uniteCombattant 3 [] ordreRien

combattant_valide_apres_deplacer :: Combattant
combattant_valide_apres_deplacer = Combattant (UniteId 0) uniteCombattant_apres_deplacer 1 [Deplacer (C 4 3)] (Deplacer (C 1 4))

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

combattants_partie_apres_deplacer :: [Combattant]
combattants_partie_apres_deplacer = [combattant_valide_apres_deplacer]

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
    
    it "returns False because 'player2' hasn't an usine in 'envRes'" $ do
      prop_pre_set_collecteur envRes (C 4 4) player2 collecteurs_partie
        `shouldBe` False
    
set_collecteur_Spec = do
   describe "set_collecteur" $ do

    it "returns the enviroment that the collecteur is build" $ do
      set_collecteur envRes (C 4 4) player1 collecteurs_partie
        `shouldBe` (envRes_collecteur, [collecteur_valide0])

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
        `shouldBe` (envRes_combattant, [combattant_valide_nouveau])

prop_post_set_combattant_Spec = do
   describe "prop_post_set_combattant" $ do

    it "returns True because in 'envRes' invariant is OK, 'player1' has an usine in '(C 4 4)' and enough credits" $ do
      prop_post_set_combattant envRes (C 4 4) player1 combattants_partie
        `shouldBe` True

----------------------------------------------------------- Etape ---------------------------------------------------------------------------------
verifie_unites_Spec = do
   describe "verifie_unites" $ do

    it "returns the same Environnement because all unites (Collecteur/Combattant) have pv >= 0" $ do
      verifie_unites envRes_combattant_collecteur collecteurs_partie_apres combattants_partie_apres
        `shouldBe` (envRes_combattant_collecteur, collecteurs_partie_apres, combattants_partie_apres)
    
    it "returns an Environnement without the collecteur who have pv = 0 and removed from his lists" $ do
      verifie_unites envRes_collecteur_invalide [collecteur_invalide_pv_0] []
        `shouldBe` (envRes_collecteur_invalide_apres, [], [])

    it "returns an Environnement without the Combattant that has pv < 0 and removed from the Combattant lists, Collecteurs in list and Environnement stay the same" $ do
      verifie_unites envRes_combattant_pv_negatif [collecteur_valide2] [combattant_invalide_pv_negatif]
        `shouldBe` (envRes_collecteur_pv_negatif_apres, [collecteur_valide2], [])


deplacer_Unite_Cood_Spec = do
   describe "deplacer_Unite_Cood" $ do

    it "returns the triple of the enviroment, the list of collecteurs and the list of the combatants after the unites have moved" $ do
      deplacer_Unite_Cood envRes_combattant_collecteur collecteurs_partie_apres combattants_partie_apres
        `shouldBe` (envRes_combattant_collecteur_apres_deplacer, collecteurs_partie_apres_deplacer, combattants_partie_apres_deplacer)

    it "returns the triple of the enviroment, the list of collecteurs and the list of the combatants after the unites have moved and changed ordres" $ do
      deplacer_Unite_Cood envRes_collecteur_1ordre [collecteur_1ordre] []
        `shouldBe` (envRes_collecteur_0ordre, [collecteur_0ordre], [])

    it "returns the Environnement and the list of Collecteurs that have only changes the ordre because the Collecteur was blocked" $ do
      deplacer_Unite_Cood envRes_collecteur_bloque [collecteur_bloque] []
        `shouldBe` (envRes_collecteur_bloque_apres, [collecteur_bloque_apres], [])
    
    it "returns the triple of the enviroment, the list of collecteurs and the list of the combatants after the unites have moved" $ do
      deplacer_Unite_Cood envRes_combattant_collecteur_test_meet_water collecteurs_partie_apres_by_water combattants_partie
        `shouldBe` (envRes_combattant_collecteur_test_meet_water_apres_deplacer, collecteurs_partie_apres_by_water_apres_deplacer, combattants_partie)


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
    verifie_unites_Spec
    deplacer_Unite_Cood_Spec
