module UniteTest (engineSpec) where

import Data.Set (Set)
import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib 

import Test.Hspec -- a voir

import Data.Maybe as May

import Control.Exception (evaluate)

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
uniteEtudiant = Unite "Etudiant" (C 3 4) (JoueurId 1) 

uniteProf :: Unite
uniteProf = Unite "Prof" (C 1 2) (JoueurId 2) 

------------------------------------------------------------collecteur--------------------------------------------------------------------------------
collecteur_valide :: Collecteur
collecteur_valide = Collecteur uniteEtudiant cuve_valide_pleine 10 [collecter, ordreDeplacer, ordreRien] collecter 

collecteur_invalide_pv_negatif :: Collecteur
collecteur_invalide_pv_negatif = Collecteur uniteProf cuve_valide_normale (-5) [] ordreRien

collecteur_invalide_mauvais_ordre :: Collecteur
collecteur_invalide_mauvais_ordre = Collecteur uniteEtudiant cuve_valide_vide 10 [ordre_valide_patrouiller] ordreDeplacer

-----------------------------------------------------------combattant---------------------------------------------------------------------------------

combattant_valide :: Combattant
combattant_valide = Combattant uniteProf 10 [ordre_valide_patrouiller, ordreDeplacer, ordreRien] ordre_valide_patrouiller

combattant_invalide_pv_negatif :: Combattant
combattant_invalide_pv_negatif = Combattant uniteProf (-5) [ordre_valide_patrouiller] ordreRien

combattant_invalide_mauvais_ordre1 :: Combattant
combattant_invalide_mauvais_ordre1 = Combattant uniteProf 10 [collecter] collecter

combattant_invalide_mauvais_ordre2 :: Combattant
combattant_invalide_mauvais_ordre2 = Combattant uniteProf 10 [ordre_invalide_patrouiller] ordre_invalide_patrouiller

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
        
engineSpec = do 
    prop_inv_Cuve_Spec
    prop_inv_Ordre_Spec
    prop_inv_Collecteur_Spec
    prop_inv_Combattant_Spec
