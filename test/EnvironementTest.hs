module EnvironementTest (engineSpec) where

import Data.Set (Set)
import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib 

import Test.Hspec -- a voir

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

---------------------------------------------------------------joueurs-----------------------------------------------------------------------------------------------
player1 :: Joueur
player1 = Joueur "Daniel" (JoueurId 1)

player2 :: Joueur
player2 = Joueur "Yukai" (JoueurId 2)

player3 :: Joueur
player3 = Joueur "Daniel" (JoueurId 3)

player4 :: Joueur
player4 = Joueur "Ronaldo" (JoueurId 1)

player5 :: Joueur
player5 = Joueur "Daniel" (JoueurId 2)

playerIlegal :: Joueur
playerIlegal = Joueur "" (JoueurId 2)

-----------------------------------------------------------------batiments------------------------------------------------------------------------------------------
fac :: Batiment
fac = Batiment "Faculte" 1 (C 0 0) (JoueurId 1)

crous :: Batiment
crous = Batiment "Crous" 10 (C 2 0) (JoueurId 2)

cafet :: Batiment
cafet = Batiment "Cafet" 5 (C 3 04) (JoueurId 3)

facIllegal2 :: Batiment
facIllegal2 = Batiment "" 8 (C 0 0) (JoueurId 1)

-----------------------------------------------------------------unite------------------------------------------------------------------------------------------

uniteEtudiant :: Unite
uniteEtudiant = Unite "Etudiant" (C 3 4) (JoueurId  1)

uniteProf :: Unite
uniteProf = Unite "Prof" (C 1 2) (JoueurId  2)

uniteIllegalName :: Unite
uniteIllegalName = Unite "" (C 0 0) (JoueurId  1)

uniteIllegalCoord :: Unite
uniteIllegalCoord = Unite "Touriste" (C 5 5) (JoueurId  3)

uniteEtudiantSameCase2 :: Unite
uniteEtudiantSameCase2 = Unite "Etudiant2" (C 3 4) (JoueurId  4)

-------------------------------------------------------------------------env------------------------------------------------------------------------------------
envLegal :: Environnement
envLegal = Environnement [player1, player2] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteProf)]) (M.fromList [(BatId 1, fac), (BatId 2, crous)])

envIllegalPlayer :: Environnement
envIllegalPlayer = Environnement [player1, player3] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteProf)]) (M.fromList [(BatId 1, fac), (BatId 2, crous)])

envIllegalPlayer2 :: Environnement
envIllegalPlayer2 = Environnement [player1, player4] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteProf)]) (M.fromList [(BatId 1, fac), (BatId 2, crous)])

envIllegalPlayer3 :: Environnement
envIllegalPlayer3 = Environnement [player1, player2, player5] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteProf)]) (M.fromList [(BatId 1, fac), (BatId 2, crous)])

envIllegalBatsurRessourse :: Environnement
envIllegalBatsurRessourse = Environnement [player1, player3] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteProf)]) (M.fromList [(BatId 1, fac), (BatId 2, Batiment "BatSurRessource" 10 (C 1 2) (JoueurId 2))])

envIllegalBatsurEau :: Environnement
envIllegalBatsurEau = Environnement [player1, player3] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteProf)]) (M.fromList [(BatId 1, fac), (BatId 2, Batiment "BatSurEau" 10 (C 3 3) (JoueurId 2))])

envIllegalBatHorsCarte :: Environnement
envIllegalBatHorsCarte = Environnement [player1, player3] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteProf)]) (M.fromList [(BatId 1, fac), (BatId 2, Batiment "BatHorsCarte" 10 (C 9 3) (JoueurId 2))])

envIllegalUnitName :: Environnement
envIllegalUnitName = Environnement [player1, player3] carteEx (M.fromList [(UniteId 1, uniteIllegalName), (UniteId 2, uniteProf)])  (M.fromList [(BatId 1, fac)])

envIllegalUnitsurEau :: Environnement
envIllegalUnitsurEau = Environnement [player1, player2] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, Unite "UnitsurEau" (C 3 3) (JoueurId 2))]) (M.fromList [(BatId 1, fac), (BatId 2, crous)])

envIllegalUnitHorsCarte :: Environnement
envIllegalUnitHorsCarte = Environnement [player1, player2] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteIllegalCoord)]) (M.fromList [(BatId 1, fac), (BatId 2, crous)])

envIllegalUnitBatAndUnitSameCaseHerbe :: Environnement
envIllegalUnitBatAndUnitSameCaseHerbe = Environnement [player1, player2] carteEx (M.fromList [(UniteId 1, uniteEtudiant), (UniteId 2, uniteEtudiantSameCase2)]) (M.fromList [(BatId 1, cafet)])

envIllegalUnitSurMemeRessource :: Environnement
envIllegalUnitSurMemeRessource = Environnement [player1, player2] carteEx (M.fromList [(UniteId 1, Unite "UnitsurRessourse1" (C 1 2) (JoueurId 2)), (UniteId 2, Unite "UnitsurRessourse2" (C 1 2) (JoueurId 1))]) (M.fromList [(BatId 1, fac), (BatId 2, crous)])

prop_inv_JoueurSpec = do
   describe "prop_inv_Joueur " $ do

    it "returns true because length of \"Daniel\" is > 0" $ do
      prop_inv_Joueur player1
        `shouldBe` True

    it "returns true because length of \"\" is 0" $ do
      prop_inv_Joueur playerIlegal
        `shouldBe` False

prop_inv_BatimentSpec = do
   describe "prop_inv_Batiment " $ do

    it "returns true because length name is > 0 and prix=1 > 0 " $ do
      prop_inv_Batiment fac
        `shouldBe` True

    it "returns false because prix=0 isn't > 0" $ do
      prop_inv_Batiment crous
        `shouldBe` True
        
    it "returns false because the length of the name is 0" $ do
      prop_inv_Batiment facIllegal2
        `shouldBe` False

prop_inv_UniteSpec = do
   describe "prop_inv_Unites " $ do

    it "returns true because length name is > 0 " $ do
      prop_inv_Unites uniteEtudiant
        `shouldBe` True

    it "returns false because length name is = 0" $ do
      prop_inv_Unites uniteIllegalName
        `shouldBe` False   

prop_inv_EnvironnementSpec = do
   describe "prop_inv_Environnement " $ do

    it "returns True because envLegal has all the fields ok (TODO) " $ do
      prop_inv_Environnement envLegal
        `shouldBe` True

    ------------------------------- Players tests --------------------------------------------------------------------
    it "returns False because envIllegalPlayer has two players with the same name " $ do
      prop_inv_Environnement envIllegalPlayer
        `shouldBe` False

    it "returns False because envIllegalPlayer2 has two players with the same ID " $ do
      prop_inv_Environnement envIllegalPlayer2
        `shouldBe` False
        
    it "returns False because envIllegalPlayer3 has a player with the same ID and Username that other players" $ do
      prop_inv_Environnement envIllegalPlayer3
        `shouldBe` False

    ------------------------------- Batiment tests --------------------------------------------------------------------
    it "returns False because envIllegalBatsurEau has a batiment on water" $ do
      prop_inv_Environnement envIllegalBatsurEau
        `shouldBe` False

    it "returns False because envIllegalBatsurRessourse has a batiment on ressource" $ do
      prop_inv_Environnement envIllegalBatsurRessourse
        `shouldBe` False

    it "returns False because envIllegalBatHorsCarte has a batiment out of carte" $ do
      prop_inv_Environnement envIllegalBatHorsCarte
        `shouldBe` False

    ------------------------------Unite tests-----------------------------------------------------------------------------
    it "returns False because envIllegalUnitName has a Unit with name length = 0" $ do
      prop_inv_Environnement envIllegalUnitName
        `shouldBe` False

    it "returns False because envIllegalUnitsurEau has a batiment on water" $ do
      prop_inv_Environnement envIllegalUnitsurEau
        `shouldBe` False

    it "returns False because envIllegalUnitHorsCarte has a Unit out of carte" $ do
      prop_inv_Environnement envIllegalUnitHorsCarte
        `shouldBe` False
    
    it "returns False because envIllegalUnitBatAndUnitSameCaseHerbe has a Unit and Batiment on same the Herbe case" $ do
      prop_inv_Environnement envIllegalUnitBatAndUnitSameCaseHerbe
        `shouldBe` False
    
    it "returns False because there are two unite on the same ressource" $ do
      prop_inv_Environnement envIllegalUnitSurMemeRessource
        `shouldBe` False

  

engineSpec = do 
    prop_inv_JoueurSpec
    prop_inv_BatimentSpec
    prop_inv_UniteSpec
    prop_inv_EnvironnementSpec
    