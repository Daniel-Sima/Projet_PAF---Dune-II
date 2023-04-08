module Lib
    ( someFunc, 
    prop_inv_Terrain,
    prop_inv_Carte,
    collecteCase,
    prop_pre_collecteCase,
    get_val_Ressource,
    prop_post_collecteCase,
    prop_inv_Joueur,
    prop_inv_Batiment,
    prop_inv_Unites,
    prop_inv_Environnement,
    listCoordtoJoueur,
    getAllBats,
    envSmart,
    getBatIdProprio,
    destructionQG,
    prop_pre_destructionQG,
    prop_post_destructionQG,
    set_raffinerie,
    prop_pre_set_raffinerie,
    getBatIdProprioCoord,
    prop_post_set_raffinerie,
    prop_pre_destruction_raffinerie,
    destruction_raffinerie,
    prop_post_destruction_raffinerie,
    prop_pre_set_usine,
    set_usine,
    prop_post_set_usine,
    Coord(..),
    Terrain(..),
    Carte(..),
    Joueur(..),
    Batiment(..),
    Unite(..),
    Environnement(..),
    JoueurId(..),
    UniteId(..),
    BatId(..)
    ) where

import qualified Data.Map.Strict as M

import Data.List as List


import Data.Maybe as May

data Coord = C {cx :: Int ,cy :: Int} deriving (Show, Eq, Ord)

data Terrain = Herbe
    | Ressource Int     
    | Eau
    deriving (Show, Eq, Ord)

        
newtype Carte = Carte {carte :: M.Map Coord Terrain } deriving (Show, Eq, Ord) -- A voir 

------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Part I -----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
prop_inv_Terrain :: Terrain -> Bool
prop_inv_Terrain (Ressource val) = val > 0
prop_inv_Terrain _ = True

prop_inv_Carte :: Carte -> Bool 
prop_inv_Carte (Carte mapC) =  (M.foldrWithKey funct True mapC) 
                        where 
                            funct k val acc = (M.foldrWithKey (funct2 k) acc mapC)  && prop_inv_Terrain val
                         
                            funct2 k k2 _ acc  = 
                               if k == k2 then acc -- a voir si acc && True
                               else ((verifie_chemin1 k k2 acc) || (verifie_chemin2 k k2 acc))
                           
                            verifie_chemin1 (C x1 y1) (C x2 y2) res = 
                               if (x1 == x2) && (y1 == y2) then res  -- a voir si acc && res
                               else 
                                if (y1 /= y2) then 
                                    if (y1 < y2) then case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin1 (C x1 (y1+1)) (C x2 y2) res
                                        Nothing -> res && False 
                                    else case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin1 (C x1 (y1-1)) (C x2 y2) res
                                        Nothing -> res && False 
                                else if (x1 /= x2) then 
                                    if (x1 < x2) then case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin1 (C (x1+1) y1) (C x2 y2) res
                                        Nothing -> res && False 
                                    else case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin1 (C (x1-1) y1) (C x2 y2) res
                                        Nothing -> res && False 
                                else res
                               
                            verifie_chemin2 (C x1 y1) (C x2 y2) res = 
                               if (x1 == x2) && (y1 == y2) then res  -- a voir si acc && res
                               else 
                                if (x1 /= x2) then 
                                    if (x1 < x2) then case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin2 (C (x1+1) y1) (C x2 y2) res
                                        Nothing -> res && False 
                                    else case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin2 (C (x1-1) y1) (C x2 y2) res
                                        Nothing -> res && False 
                                else if (y1 /= y2) then 
                                    if (y1 < y2) then case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin2 (C x1 (y1+1)) (C x2 y2) res
                                        Nothing -> res && False 
                                    else case ((M.lookup (C x1 y1) mapC)) of 
                                        Just _ -> verifie_chemin2 (C x1 (y1-1)) (C x2 y2) res
                                        Nothing -> res && False 
                                else res
     
                    
collecteCase :: Coord -> Int -> Carte -> (Int, Carte)
collecteCase (C x y) r (Carte mapC) = case M.lookup (C x y) mapC of 
                                        Just (Ressource n) -> if (n > r) then (r, (Carte (M.insert (C x y) (Ressource (n-r)) mapC)))
                                                            else (n, (Carte (M.insert (C x y) Herbe mapC)))
                                        otherwise -> (0, Carte mapC)

prop_pre_collecteCase :: Coord -> Int -> Carte -> Bool
prop_pre_collecteCase (C x y) r (Carte mapC) = (May.isJust (M.lookup (C x y) mapC)) && r > 0


get_val_Ressource :: Terrain -> Maybe Int 
get_val_Ressource (Ressource val) = Just val
get_val_Ressource _ = Nothing

-- TODO demander comment verifier le cas False
prop_post_collecteCase :: Coord -> Int -> Carte -> Bool
prop_post_collecteCase (C x y) r (Carte mapC) = 
    let (res, Carte mapRes) = collecteCase (C x y) r (Carte mapC) 
    in 
        if res == 0 then mapRes == mapC
        else if res == r then (M.foldrWithKey (funct1 mapRes) True mapC) && 
            ((May.fromJust(M.lookup (C x y) mapRes) == Ressource ((May.fromJust (get_val_Ressource (May.fromJust (M.lookup (C x y) mapC)))) - r)) || 
            ((May.fromJust (M.lookup (C x y) mapRes) == Herbe) && ((May.fromJust (get_val_Ressource (May.fromJust (M.lookup (C x y) mapC)))) == r)))          
        else if (res > 0) && (res<r) then 
            ((May.fromJust (M.lookup (C x y) mapRes)) == Herbe) && (all fun $ zip (M.keys mapRes) (M.elems mapRes))
        else False

        where 
            funct1 mapRes (C x1 y1) val1 acc = (M.foldrWithKey (funct2 (C x1 y1) val1) acc mapRes) 
            funct2 (C x1 y1) val1 (C x2 y2) val2 acc =
                if ((x1 == x2) && (y1 == y2)) && ((C x1 y1) /= (C x y)) then acc && (val1 == val2) 
                else acc  
            fun (coor, t) = 
                if (coor == (C x y)) then True 
                else (May.fromJust (M.lookup coor mapC)) == t 



------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Part II ----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

newtype JoueurId = JoueurId Int deriving (Show, Eq, Ord)
data Joueur = Joueur {username :: String, userid :: JoueurId} deriving (Show, Eq, Ord)

newtype BatId = BatId Int deriving (Show, Eq, Ord)
data Batiment = Batiment {bNom :: String, prix :: Int, batCoord :: Coord, batProprio :: JoueurId} deriving (Eq, Show)

newtype UniteId = UniteId Int deriving (Show, Eq, Ord)
data Unite = Unite {uNom :: String, unitCoord :: Coord, unitProprio :: JoueurId} deriving (Eq, Show)

data Environnement = Environnement {joueurs :: [Joueur], ecarte :: Carte, unites :: M.Map UniteId Unite, batiments :: M.Map BatId Batiment} deriving (Eq, Show)

---------------------------------------------------------Joueur----------------------------------------------------------------------------------------------

jid :: Joueur -> JoueurId
jid (Joueur _ j) = j

jusername :: Joueur -> String
jusername (Joueur username _) = username

get_id_Joueurs :: Environnement -> [JoueurId]
get_id_Joueurs (Environnement j _ _ _ ) = fmap jid j

get_usernames_Joueurs :: Environnement -> [String]
get_usernames_Joueurs (Environnement j _ _ _ ) = fmap jusername j

prop_inv_Joueur :: Joueur -> Bool
prop_inv_Joueur (Joueur user id) = ((length user) > 0)

getListPayers :: Environnement -> [Joueur]
getListPayers (Environnement j _ _ _ ) = j
--------------------------------------------------------Batiment--------------------------------------------------------------------------------------------------

bproprio :: Batiment -> JoueurId
bproprio (Batiment _ _ _ propio) = propio


get_propios_batiment :: Environnement -> [JoueurId]
get_propios_batiment (Environnement _ _ _ batiments) = fmap bproprio (M.elems batiments)

bcoord :: Batiment -> Coord
bcoord (Batiment _ _ coord _) = coord

get_coords_batiment :: Environnement -> [Coord]
get_coords_batiment (Environnement _ _ _ b) = fmap bcoord (M.elems b)

prop_inv_Batiment :: Batiment -> Bool
prop_inv_Batiment (Batiment bNom prix bcoord bproprio) = ((length bNom) > 0) && (prix >= 0)

get_number_BatId :: BatId -> Int
get_number_BatId (BatId number) = number

-------------------------------------------------------Unite-------------------------------------------------------------------------------------------------------
prop_inv_Unites :: Unite -> Bool
prop_inv_Unites (Unite uNom uCoord uProprio) = ((length uNom) > 0)

uproprio :: Unite -> JoueurId
uproprio (Unite _ _ uProprio) = uProprio

get_propios_unites :: Environnement -> [JoueurId]
get_propios_unites (Environnement _ _ unites _) = fmap uproprio (M.elems unites)

ucoord :: Unite -> Coord
ucoord (Unite _ coord _) = coord

get_coords_unite :: Environnement -> [Coord]
get_coords_unite (Environnement _ _ u _) = fmap ucoord (M.elems u)

----------------------------------------------------------------------------------------------------------------------------------------------------------------

allUnique :: Eq a => [a] -> Bool
allUnique xs = List.length xs == List.length (List.nub xs)

prop_inv_Environnement :: Environnement -> Bool
prop_inv_Environnement (Environnement eJoueurs eCarte eUnites eBatiments) = let env = (Environnement eJoueurs eCarte eUnites eBatiments) in
    ((List.all prop_inv_Joueur eJoueurs) && (allUnique (get_id_Joueurs env)) && (allUnique (get_usernames_Joueurs env))) && 
    ((List.all prop_inv_Batiment eBatiments) && (List.all (\x -> elem x (get_id_Joueurs env)) (get_propios_batiment env)) && (List.all (\x-> M.member x (carte eCarte)) (get_coords_batiment env)))  &&
    ((List.all prop_inv_Unites eUnites) && (List.all (\x -> elem x (get_id_Joueurs env)) (get_propios_unites env))  && (List.all (\x-> M.member x (carte eCarte)) (get_coords_unite env))) &&
    (List.all (\x -> (May.fromJust (M.lookup x (carte eCarte))) /= Eau) ((get_coords_batiment env) ++ (get_coords_unite env))) &&
    (allUnique ((get_coords_batiment env) ++ (get_coords_unite env))) &&
    (List.all (\x -> case (May.fromJust (M.lookup x (carte eCarte))) of
                    (Ressource _) -> False
                    _ -> True) (get_coords_batiment env)) && 
    (prop_inv_Carte eCarte)

------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Part III ----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
-- data Environnement = Environnement {joueurs :: [Joueur], ecarte :: Carte, unites :: M.Map UniteId Unite, batiments :: M.Map BatId Batiment}
-- data Batiment = Batiment {bNom :: String, prix :: Int, batCoord :: Coord, batProprio :: JoueurId}

-- counterHerbeMap :: Carte -> Int
-- counterHerbeMap mapp = if ((M.size mapp) == 0) then 0
--                        else M.foldr (\x-> if ((May.fromJust (M.lookup x (carte mapp))) == Herbe) then 1 else 0) 0 (carte mapp) 

listCoordtoJoueur :: [Coord] -> Int -> [Joueur]
listCoordtoJoueur listPlayers index = 
    if (index < List.length listPlayers) then (Joueur ("" ++ show index) (JoueurId index)) : (listCoordtoJoueur listPlayers (index+1))
    else []

getAllBats :: [Coord] -> String -> [Joueur] -> Int -> [(BatId, Batiment)]
getAllBats listCoord nameBat listPlayers prix = zip (fmap (\x -> BatId x) [0..((List.length listCoord)-1)]) (fmap (\(x,y) -> (Batiment nameBat prix x (jid y))) (zip listCoord listPlayers))

envSmart :: Carte -> [Coord] -> Environnement
envSmart mapp listPlayers | (allUnique listPlayers) = 
                            if (List.all (\x -> (May.fromJust (M.lookup x (carte mapp))) == Herbe) listPlayers) then Environnement (listCoordtoJoueur listPlayers 0) (mapp) (M.empty) (M.fromList (getAllBats listPlayers "QG" (listCoordtoJoueur listPlayers 0) 0))
                            else error "Not enough Herbe cases to build QRs for Players"
                          | otherwise = error "Duplicate Players" 
 
getBatIdProprio :: (M.Map BatId Batiment) -> JoueurId -> BatId
getBatIdProprio mapBat jID = List.head (M.keys (M.filter (\x -> (bproprio x) == jID) mapBat))

-----------------------------------------------------quartier generale-----------------------------------------------------------------------------------

destructionQG :: Environnement -> Joueur -> Environnement -- TODO voir si on doit aussi supprimer tous les autres batiments
destructionQG (Environnement joueurs mapp unites bats) player = (Environnement (List.delete player joueurs) mapp unites (M.delete (getBatIdProprio bats (jid player)) bats))  

prop_pre_destructionQG :: Environnement -> Joueur -> Bool
prop_pre_destructionQG (Environnement joueurs mapp unites bats) player = (elem player joueurs) && (prop_inv_Environnement (Environnement joueurs mapp unites bats))

get_nom_batiment :: Batiment -> String
get_nom_batiment (Batiment bNom _ _ _) = bNom

getBatIdProprioCoord ::  (M.Map BatId Batiment) -> Coord -> BatId
getBatIdProprioCoord mapBat coord = List.head (M.keys (M.filter (\x -> (bcoord x) == coord) mapBat))

prop_post_destructionQG :: Environnement -> Joueur -> Bool
prop_post_destructionQG (Environnement joueursAvant mappAvant unitesAvant batsAvant) player =
    let (Environnement joueursApres mappApres unitesApres batsApres) = (destructionQG (Environnement joueursAvant mappAvant unitesAvant batsAvant) player)
    in
        (not (elem player joueursApres)) && ((List.delete player joueursAvant) == joueursApres) &&
        (mappAvant == mappApres) && (unitesAvant == unitesApres) && (not (M.member (getBatIdProprio batsAvant (jid player)) batsApres)) &&
        ((M.delete (getBatIdProprio batsAvant (jid player)) batsAvant) == batsApres) 

----------------------------------------------------raffinerie-------------------------------------------------------------------------------------------

set_raffinerie :: Environnement -> Coord -> Joueur -> Environnement
set_raffinerie (Environnement joueurs mapp unites bats) coord player = (Environnement joueurs mapp unites (M.insert (BatId (M.size bats)) (Batiment "Raffinerie" 0 coord (jid player)) bats))

prop_pre_set_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_pre_set_raffinerie (Environnement joueurs mapp unites bats) coord player = (May.fromJust (M.lookup coord (carte mapp)) == Herbe) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) 

prop_post_set_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_post_set_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = set_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in (joueursAvant == joueursApres) && (mappAvant == mappApres) && (unitesAvant == unitesApres) &&
        (M.member (getBatIdProprioCoord batsApres coord) batsApres) && ((M.delete (getBatIdProprioCoord batsApres coord) batsApres) == batsAvant) &&
        (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord batsApres coord) batsApres)) == "Raffinerie")

prop_pre_destruction_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_pre_destruction_raffinerie (Environnement joueurs mapp unites bats) coord player = 
    (M.member (getBatIdProprioCoord bats coord) bats) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) &&
    (bproprio (May.fromJust (M.lookup (getBatIdProprioCoord bats coord) bats))) == (jid player)

destruction_raffinerie :: Environnement -> Coord -> Joueur -> Environnement
destruction_raffinerie (Environnement joueurs mapp unites bats) coord player = (Environnement joueurs mapp unites (M.delete (getBatIdProprioCoord bats coord) bats))

prop_post_destruction_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_post_destruction_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = destruction_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in (joueursAvant == joueursApres) && (mappAvant == mappApres) && (unitesAvant == unitesApres) &&
        (not (M.member (getBatIdProprioCoord batsAvant coord) batsApres)) && ((M.delete (getBatIdProprioCoord batsAvant coord) batsAvant) == batsApres)

---------------------------------------------------------------usine------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop_pre_set_usine :: Environnement -> Coord -> Joueur -> Bool
prop_pre_set_usine (Environnement joueurs mapp unites bats) coord player = (May.fromJust (M.lookup coord (carte mapp)) == Herbe) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) 

set_usine :: Environnement -> Coord -> Joueur -> Environnement
set_usine (Environnement joueurs mapp unites bats) coord player = (Environnement joueurs mapp unites (M.insert (BatId (M.size bats)) (Batiment "Usine" 0 coord (jid player)) bats))

prop_post_set_usine :: Environnement -> Coord -> Joueur -> Bool
prop_post_set_usine (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = set_usine (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in (joueursAvant == joueursApres) && (mappAvant == mappApres) && (unitesAvant == unitesApres) &&
        (M.member (getBatIdProprioCoord batsApres coord) batsApres) && ((M.delete (getBatIdProprioCoord batsApres coord) batsApres) == batsAvant) &&
        (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord batsApres coord) batsApres)) == "Usine")



someFunc :: IO ()
someFunc = putStrLn "someFunc"

