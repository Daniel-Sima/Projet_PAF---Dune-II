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
    prop_pre_destruction_usine,
    destruction_usine,
    prop_post_destruction_usine,
    prop_pre_set_centrale,
    set_centrale,
    prop_post_set_centrale,
    prop_pre_destruction_centrale,
    destruction_centrale,
    prop_post_destruction_centrale,
    prop_inv_Cuve,
    prop_inv_Ordre,
    prop_inv_Collecteur,
    prop_inv_Combattant,
    reduction_credit_player,
    prop_pre_set_collecteur,
    get_uniteID_Collecteur,
    get_unite_Collecteur,
    add_collecteur_collection,
    set_collecteur,
    prop_post_set_collecteur,
    get_nom_batiment,
    get_uniteID_Combattant,
    get_unite_Combattant,
    add_conbattant_collection,
    prop_pre_set_combattant,
    prop_post_set_combattant,
    set_combattant,
    eliminer_unites_env,
    get_ListUniteID_Collecteur,
    get_ListUniteID_Combattant,
    verifie_unites,
    go_x,
    go_y,
    deplacer_unite,
    listCollecteur_to_listUniteID,
    listCombattant_to_listUnite,
    deplacer_Collecteur_coord_aux,
    deplacer_Collecteur_coord,
    listCombattant_to_listUniteID,
    listCombattant_to_listUnite,
    deplacer_Combattant_coord_aux,
    deplacer_Combattant_coord,
    deplacer_Unite_Cood,
    etape,
    get_player_credit,
    prixRaffinerie,
    prixCollecteur,
    prixCombattant,
    prixUsine,
    prixCentrale,
    productionQG,
    productionCentrale,
    consommationUsine,
    consommationRaffinerie,
    Cuve(..),
    Ordre(..),
    Collecteur(..),
    Combattant(..),
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


-- Magic numbers: -- 
creditsDepart :: Int
creditsDepart = 1000

prixCollecteur :: Int
prixCollecteur = 10

prixCombattant :: Int
prixCombattant = 20

prixRaffinerie :: Int
prixRaffinerie = 50

prixUsine :: Int
prixUsine = 25

prixCentrale :: Int
prixCentrale = 100

productionQG :: Int
productionQG = 50

productionCentrale :: Int
productionCentrale =  20   

consommationUsine :: Int
consommationUsine = 10

consommationRaffinerie :: Int
consommationRaffinerie = 20   

cuveMax :: Integer
cuveMax = 10

pvCollecteurMax :: Int
pvCollecteurMax = 3 

pvCombattantMax :: Int
pvCombattantMax = 3
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

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
data Joueur = Joueur {username :: String, userid :: JoueurId, creditJouer :: Int} deriving (Show, Eq, Ord)

newtype BatId = BatId Int deriving (Show, Eq, Ord)
data Batiment = Batiment {bNom :: String, prix :: Int, batCoord :: Coord, batProprio :: JoueurId} deriving (Eq, Show)

newtype UniteId = UniteId Int deriving (Show, Eq, Ord)
data Unite = Unite {uNom :: String, unitCoord :: Coord, unitProprio :: JoueurId} deriving (Eq, Show, Ord)

data Environnement = Environnement {joueurs :: [Joueur], ecarte :: Carte, unites :: M.Map UniteId Unite, batiments :: M.Map BatId Batiment} deriving (Eq, Show)

---------------------------------------------------------Joueur----------------------------------------------------------------------------------------------

jid :: Joueur -> JoueurId
jid (Joueur _ j _) = j

jusername :: Joueur -> String
jusername (Joueur username _ _) = username

get_id_Joueurs :: Environnement -> [JoueurId]
get_id_Joueurs (Environnement j _ _ _ ) = fmap jid j

get_usernames_Joueurs :: Environnement -> [String]
get_usernames_Joueurs (Environnement j _ _ _ ) = fmap jusername j

prop_inv_Joueur :: Joueur -> Bool
prop_inv_Joueur (Joueur user id creditJouer) = (((length user) > 0) && (creditJouer >= 0))

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

unom :: Unite -> String
unom (Unite nom _ _) = nom

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

-- newtype JoueurId = JoueurId Int deriving (Show, Eq, Ord)
-- data Joueur = Joueur {username :: String, userid :: JoueurId, creditJouer :: Integer} deriving (Show, Eq, Ord)

-- counterHerbeMap :: Carte -> Int
-- counterHerbeMap mapp = if ((M.size mapp) == 0) then 0
--                        else M.foldr (\x-> if ((May.fromJust (M.lookup x (carte mapp))) == Herbe) then 1 else 0) 0 (carte mapp) 

listCoordtoJoueur :: [Coord] -> Int -> [Joueur]
listCoordtoJoueur listPlayers index = 
    if (index < List.length listPlayers) then (Joueur ("" ++ show index) (JoueurId index) creditsDepart) : (listCoordtoJoueur listPlayers (index+1))
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

-----------------------------------------------------quartier general-----------------------------------------------------------------------------------

destructionQG :: Environnement -> Joueur -> Environnement -- TODO voir si on doit aussi supprimer tous les autres batiments
destructionQG (Environnement joueurs mapp unites bats) player = (Environnement (List.delete player joueurs) mapp unites (M.delete (getBatIdProprio bats (jid player)) bats))  

prop_pre_destructionQG :: Environnement -> Joueur -> Bool
prop_pre_destructionQG (Environnement joueurs mapp unites bats) player = 
    (elem player joueurs) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) 

get_nom_batiment :: Batiment -> String
get_nom_batiment (Batiment nom _ _ _) = nom

getBatIdProprioCoord ::  (M.Map BatId Batiment) -> Coord -> BatId
getBatIdProprioCoord mapBat coord = 
    let res = (M.keys (M.filter (\x -> (bcoord x) == coord) mapBat))
    in 
        if (List.length res > 0) then List.head res
        else (BatId (-1))

prop_post_destructionQG :: Environnement -> Joueur -> Bool
prop_post_destructionQG (Environnement joueursAvant mappAvant unitesAvant batsAvant) player =
    let (Environnement joueursApres mappApres unitesApres batsApres) = (destructionQG (Environnement joueursAvant mappAvant unitesAvant batsAvant) player)
    in
        (not (elem player joueursApres)) && ((List.delete player joueursAvant) == joueursApres) &&
        (mappAvant == mappApres) && (unitesAvant == unitesApres) && (not (M.member (getBatIdProprio batsAvant (jid player)) batsApres)) &&
        ((M.delete (getBatIdProprio batsAvant (jid player)) batsAvant) == batsApres) 

---------------------------------------------------- Raffinerie -------------------------------------------------------------------------------------------
get_player_credit :: Joueur -> Int
get_player_credit (Joueur _ _ creditJoueur) = creditJoueur

reduction_credit_player :: [Joueur] -> Joueur -> Int -> [Joueur]
reduction_credit_player joueurs player prixPayer = fmap (\(Joueur username userid creditJouer) -> if (userid == jid player) then (Joueur username userid (creditJouer-prixPayer)) else (Joueur username userid creditJouer)) joueurs

set_raffinerie :: Environnement -> Coord -> Joueur -> Environnement
set_raffinerie (Environnement joueurs mapp unites bats) coord player = (Environnement (reduction_credit_player joueurs player prixRaffinerie) mapp unites (M.insert (BatId (M.size bats)) (Batiment "Raffinerie" 0 coord (jid player)) bats))

prop_pre_set_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_pre_set_raffinerie (Environnement joueurs mapp unites bats) coord player = (May.fromJust (M.lookup coord (carte mapp)) == Herbe) 
    && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) 
    && (elem player joueurs) && ((get_player_credit player) >= prixRaffinerie)

prop_post_set_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_post_set_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = set_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in  ((List.filter (\(Joueur _ idAvant _) -> idAvant /= jid player) joueursAvant) == (List.filter (\(Joueur _ idApres _) -> idApres /= jid player) joueursApres)) 
        && (get_player_credit (List.head (List.filter (\(Joueur _ id _) -> id == jid player) joueursApres))) == ((get_player_credit player) - prixRaffinerie)
        && (mappAvant == mappApres) && (unitesAvant == unitesApres) &&
        (M.member (getBatIdProprioCoord batsApres coord) batsApres) && ((M.delete (getBatIdProprioCoord batsApres coord) batsApres) == batsAvant) &&
        (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord batsApres coord) batsApres)) == "Raffinerie")

prop_pre_destruction_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_pre_destruction_raffinerie (Environnement joueurs mapp unites bats) coord player = 
    (M.member (getBatIdProprioCoord bats coord) bats) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) &&
    (bproprio (May.fromJust (M.lookup (getBatIdProprioCoord bats coord) bats))) == (jid player)
    && (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord bats coord) bats)) == "Raffinerie")


destruction_raffinerie :: Environnement -> Coord -> Joueur -> Environnement
destruction_raffinerie (Environnement joueurs mapp unites bats) coord player = (Environnement joueurs mapp unites (M.delete (getBatIdProprioCoord bats coord) bats))

prop_post_destruction_raffinerie :: Environnement -> Coord -> Joueur -> Bool
prop_post_destruction_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = destruction_raffinerie (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in (joueursAvant == joueursApres) && (mappAvant == mappApres) && (unitesAvant == unitesApres) &&
        (not (M.member (getBatIdProprioCoord batsAvant coord) batsApres)) && ((M.delete (getBatIdProprioCoord batsAvant coord) batsAvant) == batsApres)
        
-------------------------------------------------------------- Usine ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop_pre_set_usine :: Environnement -> Coord -> Joueur -> Bool
prop_pre_set_usine (Environnement joueurs mapp unites bats) coord player = 
    (May.fromJust (M.lookup coord (carte mapp)) == Herbe) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) && ((get_player_credit player) >= prixUsine)

set_usine :: Environnement -> Coord -> Joueur -> Environnement
set_usine (Environnement joueurs mapp unites bats) coord player = (Environnement (reduction_credit_player joueurs player prixUsine) mapp unites (M.insert (BatId (M.size bats)) (Batiment "Usine" 0 coord (jid player)) bats))

prop_post_set_usine :: Environnement -> Coord -> Joueur -> Bool
prop_post_set_usine (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = set_usine (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in ((List.filter (\(Joueur _ idAvant _) -> idAvant /= jid player) joueursAvant) == (List.filter (\(Joueur _ idApres _) -> idApres /= jid player) joueursApres)) 
        && (get_player_credit (List.head (List.filter (\(Joueur _ id _) -> id == jid player) joueursApres))) == ((get_player_credit player) - prixUsine)
        && (mappAvant == mappApres) && (unitesAvant == unitesApres) &&
        (M.member (getBatIdProprioCoord batsApres coord) batsApres) && ((M.delete (getBatIdProprioCoord batsApres coord) batsApres) == batsAvant) &&
        (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord batsApres coord) batsApres)) == "Usine")
        
prop_pre_destruction_usine :: Environnement -> Coord -> Joueur -> Bool
prop_pre_destruction_usine (Environnement joueurs mapp unites bats) coord player = 
    (M.member (getBatIdProprioCoord bats coord) bats) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) 
    && (bproprio (May.fromJust (M.lookup (getBatIdProprioCoord bats coord) bats))) == (jid player) 
    && (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord bats coord) bats)) == "Usine")

destruction_usine :: Environnement -> Coord -> Joueur -> Environnement
destruction_usine (Environnement joueurs mapp unites bats) coord player = (Environnement joueurs mapp unites (M.delete (getBatIdProprioCoord bats coord) bats))

    
prop_post_destruction_usine :: Environnement -> Coord -> Joueur -> Bool
prop_post_destruction_usine (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = destruction_usine (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in (joueursAvant == joueursApres) && (mappAvant == mappApres) && (unitesAvant == unitesApres) 
    && (not (M.member (getBatIdProprioCoord batsAvant coord) batsApres)) && ((M.delete (getBatIdProprioCoord batsAvant coord) batsAvant) == batsApres)

------------------------------------------------------------- Centrale ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop_pre_set_centrale :: Environnement -> Coord -> Joueur -> Bool
prop_pre_set_centrale (Environnement joueurs mapp unites bats) coord player = 
    (May.fromJust (M.lookup coord (carte mapp)) == Herbe) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) && ((get_player_credit player) >= prixCentrale)

set_centrale :: Environnement -> Coord -> Joueur -> Environnement
set_centrale (Environnement joueurs mapp unites bats) coord player = (Environnement (reduction_credit_player joueurs player prixCentrale) mapp unites (M.insert (BatId (M.size bats)) (Batiment "Centrale" 0 coord (jid player)) bats))

prop_post_set_centrale :: Environnement -> Coord -> Joueur -> Bool
prop_post_set_centrale (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = set_centrale (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in ((List.filter (\(Joueur _ idAvant _) -> idAvant /= jid player) joueursAvant) == (List.filter (\(Joueur _ idApres _) -> idApres /= jid player) joueursApres)) 
        && (get_player_credit (List.head (List.filter (\(Joueur _ id _) -> id == jid player) joueursApres))) == ((get_player_credit player) - prixCentrale)
        && (mappAvant == mappApres) && (unitesAvant == unitesApres) &&
        (M.member (getBatIdProprioCoord batsApres coord) batsApres) && ((M.delete (getBatIdProprioCoord batsApres coord) batsApres) == batsAvant) &&
        (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord batsApres coord) batsApres)) == "Centrale")

prop_pre_destruction_centrale :: Environnement -> Coord -> Joueur -> Bool
prop_pre_destruction_centrale (Environnement joueurs mapp unites bats) coord player = 
    (M.member (getBatIdProprioCoord bats coord) bats) && (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && (elem player joueurs) 
    && (bproprio (May.fromJust (M.lookup (getBatIdProprioCoord bats coord) bats))) == (jid player) 
    && (get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord bats coord) bats)) == "Centrale")

destruction_centrale :: Environnement -> Coord -> Joueur -> Environnement
destruction_centrale (Environnement joueurs mapp unites bats) coord player = (Environnement joueurs mapp unites (M.delete (getBatIdProprioCoord bats coord) bats))

prop_post_destruction_centrale :: Environnement -> Coord -> Joueur -> Bool
prop_post_destruction_centrale (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player = 
    let (Environnement joueursApres mappApres unitesApres batsApres) = destruction_centrale (Environnement joueursAvant mappAvant unitesAvant batsAvant) coord player
    in (joueursAvant == joueursApres) && (mappAvant == mappApres) && (unitesAvant == unitesApres) 
    && (not (M.member (getBatIdProprioCoord batsAvant coord) batsApres)) && ((M.delete (getBatIdProprioCoord batsAvant coord) batsAvant) == batsApres)


------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Part IV ----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
-- data Environnement = Environnement {joueurs :: [Joueur], ecarte :: Carte, unites :: M.Map UniteId Unite, batiments :: M.Map BatId Batiment}
-- data Batiment = Batiment {bNom :: String, prix :: Int, batCoord :: Coord, batProprio :: JoueurId}
-- data Unite = Unite {uNom :: String, unitCoord :: Coord, unitProprio :: JoueurId} deriving (Eq, Show)


data Cuve =
  Cuve Integer Integer
  | CuvePleine Integer
  | CuveVide Integer
  deriving (Show, Eq, Ord)

prop_inv_Cuve :: Cuve -> Bool
prop_inv_Cuve (Cuve qty cap) = 0 < qty && qty < cap
prop_inv_Cuve (CuveVide cap) = cap > 0
prop_inv_Cuve (CuvePleine cap) = cap > 0
  
data Ordre =
    Rien 
    | Collecter Coord
    | Deplacer Coord
    | Patrouiller Coord Coord   
    deriving (Show, Eq, Ord) 

prop_inv_Ordre :: Ordre -> Bool
prop_inv_Ordre (Patrouiller coords1 coords2) = coords1 /= coords2
prop_inv_Ordre _ = True

data Collecteur = Collecteur {uniteIDCollecteur :: UniteId, uniteCollecteur :: Unite, cuve :: Cuve, pvCollecteur :: Int, ordresCollecteur :: [Ordre], butCollecteur :: Ordre } deriving (Show, Eq, Ord) -- A voir deriving

verif_Patrouiller :: Ordre -> Bool
verif_Patrouiller ordre = 
    (case ordre of
        Patrouiller _ _ -> False
        otherwise -> True)

prop_inv_Collecteur :: Collecteur -> Bool
prop_inv_Collecteur (Collecteur uniID uni cuve pvv ordres but) = 
    -- (uniID >= 0) && 
    (prop_inv_Unites uni) && (prop_inv_Cuve cuve) && (pvv >= 0) && (List.all prop_inv_Ordre ordres) && (List.all verif_Patrouiller ordres) && (prop_inv_Ordre but) 
    && (verif_Patrouiller but)

data Combattant = Combattant {uniteIDCombattant :: UniteId, uniteCombatant :: Unite, pvCombattant :: Int, ordresCombattant :: [Ordre], butCombattant :: Ordre } deriving (Show, Eq, Ord) -- A voir deriving

verif_Collecter :: Ordre -> Bool
verif_Collecter ordre = 
    (case ordre of
        Collecter _ -> False
        otherwise -> True)

prop_inv_Combattant :: Combattant -> Bool
prop_inv_Combattant (Combattant uniID uni pvv ordres but) = 
    -- (uniID >= 0) && 
    (prop_inv_Unites uni) && (pvv >= 0) && (List.all prop_inv_Ordre ordres) && (List.all verif_Collecter ordres) && (prop_inv_Ordre but) 
    && (verif_Collecter but) 

prop_pre_set_collecteur :: Environnement -> Coord -> Joueur -> [Collecteur] -> Bool
prop_pre_set_collecteur (Environnement joueurs mapp unites bats) coordUsine player listCollecteurs = 
    let res = (M.lookup (getBatIdProprioCoord bats coordUsine) bats)
    in 
        (isJust res) &&
        ((get_nom_batiment (May.fromJust (M.lookup (getBatIdProprioCoord bats coordUsine) bats))) == "Usine") && 
        (bproprio (May.fromJust res) == jid player) && 
        (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && 
        (elem player joueurs) &&
        ((get_player_credit player) >= prixCollecteur) &&
        (List.all prop_inv_Collecteur listCollecteurs)

get_uniteID_Collecteur :: Collecteur -> UniteId
get_uniteID_Collecteur (Collecteur uniteID _ _ _ _ _) = uniteID

get_unite_Collecteur :: Collecteur -> Unite
get_unite_Collecteur (Collecteur _ uniteCollecteur _ _ _ _) = uniteCollecteur

get_unite_Combattant :: Combattant -> Unite
get_unite_Combattant (Combattant _ uniteCombattant _ _ _) = uniteCombattant

get_uniteID_Combattant :: Combattant -> UniteId
get_uniteID_Combattant (Combattant uniteID _ _ _ _) = uniteID

add_collecteur_collection :: UniteId -> Unite -> [Collecteur] -> Unite
add_collecteur_collection uniteID unite listCollecteur = (get_unite_Collecteur (Collecteur uniteID unite (CuveVide cuveMax) pvCollecteurMax [] Rien))

set_collecteur :: Environnement -> Coord -> Joueur -> [Collecteur] -> (Environnement, [Collecteur])
set_collecteur (Environnement joueurs mapp unites bats) coordUsine player listCollecteurs = 
    let (C x y) = coordUsine 
    in 
        ((Environnement (reduction_credit_player joueurs player prixCollecteur) mapp (M.insert (UniteId (M.size unites)) (add_collecteur_collection (UniteId (M.size unites)) (Unite "Collecteur" (C x (y+1)) (jid player)) listCollecteurs) unites) bats), (listCollecteurs ++ [(Collecteur (UniteId (M.size unites)) (Unite "Collecteur" (C x (y+1)) (jid player)) (CuveVide cuveMax) pvCollecteurMax [] Rien)])) 

-- TODO ajouter le lieu de spawn du collecteur
prop_post_set_collecteur :: Environnement -> Coord -> Joueur -> [Collecteur] -> Bool
prop_post_set_collecteur (Environnement joueursAvant mappAvant unitesAvant batsAvant) coordUsine player listCollecteurs =
    let ((Environnement joueursApres mappApres unitesApres batsApres), listCollecteursApres) = set_collecteur (Environnement joueursAvant mappAvant unitesAvant batsAvant) coordUsine player listCollecteurs
    in ((List.filter (\(Joueur _ idAvant _) -> idAvant /= jid player) joueursAvant) == (List.filter (\(Joueur _ idApres _) -> idApres /= jid player) joueursApres)) &&
        (get_player_credit (List.head (List.filter (\(Joueur _ id _) -> id == jid player) joueursApres))) == ((get_player_credit player) - prixCollecteur) &&
        (mappAvant == mappApres) && (batsAvant == batsApres) && 
        unom (May.fromJust (M.lookup (get_uniteID_Collecteur (List.head (listCollecteursApres \\ listCollecteurs))) unitesApres)) == "Collecteur" &&
        ((M.delete (get_uniteID_Collecteur (List.head (listCollecteursApres \\ listCollecteurs))) unitesApres) == unitesAvant)

    
add_conbattant_collection :: UniteId -> Unite -> [Combattant] -> Unite
add_conbattant_collection uniteID unite listCombattant = (get_unite_Combattant (Combattant uniteID unite pvCollecteurMax [] Rien))

-- TODO ajouter le lieu de spawn du combattant
prop_pre_set_combattant :: Environnement -> Coord -> Joueur -> [Combattant] -> Bool
prop_pre_set_combattant (Environnement joueurs mapp unites bats) coordUsine player listCombattant = 
    let res = (M.lookup (getBatIdProprioCoord bats coordUsine) bats)
    in 
        (isJust res) &&
        ((get_nom_batiment (May.fromJust res)) == "Usine") &&
        (bproprio (May.fromJust res) == jid player) && 
        (prop_inv_Environnement (Environnement joueurs mapp unites bats)) && 
        (elem player joueurs) &&
        ((get_player_credit player) >= prixCombattant) &&
        (List.all prop_inv_Combattant listCombattant)

set_combattant :: Environnement -> Coord -> Joueur -> [Combattant] -> (Environnement, [Combattant])
set_combattant (Environnement joueurs mapp unites bats) coordUsine player listCombattant = 
    let (C x y) = coordUsine 
    in 
        ((Environnement (reduction_credit_player joueurs player prixCombattant) mapp (M.insert (UniteId (M.size unites)) (add_conbattant_collection (UniteId (M.size unites)) (Unite "Combattant" (C x (y+1)) (jid player)) listCombattant) unites) bats), (listCombattant ++ [(Combattant (UniteId (M.size unites)) (Unite "Combattant" (C x (y+1)) (jid player)) pvCombattantMax [] Rien)])) 

prop_post_set_combattant :: Environnement -> Coord -> Joueur -> [Combattant] -> Bool
prop_post_set_combattant (Environnement joueursAvant mappAvant unitesAvant batsAvant) coordUsine player listCombattant =
    let ((Environnement joueursApres mappApres unitesApres batsApres), listCombattantApres) = set_combattant (Environnement joueursAvant mappAvant unitesAvant batsAvant) coordUsine player listCombattant
    in ((List.filter (\(Joueur _ idAvant _) -> idAvant /= jid player) joueursAvant) == (List.filter (\(Joueur _ idApres _) -> idApres /= jid player) joueursApres)) &&
        (get_player_credit (List.head (List.filter (\(Joueur _ id _) -> id == jid player) joueursApres))) == ((get_player_credit player) - prixCombattant) &&
        (mappAvant == mappApres) && (batsAvant == batsApres) && 
        unom (May.fromJust (M.lookup (get_uniteID_Combattant (List.head (listCombattantApres \\ listCombattant))) unitesApres)) == "Combattant" &&
        ((M.delete (get_uniteID_Combattant (List.head (listCombattantApres \\ listCombattant))) unitesApres) == unitesAvant)

------------------------------------------------------------ Etape -----------------------------------------------------------------------
-- data Environnement = Environnement {joueurs :: [Joueur], ecarte :: Carte, unites :: M.Map UniteId Unite, batiments :: M.Map BatId Batiment}
-- data Batiment = Batiment {bNom :: String, prix :: Int, batCoord :: Coord, batProprio :: JoueurId}
-- data Unite = Unite {uNom :: UniteId, unitCoord :: Coord, unitProprio :: JoueurId} deriving (Eq, Show)
-- data Collecteur = Collecteur {uniteIDCollecteur :: UniteId, uniteCollecteur :: Unite, cuve :: Cuve, pvCollecteur :: Int, ordresCollecteur :: [Ordre], butCollecteur :: Ordre } deriving (Show, Eq, Ord) -- A voir deriving
-- data Combattant = Combattant {uniteIDCombattant :: UniteId, uniteCombatant :: Unite, pvCombattant :: Int, ordresCombattant :: [Ordre], butCombattant :: Ordre } deriving (Show, Eq, Ord) -- A voir deriving

-- etape_collecteurs :: [Collecteur] ->  M.Map UniteId Unite 

--filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a

-- Fonction qui eliminer tous les UniteId doont le pv inferieur <= 0 dans Environnement
eliminer_unites_env :: Environnement -> [UniteId] -> Environnement
eliminer_unites_env (Environnement joueurs mapp unites bats) listUniteID = (Environnement joueurs mapp (M.filterWithKey (\k _ -> not (elem k listUniteID)) unites) bats) 

-- Fonction qui retourne la liste d'UniteId d'une liste de Collecteurs qui ont un pv <= 0
get_ListUniteID_Collecteur :: [Collecteur] -> [UniteId]
get_ListUniteID_Collecteur listCollecteurs = fmap (\(Collecteur uniteIDCollecteur _ _ pvCollecteur _ _) -> if pvCollecteur <= 0 then uniteIDCollecteur else (UniteId (-1))) listCollecteurs

-- Fonction qui retourne la liste d'UniteId d'une liste de Combattant qui ont un pv <= 0
get_ListUniteID_Combattant :: [Combattant] -> [UniteId]
get_ListUniteID_Combattant listCombattans = fmap (\(Combattant uniteIDCombattant _ pvCombattant _ _) -> if pvCombattant <= 0 then uniteIDCombattant else (UniteId (-1))) listCombattans

-- Fonction qui verifie la vie (PV) des Combattants/Collecteurs dans la partie (Environnement) et les elimine si morts
verifie_unites :: Environnement -> [Collecteur] -> [Combattant] -> (Environnement, [Collecteur], [Combattant]) 
verifie_unites  env listCollecteur listCombattant = 
    let listeEliminer = (get_ListUniteID_Collecteur listCollecteur) ++ (get_ListUniteID_Combattant listCombattant)
    in 
        (eliminer_unites_env env listeEliminer, (List.filter (\(Collecteur uniteIDCollecteur _ _ pvCollecteur _ _) -> pvCollecteur > 0) listCollecteur), (List.filter (\(Combattant uniteIDCombattant _ pvCombattant _ _) -> pvCombattant > 0) listCombattant))

-------------------------------------------------------------------deplacer unite-------------------------------------------------------------------------------------------

-- Fonction qui se deplace selon l'axe Ouest-Est si non deja bloque
go_x ::  Coord -> Coord -> Bool -> Carte -> Maybe Coord
go_x (C x y) (C xObjectif yObjectif) isBlocked (Carte mapp) = 
    if (x-xObjectif) > 0 then (
        if (May.fromJust (M.lookup (C (x-1) y) mapp)) /= Eau then 
            Just (C (x-1) y)
        else (
            if (isBlocked) then Nothing
            else go_y (C x y) (C xObjectif yObjectif) True (Carte mapp))) 
    else (
        if (May.fromJust (M.lookup (C (x+1) y) mapp)) /= Eau then 
            Just (C (x+1) y) 
        else (
            if (isBlocked) then Nothing
            else go_y (C x y) (C xObjectif yObjectif) True (Carte mapp)))

-- Fonction qui se deplace selon l'axe Nord-Sud si non deja bloque
go_y :: Coord -> Coord -> Bool -> Carte -> Maybe Coord
go_y (C x y) (C xObjectif yObjectif) isBlocked (Carte mapp) =
    if (y-yObjectif) > 0 then (
        if (May.fromJust (M.lookup (C x (y-1)) mapp)) /= Eau then  
            Just (C x (y-1)) 
        else (
            if (isBlocked) then Nothing
            else go_x (C x y) (C xObjectif yObjectif) True (Carte mapp))) 
    else (
        if (May.fromJust (M.lookup (C x (y+1)) mapp)) /= Eau then 
            Just (C x (y+1)) 
        else (
            if (isBlocked) then Nothing
            else go_x (C x y) (C xObjectif yObjectif) True (Carte mapp)))
        
-- Fonction qui applique le deplacement d'une etape pour une Unite
deplacer_unite :: Environnement -> Unite -> Coord -> Maybe Unite
deplacer_unite (Environnement joueurs mapp unites bats) (Unite nom (C x y) proprio) (C xObjectif yObjectif) = 
    if (abs (x-xObjectif)) > (abs (y-yObjectif)) then (
        case (go_x (C x y) (C xObjectif yObjectif) False mapp) of 
            Just coord -> Just (Unite nom coord proprio)
            Nothing -> Nothing  
    )
    else (
        case (go_y (C x y) (C xObjectif yObjectif) False mapp) of 
            Just coord -> Just (Unite nom coord proprio)
            Nothing -> Nothing  
    )  
----------------------------------------------------------------deplacer collecteur------------------------------------------------------------------------

-- Fonction qui transforme une listede Collecteur en une liste de UniteID de ces Collecteurs
listCollecteur_to_listUniteID :: [Collecteur] -> [UniteId]
listCollecteur_to_listUniteID listeCollecteur = fmap (\(Collecteur uniteIDCollecteur _ _ _ _ _) -> uniteIDCollecteur) listeCollecteur

-- Fonction qui transforme une listede Collecteur en une liste de Unite de ces Collecteurs
listCollecteur_to_listUnite :: [Collecteur] -> [Unite]
listCollecteur_to_listUnite listeCollecteur = fmap (\(Collecteur _ unite _ _ _ _) -> unite) listeCollecteur

deplacer_Collecteur_coord_aux :: Environnement -> Collecteur -> Collecteur
deplacer_Collecteur_coord_aux env (Collecteur uniteIDCollecteur uniteCollecteur cuve pvCollecteur ordresCollecteur butCollecteur) = 
    case butCollecteur of 
        Deplacer coord ->
            if (coord /= (ucoord uniteCollecteur)) then (
                let uniteDep = (deplacer_unite env uniteCollecteur coord) 
                in 
                    if not (isJust uniteDep) then (Collecteur uniteIDCollecteur uniteCollecteur cuve pvCollecteur [] Rien)
                    else (Collecteur uniteIDCollecteur (May.fromJust (deplacer_unite env uniteCollecteur coord)) cuve pvCollecteur ordresCollecteur butCollecteur) 
            )
            else (
                if ((length ordresCollecteur) == 0) then (Collecteur uniteIDCollecteur uniteCollecteur cuve pvCollecteur ordresCollecteur Rien)
                else (Collecteur uniteIDCollecteur uniteCollecteur cuve pvCollecteur (drop 1 ordresCollecteur) (head ordresCollecteur))
            )
        otherwise -> (Collecteur uniteIDCollecteur uniteCollecteur cuve pvCollecteur ordresCollecteur butCollecteur)

-- Fonction qui va deplacer un Collecteur d'une case selon l'axe qu'il est le plus loin - version Collecteur 
deplacer_Collecteur_coord :: Environnement -> [Collecteur] -> ([Collecteur], M.Map UniteId Unite)
deplacer_Collecteur_coord env listCollecteurs = 
    let resListCollecteur = fmap (deplacer_Collecteur_coord_aux env) listCollecteurs   
    in (resListCollecteur, (M.fromList (List.zip (listCollecteur_to_listUniteID resListCollecteur) (listCollecteur_to_listUnite resListCollecteur))))
        
    
---------------------------------------------------------deplacer combattants--------------------------------------------------------------------------------------------------------------------------

-- Fonction qui transforme une listede Combattant en une liste de UniteID de ces Combattant
listCombattant_to_listUniteID :: [Combattant] -> [UniteId]
listCombattant_to_listUniteID listeCombattant = fmap (\(Combattant uniteIDCombattant _ _ _ _) -> uniteIDCombattant) listeCombattant

-- Fonction qui transforme une listede Combattant en une liste de Unite de ces Combattant
listCombattant_to_listUnite :: [Combattant] -> [Unite]
listCombattant_to_listUnite listeCombattant = fmap (\(Combattant _ unite _ _ _) -> unite) listeCombattant

deplacer_Combattant_coord_aux :: Environnement -> Combattant -> Combattant
deplacer_Combattant_coord_aux env (Combattant uniteIDCombattant uniteCombattant pvCombattant ordresCombattant butCombattant) = 
    case butCombattant of
            Deplacer coord -> if (coord /= (ucoord uniteCombattant)) then (
                                let uniteDep = (deplacer_unite env uniteCombattant coord) 
                                in 
                                    if not (isJust uniteDep) then Combattant uniteIDCombattant uniteCombattant pvCombattant [] Rien 
                                    else (Combattant uniteIDCombattant (May.fromJust (deplacer_unite env uniteCombattant coord)) pvCombattant ordresCombattant butCombattant)
                                ) 
                              else (
                                if ((length ordresCombattant) == 0) then (Combattant uniteIDCombattant uniteCombattant pvCombattant ordresCombattant Rien)
                                else (Combattant uniteIDCombattant uniteCombattant pvCombattant (drop 1 ordresCombattant) (head ordresCombattant))
                              )
            otherwise -> (Combattant uniteIDCombattant uniteCombattant pvCombattant ordresCombattant butCombattant)

-- Fonction qui va deplacer un Combattant d'une case selon l'axe qu'il est le plus loin - version Combattant 
deplacer_Combattant_coord :: Environnement -> [Combattant] -> ([Combattant], M.Map UniteId Unite)
deplacer_Combattant_coord env listCombattant = 
    let resListCombattant = fmap (deplacer_Combattant_coord_aux env) listCombattant   
    in (resListCombattant, (M.fromList (List.zip (listCombattant_to_listUniteID resListCombattant) (listCombattant_to_listUnite resListCombattant))))
        

-- Fonction qui va deplacer les Unites lors d'une etape si but est de deplacer
deplacer_Unite_Cood :: Environnement -> [Collecteur] -> [Combattant] -> (Environnement, [Collecteur], [Combattant])
deplacer_Unite_Cood (Environnement joueurs mapp unites bats) listCollecteurs listeCombattant =
        let (resListCollecteur, resMapCollecteurs) = deplacer_Collecteur_coord (Environnement joueurs mapp unites bats) listCollecteurs in
            let (resListCombattant, resMapCombattant) = deplacer_Combattant_coord (Environnement joueurs mapp unites bats) listeCombattant in
                ((Environnement joueurs mapp (M.union resMapCollecteurs resMapCombattant) bats), resListCollecteur, resListCombattant)


-- faire prop pre etape 

etape :: Environnement -> [Collecteur] -> [Combattant] -> (Environnement, [Collecteur], [Combattant])
etape env listCollecteurs listeCombattants  =  
    deplacer_Unite_Cood env listCollecteurs listeCombattants





someFunc :: IO ()
someFunc = putStrLn "someFunc"

