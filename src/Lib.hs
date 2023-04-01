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
data Batiment = Batiment {bNom :: String, prix :: Int, batCoord :: Coord, batProprio :: JoueurId}

newtype UniteId = UniteId Int deriving (Show, Eq, Ord)
data Unite = Unite {uNom :: String, unitCoord :: Coord, unitProprio :: JoueurId}

data Environnement = Environnement {joueurs :: [Joueur], ecarte :: Carte, unites :: M.Map UniteId Unite, batiments :: M.Map BatId Batiment}

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
prop_inv_Batiment (Batiment bNom prix bcoord bproprio) = ((length bNom) > 0) && (prix > 0)

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

