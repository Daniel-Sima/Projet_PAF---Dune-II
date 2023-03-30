module Lib
    ( someFunc, 
    prop_inv_Terrain,
    Coord(..),
    Terrain(..)
    ) where

import qualified Data.Map.Strict as M

data Coord = C {cx :: Int ,cy :: Int} deriving (Show, Eq, Ord)

data Terrain = Herbe
    | Ressource Int     
    | Eau
    deriving (Show, Eq, Ord)

        
newtype Carte = Carte {carte :: M.Map Coord Terrain } deriving (Show, Eq, Ord) -- A voir 

prop_inv_Terrain :: Terrain -> Bool
prop_inv_Terrain (Ressource val) = val > 0
prop_inv_Terrain _ = True

prop_inv_Carte :: Carte -> Bool
prop_inv_Carte (Carte mapC) = (M.foldr prop_inv_Terrain True mapC) && (M.foldrWithKey funct True (Carte mapC))
                        where 
                            funct k val acc = (M.foldrWithKey funct2 acc (Carte mapC))
                         
                            funct2 k2 val2 acc = 
                               if k == k2 then acc -- a voir si acc && True
                                else (verifie_chemin1 k k2 acc) || (verifie_chemin2 k k2 acc)
                           
                            verifie_chemin1 p1 p2 res = 
                               if (p1 == p2) then res 
                                else let (x1, y1) = p1 in let (x2, y2) = p2 in 
                                    if (y1 \= y2) then case ((M.lookup (x1, y1) mapC)) of 
                                        Just value -> verifie_chemin1 (x1, y1+1) (x2, y2) res
                                        Nothing -> res && False 
                                    else if (x1 \= x2) then case ((M.lookup (x1, y1) mapC)) of 
                                        Just value -> verifie_chemin1 (x1+1, y1) (x2, y2) res
                                        Nothing -> res && False 
                                    else res
                               
                            verifie_chemin2 p1 p2 res = 
                               if (p1 == p2) then res  -- a voir si acc && res
                               else let (x1, y1) = p1 in let (x2, y2) = p2 in 
                                    if (x1 \= x2) then case ((M.lookup (x1, y1) mapC)) of 
                                        Just value -> verifie_chemin2 (x1+1, y1) (x2, y2) res
                                        Nothing -> res && False 
                                    else if (y1 \= y2) then case ((M.lookup (x1, y1) mapC)) of 
                                        Just value -> verifie_chemin2 (x1, y1+1) (x2, y2) res
                                        Nothing -> res && False 
                                        else res 

                             

-- -- prop_inv_Carte2 :: Carte -> Bool
-- -- prop_inv_Carte2 carte = all prop_inv_Terrain (M.elems carte) && all (verifie_chemin carte)  (build_segment carte)
-- --                         where build_segment carte = [(x,y)|x<-(M.keys carte) y<-(M.keys carte) ]
-- --                         where verifie_chemin ((x1,y1),(x2,y2)) = fun 
     
                    
-- collecteCase :: Coord -> Int -> Carte -> (Int, Carte)
-- collecteCase (x, y) r mapC = case M.lookup (x, y) mapC of 
--                                 Just (Ressource n) -> if (n > r) then (r, M.insert (x, y) (Ressource (n-r)) mapC)
--                                                       else (n, M.insert (x, y) Herbe mapC)
--                                 otherwise -> (0, mapC)

-- prop_pre_collecteCase :: Coord -> Int -> Carte -> Bool
-- prop_pre_collecteCase (x, y) r mapC = (isJust (M.lookup (x,y) mapC)) && r > 0


-- get_val_Ressource :: Terrain -> Maybe Int 
-- get_val_Ressource (Ressource val) = Just val
-- get_val_Ressource _ = Nothing

-- prop_post_collecteCase :: Coord -> Int -> Carte  -> Bool
-- prop_post_collecteCase (x, y) r mapC = case collecteCase (x, y) r mapC of 
--                                             (r, mapRes) -> ((M.lookup (x, y) mapRes) == Ressourse ((get_val_Ressource (M.lookup (x, y) mapC)) - r) || 
--                                                          ((M.lookup (x, y) mapRes) == Herbe) && ((get_val_Ressource (M.lookup (x, y) mapC)) == r)) 
--                                                          &&
--                                                          (M.foldrWithKey funct1 True mapC)
--                                                          where
--                                                             funct1 p1 val1 acc =
--                                                                 (M.foldrWithKey funct2 acc mapRes)
--                                                                 where 
--                                                                     funct2 p2 val2 acc =
--                                                                         if (p1 == p2) && (p1 \= (x, y)) then acc && (val1 == val2) 
--                                                                         else acc            
--                                             (0, mapRes) -> mapRes == mapC
--                                             (n, mapRes) ->  ((M.lookup (x, y) mapRes) == Herbe) && (n < r) && (all fun $ zip (M.keys mapRes,M.elems mapRes))
--                                                             where 
--                                                                 fun (coor, t) = 
--                                                                     if (coor == (x,y)) then True 
--                                                                     else (fromJust (M.lookup coor mapC)) == t 

                                             



someFunc :: IO ()
someFunc = putStrLn "someFunc"

