module CarteTest where

import qualified Data.Set as Set 

import qualified Data.Sequence as Seq

import qualified Data.Map.Strict as M

import Lib

import Test.Hspec -- a voir

let carte = M.empty
carte = M.insert (0, 0) Herbe carte
   
engineSpec = do 
    Show Carte 
