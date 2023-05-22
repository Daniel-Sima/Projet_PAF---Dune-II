import Test.Hspec
import CarteTest as CT
import EnvironementTest as ET
import BatTest as BT
import UniteTest as UT
import Lib

main :: IO ()
main = hspec $ do
  CT.engineSpec 
  ET.engineSpec  
  BT.engineSpec
  UT.engineSpec