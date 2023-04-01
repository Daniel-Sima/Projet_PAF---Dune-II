import Test.Hspec
import CarteTest as CT
import EnvironementTest as ET
import Lib

main :: IO ()
main = hspec $ do
  --CT.engineSpec -- a voir 
  ET.engineSpec -- a voir 