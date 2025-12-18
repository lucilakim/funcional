import PdePreludat
import Test.Hspec
--import qualified RecuHolidaysSpec
import qualified RecuAventurerosSpec


main :: IO ()
main = hspec $ do
  --RecuHolidaysSpec.spec
  --RecuAventurerosSpec.spec
  RecuMagosSpec.spec