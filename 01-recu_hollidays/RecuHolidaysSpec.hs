module RecuHolidaysSpec (spec) where

import PdePreludat
import Test.Hspec
import RecuHolidays   -- tu solución (o Library si ahí está doble)

spec :: Spec
spec = do
  describe "Recu Holidays" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2