module RecuAventurerosSpec (spec) where

import PdePreludat
import Test.Hspec
import RecuAventureros   -- tu solución (o Library si ahí está doble)

spec :: Spec
spec = do
  describe "Recu Aventureros" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2