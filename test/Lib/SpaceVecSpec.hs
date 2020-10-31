module Lib.SpaceVecSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Lib.SpaceVec

spec :: Spec
spec = do
  describe "SpaceVec" $ do
    it "has zero vec" $ do
      x zeroSV `shouldBe` 0.0
      y zeroSV `shouldBe` 0.0
      z zeroSV `shouldBe` 0.0

    it "should add properly" $ do
      zeroSV ^+^ zeroSV `shouldBe` zeroSV

      let v1 = SpaceVec 1 2 3
      zeroSV ^+^ v1 `shouldBe` v1
      v1 ^+^ zeroSV `shouldBe` v1
      v1 ^+^ v1 `shouldBe` SpaceVec 2 4 6

    it "should dot product" $ do
      zeroSV ^.^ zeroSV `shouldBe` 0.0

      let v1 = SpaceVec 1 2 3
      (zeroSV ^.^ v1) `shouldBe` (v1 ^.^ zeroSV)
      (zeroSV ^.^ v1) `shouldBe` 0.0
      (v1 ^.^ v1) `shouldBe` 14.0

    it "should have scalar multiplication" $ do
      zeroSV ^* 2.0 `shouldBe` zeroSV
      2.0 *^ zeroSV `shouldBe` zeroSV

      let v1 = SpaceVec 1 2 3
      v1 ^* 2.0 `shouldBe` SpaceVec 2 4 6
      2.0 *^ v1 `shouldBe` SpaceVec 2 4 6
      v1 ^* 0.0 `shouldBe` SpaceVec 0 0 0
      0.0 *^ v1 `shouldBe` SpaceVec 0 0 0

    it "should have magnitude" $ do
      magnitude zeroSV `shouldBe` 0.0
      let v1 = SpaceVec 1 2 3
      magnitude v1 `shouldBe` sqrt 14.0
