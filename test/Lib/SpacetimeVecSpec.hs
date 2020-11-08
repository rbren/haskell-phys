module Lib.SpacetimeVecSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit.Approx

import Lib.SpacetimeVec
import Lib.SpaceVec

shouldBeAbout :: Double -> Double -> Double -> Expectation
shouldBeAbout a b precision = aRound `shouldBe` bRound
  where
    aRound = fromIntegral (round (a * precision)) / precision
    bRound = fromIntegral (round (b * precision)) / precision

spec :: Spec
spec = do
  describe "SpacetimeVec" $ do
    -- https://oyc.yale.edu/sites/default/files/problem_set_7_solutions_4.pdf
    it "transforms time (problem 1)" $ do
      let dist = 1.493e+11
      let time = 120
      let evtA = SpacetimeVec 0.0 (SpaceVec 0 0 0)
      let evtB = SpacetimeVec time (SpaceVec dist 0 0)
      let diff = evtB $-$ evtA
      let speed = 0.8 * speedOfLight
      let obsMovingAToB = SpaceVec speed 0 0
      let obsMovingBToA = SpaceVec (-speed) 0 0
      let measAToB = transformCoordinates diff obsMovingAToB
      let measBToA = transformCoordinates diff obsMovingBToA
      let beta = speed * speed / (speedOfLight * speedOfLight)
      let gamma = 1 / sqrt (1.0 - beta)
      shouldBeAbout (t measAToB) (-7.73333 * 60) 1
      shouldBeAbout (t measBToA) (14.4 * 60) 1

    it "transforms space and time (problem 4 inverted)" $ do
      let evtRMeas = SpacetimeVec 0.0 (SpaceVec 1210 0 0)
      let evtBMeas = SpacetimeVec 4.96e-6 (SpaceVec 480 0 0)
      let diff = evtBMeas $-$ evtRMeas
      let collocVel = findColocated diff
      shouldBeAbout (x collocVel) (-1.47e8) 1e-6 -- answer to part 1

      let colloc = transformCoordinates diff collocVel
      print ("vel " ++ show collocVel)
      print ("displacement " ++ show diff)
      let stDist = diff $.$ diff
      print ("displacement dist " ++ show stDist)
      print ("transformed " ++ show colloc)
      let clDist = colloc $.$ colloc
      print ("transformed dist " ++ show clDist)
      shouldBeAbout (x (r colloc)) 0.0 1e6
      y (r colloc) `shouldBe` 0.0
      z (r colloc) `shouldBe` 0.0
      shouldBeAbout (t colloc) 4.32e-6 1e8

    it "transforms length (problem 5)" $ do
      let speed = -0.8 * speedOfLight
      let rocketLengthRest = SpacetimeVec 0.0 (SpaceVec 1.0 0 0)
      let rocketVel = SpaceVec speed 0 0
      let rocketLengthObs = transformCoordinates rocketLengthRest rocketVel
      x (r rocketLengthObs) `shouldBe` 0.6

