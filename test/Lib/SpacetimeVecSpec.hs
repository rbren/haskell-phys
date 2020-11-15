module Lib.SpacetimeVecSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit.Approx

import Lib.SpacetimeVec
import Lib.SpaceVec

shouldBeAbout :: Double -> Double -> Expectation
shouldBeAbout a b = do
  let fracDiff = if a == b
                   then 0.0
                 else if a == 0.0
                   then b
                 else if b == 0.0
                   then a
                 else abs (1.0 - b / a)
  let isInBounds = fracDiff < 0.001
  --print ("test " ++ show a ++ ", " ++ show b ++ ", " ++ show fracDiff ++ ", " ++ show isInBounds)
  isInBounds `shouldBe` True

spec :: Spec
spec = do
  describe "SpacetimeVec" $ do
    it "can find collocated reference frame" $ do
      let displacement = SpacetimeVec 1.0 (SpaceVec 1 1 1)
      let collocSpeed = findCollocatedVelocity displacement
      let colloc = transformCoordinates displacement collocSpeed
      let collocLoc = r colloc
      print ("colloc " ++ show colloc)
      (x collocLoc) `shouldBeAbout` (0.0)
      (y collocLoc) `shouldBeAbout` (0.0)
      (z collocLoc) `shouldBeAbout` (0.0)

    it "can find cotemporaneous reference frame" $ do
      let displacement = SpacetimeVec 1.0 (SpaceVec 10000000000 0.0 0.0)
      let cotempSpeed = findCotemporaneousVelocity displacement
      let cotemp = transformCoordinates displacement cotempSpeed
      print ("cotemp " ++ show cotemp)
      (t cotemp) `shouldBeAbout` (0.0)

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
      (t measAToB) `shouldBeAbout` (-7.73333 * 60)
      (t measBToA) `shouldBeAbout` (14.4 * 60)

    it "transforms space and time (problem 4 inverted)" $ do
      let evtRMeas = SpacetimeVec 0.0 (SpaceVec 1210 0 0)
      let evtBMeas = SpacetimeVec 4.96e-6 (SpaceVec 480 0 0)
      let diff = evtBMeas $-$ evtRMeas
      let collocVel = findCollocatedVelocity diff
      (x collocVel) `shouldBeAbout` (-1.4717e8) -- answer to part 1

      let colloc = transformCoordinates diff collocVel
      print ("vel " ++ show collocVel)
      print ("displacement " ++ show diff)
      let stDist = diff $.$ diff
      print ("displacement dist " ++ show stDist)
      print ("transformed " ++ show colloc)
      let clDist = colloc $.$ colloc
      print ("transformed dist " ++ show clDist)
      (x (r colloc)) `shouldBeAbout` 0.0
      y (r colloc) `shouldBe` 0.0
      z (r colloc) `shouldBe` 0.0
      (t colloc) `shouldBeAbout` 4.32e-6

    it "transforms length (problem 5 inverted)" $ do
      let speed = -0.8 * speedOfLight
      let rocketVel = SpaceVec speed 0 0
      let rocketLengthObsAtSpeed = SpacetimeVec 0.0 (SpaceVec 0.6 0 0)
      let rocketLengthRest = transformCoordinates rocketLengthObsAtSpeed rocketVel
      x (r rocketLengthRest) `shouldBe` 1.0
