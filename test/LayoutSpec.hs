{-# LANGUAGE FlexibleInstances #-}

module LayoutSpec (spec) where

import           Test.Hspec
import           Data.List                      ( isPrefixOf )

import           TXMonad.Core                   ( Rectangle(..)
                                                , TX
                                                , runTX
                                                , LayoutClass(..)
                                                , SomeMessage(..)
                                                )
import           TXMonad.Layout
import qualified TXMonad.StackSet               as W

-- =====================================================================
-- Helpers
-- =====================================================================

-- | Run a TX computation with dummy state and config (layouts don't access them)
runTX' :: TX a -> IO a
runTX' tx = fmap fst $ runTX undefined undefined tx

-- | Screen rectangle for tests
testRect :: Rectangle
testRect = Rectangle 0 0 1000 800

-- =====================================================================
-- Tests
-- =====================================================================

spec :: Spec
spec = do
  describe "Full layout" $ do
    it "single window covers the entire screen" $ do
      let ws = W.Workspace "1" Full (Just (W.Stack "w1" [] []))
      (result, _) <- runTX' $ runLayout ws testRect
      result `shouldBe` [("w1", testRect)]

    it "multiple windows: only focused window is placed (fullscreen)" $ do
      let ws = W.Workspace "1" Full (Just (W.Stack "w1" [] ["w2", "w3"]))
      (result, _) <- runTX' $ runLayout ws testRect
      result `shouldBe` [("w1", testRect)]

    it "empty workspace returns no placements" $ do
      let ws = W.Workspace "1" Full (Nothing :: Maybe (W.Stack String))
      (result, _) <- runTX' $ runLayout ws testRect
      result `shouldBe` []

    it "description is 'Full'" $ do
      description Full `shouldBe` "Full"

  describe "Tall layout" $ do
    it "single window covers entire screen" $ do
      let tall = Tall 1 (3 / 100) (1 / 2)
          ws   = W.Workspace "1" tall (Just (W.Stack "w1" [] []))
      (result, _) <- runTX' $ runLayout ws testRect
      result `shouldBe` [("w1", testRect)]

    it "two windows split screen horizontally (master left)" $ do
      let tall = Tall 1 (3 / 100) (1 / 2)
          ws   = W.Workspace "1" tall (Just (W.Stack "w1" [] ["w2"]))
      (result, _) <- runTX' $ runLayout ws testRect
      length result `shouldBe` 2
      let (w1Name, r1) = result !! 0
          (w2Name, r2) = result !! 1
      w1Name `shouldBe` "w1"
      w2Name `shouldBe` "w2"
      -- Master takes left half (ratio 1/2)
      x r1 `shouldBe` 0
      width r1 `shouldBe` 500
      height r1 `shouldBe` 800
      x r2 `shouldBe` 500
      width r2 `shouldBe` 500
      height r2 `shouldBe` 800

    it "three windows: master left, two stacked right" $ do
      let tall = Tall 1 (3 / 100) (1 / 2)
          ws   = W.Workspace "1" tall (Just (W.Stack "w1" [] ["w2", "w3"]))
      (result, _) <- runTX' $ runLayout ws testRect
      length result `shouldBe` 3
      let (_, r1) = result !! 0
          (_, r2) = result !! 1
          (_, r3) = result !! 2
      -- Master: left half full height
      width r1 `shouldBe` 500
      height r1 `shouldBe` 800
      -- Slave windows: right half, stacked vertically
      x r2 `shouldBe` 500
      height r2 `shouldBe` 400
      x r3 `shouldBe` 500
      y r3 `shouldBe` 400

    it "two master windows: left half split vertically" $ do
      let tall = Tall 2 (3 / 100) (1 / 2)
          ws   = W.Workspace "1" tall (Just (W.Stack "w1" [] ["w2", "w3"]))
      (result, _) <- runTX' $ runLayout ws testRect
      length result `shouldBe` 3
      let (_, r1) = result !! 0
          (_, r2) = result !! 1
          (_, r3) = result !! 2
      -- Two masters in left half, stacked vertically
      x r1 `shouldBe` 0
      width r1 `shouldBe` 500
      height r1 `shouldBe` 400
      x r2 `shouldBe` 0
      y r2 `shouldBe` 400
      height r2 `shouldBe` 400
      -- One slave in right half
      x r3 `shouldBe` 500
      width r3 `shouldBe` 500
      height r3 `shouldBe` 800

  describe "Mirror Tall layout" $ do
    it "single window still covers entire screen" $ do
      let mirrorTall = Mirror (Tall 1 (3 / 100) (1 / 2))
          ws         = W.Workspace "1" mirrorTall (Just (W.Stack "w1" [] []))
      (result, _) <- runTX' $ runLayout ws testRect
      result `shouldBe` [("w1", testRect)]

    it "two windows: master on top, slave on bottom (vertical split)" $ do
      let mirrorTall = Mirror (Tall 1 (3 / 100) (1 / 2))
          ws         = W.Workspace "1" mirrorTall (Just (W.Stack "w1" [] ["w2"]))
      (result, _) <- runTX' $ runLayout ws testRect
      length result `shouldBe` 2
      let (_, r1) = result !! 0
          (_, r2) = result !! 1
      -- Mirror swaps x/y and w/h, so master is top half, slave is bottom half
      -- Tall on mirrored rect (800x1000): r1=(0,0,400,1000) r2=(400,0,400,1000)
      -- After mirrorRect back: r1'=(0,0,1000,400) r2'=(0,400,1000,400)
      y r1 `shouldBe` 0
      width r1 `shouldBe` 1000
      height r1 `shouldBe` 400
      y r2 `shouldBe` 400
      width r2 `shouldBe` 1000
      height r2 `shouldBe` 400

    it "description starts with 'Mirror'" $ do
      let mirrorTall = Mirror (Tall 1 (3 / 100) (1 / 2))
      take 6 (description mirrorTall) `shouldBe` "Mirror"

    it "total area equals screen area with multiple windows" $ do
      let mirrorTall = Mirror (Tall 1 (3 / 100) (1 / 2))
          ws = W.Workspace "1" mirrorTall (Just (W.Stack "w1" [] ["w2", "w3"]))
      (result, _) <- runTX' $ runLayout ws testRect
      let totalArea = sum [width r * height r | (_, r) <- result]
      totalArea `shouldBe` (width testRect * height testRect)

  describe "Choose (|||) layout" $ do
    it "defaults to left layout (Full)" $ do
      let c  = Full ||| Tall 1 (3 / 100) (1 / 2)
          ws = W.Workspace "1" c (Just (W.Stack "w1" [] []))
      (result, _) <- runTX' $ runLayout ws testRect
      -- Full layout: single window covers entire screen
      result `shouldBe` [("w1", testRect)]

    it "left side is active initially" $ do
      let c = Full ||| Tall 1 (3 / 100) (1 / 2)
      description c `shouldBe` "Full"

    it "can switch to right side via NextLayout" $ do
      let c = Full ||| Tall 1 (3 / 100) (1 / 2)
      Just c' <- runTX' $ handleMessage c (SomeMessage NextLayout)
      ("Tall" `isPrefixOf` description c') `shouldBe` True
      -- Verify runLayout now uses Tall (2 windows split)
      let ws = W.Workspace "1" c' (Just (W.Stack "w1" [] ["w2"]))
      (result, _) <- runTX' $ runLayout ws testRect
      length result `shouldBe` 2
      let (_, r1) = result !! 0
          (_, r2) = result !! 1
      -- Tall: master left, slave right
      width r1 `shouldBe` 500
      width r2 `shouldBe` 500

    it "FirstLayout switches back to left side" $ do
      let c = Full ||| Tall 1 (3 / 100) (1 / 2)
      -- First switch to right (Tall)
      Just c' <- runTX' $ handleMessage c (SomeMessage NextLayout)
      ("Tall" `isPrefixOf` description c') `shouldBe` True
      -- Then back to first (Full)
      Just c'' <- runTX' $ handleMessage c' (SomeMessage FirstLayout)
      description c'' `shouldBe` "Full"
      -- Verify runLayout now uses Full (single window fullscreen)
      let ws = W.Workspace "1" c'' (Just (W.Stack "w1" [] ["w2"]))
      (result, _) <- runTX' $ runLayout ws testRect
      result `shouldBe` [("w1", testRect)]

  describe "Layout messages" $ do
    it "Shrink decreases Tall master ratio" $ do
      let tall    = Tall 1 (3 / 100) (1 / 2)
          Just t' = pureMessage tall (SomeMessage Shrink)
      tallRatio t' `shouldBe` (1 / 2 - 3 / 100)

    it "Expand increases Tall master ratio" $ do
      let tall    = Tall 1 (3 / 100) (1 / 2)
          Just t' = pureMessage tall (SomeMessage Expand)
      tallRatio t' `shouldBe` (1 / 2 + 3 / 100)

    it "Shrink does not go below 0" $ do
      let tall    = Tall 1 (3 / 100) 0
          Just t' = pureMessage tall (SomeMessage Shrink)
      tallRatio t' `shouldBe` 0

    it "Expand does not go above 1" $ do
      let tall    = Tall 1 (3 / 100) 1
          Just t' = pureMessage tall (SomeMessage Expand)
      tallRatio t' `shouldBe` 1

    it "multiple Expand messages accumulate ratio" $ do
      let tall0 = Tall 1 (1 / 10) (1 / 2)
          Just tall1 = pureMessage tall0 (SomeMessage Expand)
          Just tall2 = pureMessage tall1 (SomeMessage Expand)
      tallRatio tall2 `shouldBe` (1 / 2 + 2 / 10)

    it "IncMasterN increases master count" $ do
      let tall    = Tall 1 (3 / 100) (1 / 2)
          Just t' = pureMessage tall (SomeMessage (IncMasterN 1))
      tallNMaster t' `shouldBe` 2

    it "IncMasterN with negative value decreases master count" $ do
      let tall    = Tall 2 (3 / 100) (1 / 2)
          Just t' = pureMessage tall (SomeMessage (IncMasterN (-1)))
      tallNMaster t' `shouldBe` 1

    it "IncMasterN does not go below 0" $ do
      let tall    = Tall 0 (3 / 100) (1 / 2)
          Just t' = pureMessage tall (SomeMessage (IncMasterN (-1)))
      tallNMaster t' `shouldBe` 0

    it "Shrink/Expand does not change nmaster" $ do
      let tall = Tall 2 (3 / 100) (1 / 2)
          Just t' = pureMessage tall (SomeMessage Shrink)
      tallNMaster t' `shouldBe` 2

    it "IncMasterN does not change ratio" $ do
      let tall = Tall 1 (3 / 100) (1 / 2)
          Just t' = pureMessage tall (SomeMessage (IncMasterN 3))
      tallRatio t' `shouldBe` (1 / 2)
