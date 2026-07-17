{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StackSetSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import qualified TXMonad.StackSet as W

-- | StackSet type alias for tests: tag=String, layout=(), window=Int, screen=Int, detail=()
type TestSS = W.StackSet String () Int Int ()

-- | Helper: get current workspace tag through public API
currentTag :: W.StackSet i l a s sd -> i
currentTag = W.tag . W.workspace . W.current

-- | Helper: get hidden workspace tags
hiddenTags :: W.StackSet i l a s sd -> [i]
hiddenTags = map W.tag . W.hidden

spec :: Spec
spec = do
  describe "new" $ do
    it "creates an empty StackSet with no windows" $ do
      let s = W.new () ["a", "b", "c"] [()] :: TestSS
      W.peek s `shouldBe` Nothing
      W.allWindows s `shouldBe` []

    it "sets up the correct number of screens and hidden workspaces" $ do
      let s = W.new () ["a", "b", "c"] [(), ()] :: TestSS
      length (W.screens s) `shouldBe` 2
      length (W.hidden s) `shouldBe` 1

    it "first workspace tag is current" $ do
      let s = W.new () ["a", "b", "c"] [()] :: TestSS
      currentTag s `shouldBe` "a"

  describe "insertUp" $ do
    it "makes the inserted window the focus" $ do
      let s = W.insertUp 1 (W.new () ["a"] [()] :: TestSS)
      W.peek s `shouldBe` Just 1

    it "inserts multiple windows with last-inserted as focus" $ do
      let s = foldl (flip W.insertUp) (W.new () ["a"] [()] :: TestSS) [1, 2, 3]
      W.peek s `shouldBe` Just 3

    it "does not duplicate existing windows" $ do
      let s0 = W.new () ["a"] [()] :: TestSS
          s1 = W.insertUp 1 s0
          s2 = W.insertUp 1 s1
      W.allWindows s1 `shouldBe` W.allWindows s2

  describe "delete" $ do
    it "removes the current focus window" $ do
      let s0 = W.insertUp 1 (W.new () ["a"] [()] :: TestSS)
          s1 = W.delete 1 s0
      W.peek s1 `shouldBe` Nothing
      W.allWindows s1 `shouldBe` []

    it "shifts focus to remaining window after delete" $ do
      let s0 = foldr W.insertUp (W.new () ["a"] [()] :: TestSS) [1, 2]
          -- Stack: focus=2, down=[1] (insertUp 1 then insertUp 2)
          s1 = W.delete 2 s0
      W.peek s1 `shouldBe` Just 1

    it "is safe to delete a non-existent window" $ do
      let s = W.new () ["a"] [()] :: TestSS
      W.allWindows (W.delete 999 s) `shouldBe` []

  describe "view" $ do
    it "switches to a hidden workspace" $ do
      let s = W.new () ["a", "b", "c"] [()] :: TestSS
      currentTag (W.view "b" s) `shouldBe` "b"

    it "is identity when viewing the current workspace" $ do
      let s = W.new () ["a", "b", "c"] [()] :: TestSS
      W.view "a" s `shouldBe` s

    it "is identity for a non-existent workspace tag" $ do
      let s = W.new () ["a", "b", "c"] [()] :: TestSS
      W.view "zzz" s `shouldBe` s

  describe "greedyView" $ do
    it "behaves like view for hidden workspaces" $ do
      let s = W.new () ["a", "b", "c"] [()] :: TestSS
      currentTag (W.greedyView "b" s) `shouldBe` "b"

    it "is identity when viewing the current workspace" $ do
      let s = W.new () ["a", "b", "c"] [()] :: TestSS
      W.greedyView "a" s `shouldBe` s

  describe "focusUp / focusDown" $ do
    it "focusUp wraps around for a single-element stack" $ do
      let s = W.insertUp 1 (W.new () ["a"] [()] :: TestSS)
      W.peek (W.focusUp s) `shouldBe` Just 1

    it "focusDown wraps around for a single-element stack" $ do
      let s = W.insertUp 1 (W.new () ["a"] [()] :: TestSS)
      W.peek (W.focusDown s) `shouldBe` Just 1

    it "moves focus through the stack" $ do
      let s0 = foldl (flip W.insertUp) (W.new () ["a"] [()] :: TestSS) [1, 2, 3]
      -- Stack after insertUp 1,2,3: focus=3, up=[], down=[2,1]
      W.peek s0 `shouldBe` Just 3
      -- focusDown moves to 2 (next in down list)
      W.peek (W.focusDown s0) `shouldBe` Just 2

  describe "swapMaster" $ do
    it "is identity when already master (single window)" $ do
      let s = W.insertUp 1 (W.new () ["a"] [()] :: TestSS)
      W.allWindows (W.swapMaster s) `shouldBe` [1]

    it "makes focused window the master" $ do
      let s0 = foldr W.insertUp (W.new () ["a"] [()] :: TestSS) [1, 2, 3]
          s1 = W.focusDown s0
          s2 = W.swapMaster s1
      W.peek s2 `shouldBe` Just 2
      head (W.integrate' (W.stack (W.workspace (W.current s2)))) `shouldBe` 2

  describe "allWindows" $ do
    it "returns empty list for a new StackSet" $ do
      let s = W.new () ["a", "b"] [()] :: TestSS
      W.allWindows s `shouldBe` []

    it "returns all inserted windows" $ do
      let s = foldr W.insertUp (W.new () ["a"] [()] :: TestSS) [1, 2, 3]
      length (W.allWindows s) `shouldBe` 3

  describe "shift" $ do
    it "moves the focused window to the target workspace" $ do
      let s0 = foldl (flip W.insertUp) (W.new () ["a", "b"] [()] :: TestSS) [1]
          s1 = W.shift "b" s0
      -- Current workspace "a" should now be empty
      let curStack = W.stack (W.workspace (W.current s1))
      W.integrate' curStack `shouldBe` ([] :: [Int])

  describe "integrate / integrate'" $ do
    it "integrate' returns empty list for Nothing" $ do
      W.integrate' (Nothing :: Maybe (W.Stack Int)) `shouldBe` []

    it "integrate returns focus : down when up is empty" $ do
      W.integrate (W.Stack 1 [] [2, 3]) `shouldBe` [1, 2, 3]

    it "integrate reverses up then focus then down" $ do
      W.integrate (W.Stack 2 [1] [3]) `shouldBe` [1, 2, 3]

  describe "lookupWorkspace" $ do
    it "returns the tag of the workspace on the given screen" $ do
      let s = W.new () ["a", "b"] [(), ()] :: TestSS
      W.lookupWorkspace (0 :: Int) s `shouldBe` Just "a"
      W.lookupWorkspace (1 :: Int) s `shouldBe` Just "b"

    it "returns Nothing for non-existent screen" $ do
      let s = W.new () ["a"] [()] :: TestSS
      W.lookupWorkspace (5 :: Int) s `shouldBe` Nothing

  -- =====================================================================
  -- QuickCheck Property Tests
  -- =====================================================================

  describe "QuickCheck properties" $ do

    it "view is idempotent" $ do
      property $ \i (SS s) ->
        W.view i (W.view i s) == (W.view i s :: TestSS)

    it "insertUp then delete restores the original StackSet" $ do
      property $ \w (SS s) ->
        not (w `elem` W.allWindows s) ==>
          W.delete w (W.insertUp w s) == (s :: TestSS)

    it "focusUp cycled n times on n windows returns to original focus" $ do
      property $ \(Positive n) ->
        let ws = take n [1 ..]
            s  = foldr W.insertUp (W.new () ["a"] [()] :: TestSS) ws
        in W.peek (applyN n W.focusUp s) == W.peek s

    it "focusDown cycled n times on n windows returns to original focus" $ do
      property $ \(Positive n) ->
        let ws = take n [1 ..]
            s  = foldr W.insertUp (W.new () ["a"] [()] :: TestSS) ws
        in W.peek (applyN n W.focusDown s) == W.peek s

    it "shift moves window to target workspace" $ do
      property $ \(SS s) ->
        case W.peek s of
          Nothing -> True
          Just w  -> case hiddenTags s of
            []      -> True
            (i : _) ->
              let s' = W.shift i s
                  targetStack = W.stack (W.workspace (W.current (W.view i s')))
              in w `elem` W.integrate' targetStack

    it "allWindows length increases by 1 after insertUp" $ do
      property $ \w (SS s) ->
        not (w `elem` W.allWindows s) ==>
          length (W.allWindows (W.insertUp w s :: TestSS)) == length (W.allWindows s) + 1

    it "integrate' . Just == integrate" $ do
      property $ \(NonEmpty xs) ->
        let (f : rest) = xs :: [Int]
            (upList, downList) = splitAt (length rest `div` 2) rest
            st = W.Stack f upList downList
        in W.integrate' (Just st) == W.integrate st

    it "swapMaster makes focused window first in integrate order" $ do
      property $ \(SS s) ->
        case W.peek s of
          Nothing -> True
          Just w  ->
            let s'  = W.swapMaster s
                ws' = W.integrate' (W.stack (W.workspace (W.current s')))
            in not (null ws') && head ws' == w

    it "insertUp is idempotent for existing windows" $ do
      property $ \(SS s) ->
        case W.peek s of
          Nothing -> True
          Just w  -> W.insertUp w s == (s :: TestSS)

-- | Apply a function n times
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

-- =====================================================================
-- Arbitrary instances
-- =====================================================================

-- | Newtype wrapper for generating arbitrary non-empty StackSets
newtype SS = SS TestSS deriving (Show)

instance Arbitrary SS where
  arbitrary = do
    nTags    <- choose (1, 5)
    let tags = map show ([1 .. nTags] :: [Int])
    nScreens <- choose (1, min 3 nTags)
    let sds = replicate nScreens ()
    let s0  = W.new () tags sds :: TestSS
    nWins    <- choose (0, 10)
    wins     <- vectorOf nWins (choose (1, 100) :: Gen Int)
    return $ SS (foldr W.insertUp s0 wins)

instance Arbitrary (W.Stack Int) where
  arbitrary = do
    f     <- arbitrary
    upL   <- listOf arbitrary
    downL <- listOf arbitrary
    return $ W.Stack f upL downL
