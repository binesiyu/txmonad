-- | txmonad 应用程序入口点
module Main
  ( main
  )
where

import           TXMonad

main :: IO ()
main = txmonad def
