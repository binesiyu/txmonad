-- | txmonad 库的顶层重导出模块。
--
-- 本模块汇聚了 'TXMonad.Main'、'TXMonad.Core' 和 'TXMonad.Config'
-- 的公开 API，方便统一导入。
module TXMonad
  ( module TXMonad.Main
  , module TXMonad.Core
  , module TXMonad.Config
  )
where

import           TXMonad.Config
import           TXMonad.Core
import           TXMonad.Main
