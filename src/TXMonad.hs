-- | txmonad 库的顶层重导出模块。
--
-- 本模块汇聚了各子模块的公开 API，方便统一导入。
-- 用户只需 @import TXMonad@ 即可获取全部常用类型与函数。
module TXMonad
  ( -- * 入口函数
    module TXMonad.Main
    -- * 核心类型与抽象
  , module TXMonad.Core
    -- * 默认配置
  , module TXMonad.Config
    -- * 布局类型与消息
  , module TXMonad.Layout
  )
where

import           TXMonad.Config
import           TXMonad.Core                   hiding ( handleEventHook
                                                , keys
                                                , screenEventHook
                                                , workspaces
                                                )
import           TXMonad.Layout
import           TXMonad.Main
