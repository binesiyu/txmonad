{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | txmonad 默认配置模块。
--
-- 定义了默认工作空间、默认布局栈、默认按键绑定、
-- 事件钩子以及帮助文本，并实现 'Data.Default.Default'
-- 实例。
module TXMonad.Config
  ( Default(..)
  , Event
  )
where

import           TXMonad.Core                  as TXMonad
                                         hiding ( handleEventHook
                                                , keys
                                                , screenEventHook
                                                , workspaces
                                                )

import           Data.Default
import qualified Data.Map                      as M
import           Data.Monoid
import           System.Exit
import qualified TXMonad.Core                  as TXMonad
                                                ( handleEventHook
                                                , keys
                                                , screenEventHook
                                                , workspaces
                                                )
import           TXMonad.Layout
import           TXMonad.Operations
import qualified TXMonad.StackSet              as W

-- | 默认工作空间列表：1 到 9。
workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

-- | 默认事件处理钩子：允许所有事件通过。
--
-- 返回 'All True' 表示事件可继续处理。
handleEventHook :: Event -> TX All
handleEventHook _ = return (All True)

-- | 默认屏幕刷新钩子：大多数事件都触发重绘。
--
-- 按下 "h"（帮助键）时不触发重绘（返回 'All False'），
-- 因为帮助命令会单独清屏。
screenEventHook :: Event -> TX All
screenEventHook "h" = return (All False)
screenEventHook _   = return (All True)

-- | 默认布局栈：@Tall ||| Mirror Tall ||| Full@。
--
-- 初始主区域 1 个窗口，主区域比例 1/2，每次调整 3%。
layout = tiled ||| Mirror tiled ||| Full
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1
  delta   = 3 / 100
  ratio   = 1 / 2

-- | 默认按键绑定映射表。
--
-- 支持以下操作：
--
--   * @a@/@x@：添加/删除窗口
--   * @n@：切换布局
--   * @j@/@k@：焦点下/上移
--   * @sj@/@sk@：交换窗口位置
--   * @,@/@.@：增减主区域窗口数
--   * @h@/@l@：缩放主区域
--   * @m@/@sm@：聚焦/交换 master
--   * @q@：退出
--   * @j[1..9]@：切换工作空间
--   * @sj[1..9]@：移动窗口到工作空间
--   * @j{w,e,r}@：切换屏幕
--   * @sj{w,e,r}@：移动窗口到屏幕
keys :: TXConfig Layout -> M.Map Event (TX ())
keys conf =
  M.fromList
    $  [ ("a" , addWindow)
       , ("x" , deleteWindow)
       , ("n" , sendMessage NextLayout)
       , ("j" , windows W.focusDown)
       , ("k" , windows W.focusUp)
       , ("sj", windows W.swapDown)
       , ("sk", windows W.swapUp)
       , ("," , sendMessage (IncMasterN 1))
       , ("." , sendMessage (IncMasterN (-1)))
       , ("h" , sendMessage Shrink)
       , ("l" , sendMessage Expand)
       , ("m" , windows W.focusMaster)
       , ("sm", windows W.swapMaster)
       , ("h" , helpCommand help)
       , ("q" , io exitSuccess)
       ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
       [ (m ++ show k, windows $ f i)
       | (i, k) <- zip (TXMonad.workspaces conf) [1 .. 9]
       , (f, m) <- [(W.greedyView, "j"), (W.shift, "sj")]
       ]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
       [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip ["w", "e", "r"] [0 ..]
       , (f, m) <- [(W.view, "j"), (W.shift, "sj")]
       ]

-- | 'TXConfig' 的 'Default' 实例：提供 txmonad 的默认运行时配置。
--
-- 包含两个屏幕（80x20 和 40x20）、蓝色普通边框、红色聚焦边框、
-- 以及 Unicode 边框字符。
instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) =>
         Default (TXConfig a) where
  def = TXConfig
    { TXMonad.workspaces         = workspaces
    , TXMonad.layoutHook         = layout
    , TXMonad.keys               = keys
    , TXMonad.handleEventHook    = handleEventHook
    , TXMonad.screenEventHook    = screenEventHook
    , TXMonad.sd = [SD (Rectangle 0 0 80 20), SD (Rectangle 0 0 40 20)]
    , TXMonad.normalBorderColor  = "Blue"
    , TXMonad.focusedBorderColor = "Red"
    , TXMonad.upBorder           = '▄'
    , TXMonad.downBorder         = '▀'
    , TXMonad.leftBorder         = '▌'
    , TXMonad.rightBorder        = '▐'
    }

-- | 默认按键绑定说明文本（帮助信息）。
help :: String
help = unlines
  [ "-- launching and killing programs"
  , "a        Add one focused window"
  , "x        Close the focused window"
  , "n        Rotate through the available layout algorithms"
  , ""
  , "-- move focus up or down the window stack"
  , "j        Move focus to the next window"
  , "k        Move focus to the previous window"
  , "m        Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "sj       Swap the focused window with the next window"
  , "sk       Swap the focused window with the previous window"
  , "sm       Swap the focused window and the master window"
  , ""
  , "-- resizing the master/slave ratio"
  , "h        Shrink the master area"
  , "l        Expand the master area"
  , ""
  , "-- increase or decrease number of windows in the master area"
  , "comma  (,)      Increment the number of windows in the master area"
  , "period (.)      减少主区域中的窗口数量"
  , ""
  , "-- quit, or restart"
  , "q               Quit txmonad"
  , ""
  , "-- Workspaces & screens"
  , "j[1..9]         切换到工作空间 N"
  , "sj[1..9]        Move client to workspace N"
  , "j{w,e,r}        Switch to screen 1, 2, or 3"
  , "sj{w,e,r}       Move client to screen 1, 2, or 3"
  ]
