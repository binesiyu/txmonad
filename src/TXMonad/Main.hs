{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | txmonad 主循环与事件处理模块。
--
-- 提供 'txmonad' 入口函数、主循环 'launch' 以及
-- 事件分发逻辑。
module TXMonad.Main
  ( txmonad
  )
where

import           Control.Monad                  ( forever, when )
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map                      as M
import           Data.Monoid                    ( getAll )
import           TXMonad.Config
import           TXMonad.Core
import           TXMonad.Operations
import           TXMonad.StackSet               ( new )

-- | txmonad 的公共入口函数。
--
-- 接收用户提供的 'TXConfig'，初始化窗口集、
-- 创建 'TX' monad 环境并启动主循环。
txmonad :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
txmonad = launch

-- | 启动窗口管理器主循环。
--
--  1. 将用户布局钩子包装为 'Layout' 存在类型
--  2. 创建初始窗口集（填充工作空间和屏幕）
--  3. 构建配置上下文和初始状态
--  4. 运行 'TX' monad 并进入事件循环（读取 stdin）
launch :: (LayoutClass l Window, Read (l Window)) => TXConfig l -> IO ()
launch initConfig = do
  let xmc = initConfig { layoutHook = Layout $ layoutHook initConfig }
  let layout = layoutHook xmc
      initialWinset =
        let padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
        in  new layout (padToLen (length $ sd xmc) (workspaces xmc)) (sd xmc)
      cf = TXConf { config = xmc, keyActions = keys xmc xmc }
      st = TXState { windowset = initialWinset, uniqueCnt = 0 }
  runTX cf st $ do
    printScreen
    forever $ prehandle =<< io getLine
  return ()
  where prehandle e = handleWithHook e

-- | 带钩子的事件处理：先检查事件钩子，再分发事件。
--
--   * 若 '@handleEventHook@' 返回 'All True'，继续处理事件
--   * 若 '@screenEventHook@' 返回 'All True'，重绘屏幕
handleWithHook :: Event -> TX ()
handleWithHook e = do
  evHook <- asks (handleEventHook . config)
  scHook <- asks (screenEventHook . config)
  whenTX (userCodeDef True $ getAll `fmap` evHook e) (handle e)
  whenTX (userCodeDef True $ getAll `fmap` scHook e) printScreen

-- | 事件分发核心：查找按键映射表并执行对应操作。
--
-- 若按键未在映射表中找到，则忽略该事件。
handle :: Event -> TX ()
handle e = do
  ks <- asks keyActions
  userCodeDef () $ whenJust (M.lookup e ks) id
  return ()
