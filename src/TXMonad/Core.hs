{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | txmonad 窗口管理器的核心类型与抽象。
--
-- 本模块定义了基础数据类型，包括 'TX' monad
-- （'ReaderT' 'TXConf' ('StateT' 'TXState' 'IO') 栈）、窗口集
-- 类型别名、'LayoutClass' 类型类层次，以及可扩展的
-- 'Message' 布局通信机制。
module TXMonad.Core
  ( TX
  , WindowSet
  , WindowScreen
  , WindowSpace
  , WorkspaceId
  , Window
  , Event
  , ScreenId(..)
  , TXState(..)
  , TXConf(..)
  , TXConfig(..)
  , LayoutClass(..)
  , Layout(..)
  , Typeable
  , Message
  , Rectangle(..)
  , SomeMessage(..)
  , LayoutMessages(..)
  , ScreenDetail(..)
  , fromMessage
  , runTX
  , catchTX
  , runOnWorkSpaces
  , userCode
  , userCodeDef
  , whenTX
  , whenJust
  , io
  , withWindowSet
  )
where

import           TXMonad.StackSet

import           Control.Monad                  ( liftM, liftM2, when )
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Default
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Monoid
import           Data.Typeable

-- | 窗口管理器运行时状态。
--
-- 包含当前窗口集和用于生成唯一窗口 ID 的计数器。
data TXState = TXState
  { windowset :: WindowSet  -- ^ 当前窗口集（工作空间、屏幕、窗口栈）
  , uniqueCnt :: Int        -- ^ 窗口唯一 ID 计数器，每次创建窗口自增
  } deriving (Show)

-- | 窗口管理器运行时配置上下文。
--
-- 包含了从 'TXConfig' 解析出的配置信息和已计算好的按键映射表。
data TXConf = TXConf
  { config     :: TXConfig Layout   -- ^ 用户配置（布局钩子、工作空间等）
  , keyActions :: M.Map Event (TX ()) -- ^ 按键到操作的映射表
  }

-- | 窗口管理器配置记录。
--
-- 包含用户可自定义的所有配置选项：布局、按键绑定、
-- 边框样式、事件钩子等。
data TXConfig l = TXConfig
  { layoutHook         :: l Window
    -- ^ 布局钩子，定义窗口如何在屏幕上排列
  , workspaces         :: [String]
    -- ^ 工作空间名称列表
  , keys               :: TXConfig Layout -> M.Map Event (TX ())
    -- ^ 按键映射函数，接收配置返回按键到操作的映射
  , sd                 :: [ScreenDetail]
    -- ^ 屏幕细节列表（每个屏幕的位置和大小）
  , handleEventHook    :: Event -> TX All
    -- ^ 事件处理前钩子，返回 'All False' 可阻止事件处理
  , screenEventHook    :: Event -> TX All
    -- ^ 屏幕刷新钩子，返回 'All False' 可阻止自动重绘
  , normalBorderColor  :: String
    -- ^ 非聚焦窗口的边框颜色
  , focusedBorderColor :: String
    -- ^ 聚焦窗口的边框颜色
  , upBorder           :: Char
    -- ^ 窗口上边框字符
  , downBorder         :: Char
    -- ^ 窗口下边框字符
  , leftBorder         :: Char
    -- ^ 窗口左边框字符
  , rightBorder        :: Char
    -- ^ 窗口右边框字符
  }

-- | 矩形几何区域，用于描述窗口和屏幕的位置与大小。
data Rectangle = Rectangle
  { x      :: Int  -- ^ 左上角的 x 坐标（列）
  , y      :: Int  -- ^ 左上角的 y 坐标（行）
  , width  :: Int  -- ^ 矩形的宽度（列数）
  , height :: Int  -- ^ 矩形的高度（行数）
  } deriving (Eq, Show, Read)

-- | 窗口集的完整类型别名，参数化为具体的 Window 和 ScreenId 类型。
type WindowSet
  = StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail

-- | 窗口屏幕的类型别名。
type WindowScreen
  = Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

-- | 窗口工作空间的类型别名。
type WindowSpace = Workspace WorkspaceId (Layout Window) Window

-- | 工作空间标识符，用字符串表示。
type WorkspaceId = String

-- | 窗口标识符，用字符串表示。
type Window = String

-- | 事件类型，用字符串表示（通常为按键组合）。
type Event = String

-- | 屏幕标识符，用整数 newtype 封装。
--
-- 派生 'Num'、'Enum'、'Integral' 等实例以方便算术操作。
newtype ScreenId =
  S Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

-- | 屏幕细节，描述单个屏幕的布局信息。
data ScreenDetail = SD
  { screenRect :: Rectangle  -- ^ 该屏幕在工作区域中的位置和大小
  } deriving (Eq, Show, Read)

-- | 'TX' monad：窗口管理器内部所有操作的载体。
--
-- 基于 'ReaderT' 'TXConf' ('StateT' 'TXState' 'IO') 栈：
--
--   * 'ReaderT' 提供只读配置环境
--   * 'StateT' 提供可变窗口状态
--   * 'IO' 提供终端 I/O 能力
--
newtype TX a =
  TX (ReaderT TXConf (StateT TXState IO) a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState TXState, MonadReader TXConf)

-- 注意：'TX' 使用标准 GHC 派生机制获得 'Monad' 与 'Applicative' 实例。

-- | 'Semigroup' 实例：通过 'liftM2' 将操作提升到 'TX' monad 中。
instance Semigroup a => Semigroup (TX a) where
  (<>) = liftM2 (<>)

-- | 'Monoid' 实例：'mempty' 返回 'mempty' 包装后的值。
instance (Monoid a) => Monoid (TX a) where
  mempty = return mempty

-- | 'Default' 实例：通过 'def' 获取包装后的默认值。
instance Default a => Default (TX a) where
  def = return def

-- | 运行 'TX' monad 栈，给定初始配置和状态。
--
-- 返回最终结果值和最终状态。这是 'TX' monad 的唯一出口点。
runTX :: TXConf -> TXState -> TX a -> IO (a, TXState)
runTX c st (TX a) = runStateT (runReaderT a c) st

-- | 异常捕获：执行 @job@，忽略其中发生的异常并用 @errHandler@ 替代。
--
-- 注意：当前实现仅执行 @job@，未实际捕获异常。
catchTX :: TX a -> TX a -> TX a
catchTX job errHandler = do
  st      <- get
  c       <- ask
  (a, s') <- io $ runTX c st job
  put s'
  return a

-- | 安全包装用户代码，捕获可能的运行时异常。
--
-- 返回 'Just' 结果或 'Nothing'（发生异常时）。
userCode :: TX a -> TX (Maybe a)
userCode a = catchTX (Just `liftM` a) (return Nothing)

-- | 安全包装用户代码并指定默认值。
--
-- 执行用户代码，若发生异常则返回指定的默认值。
userCodeDef :: a -> TX a -> TX a
userCodeDef defValue a = fromMaybe defValue `liftM` userCode a

-- | 存在量化的布局包装类型。
--
-- 使用存在类型擦除具体布局类型，使得不同类型布局可以
-- 存放在同一数据结构中。要求布局类型同时支持 'Read'。
data Layout a =
  forall l. (LayoutClass l a, Read (l a)) =>
            Layout (l a)

-- | 布局类：所有布局算法必须实现的类型类。
--
-- 定义了布局计算的核心接口，包括纯函数版本和 monadic 版本。
-- 每个方法都有默认实现，布局类型可按需覆盖。
class Show (layout a) =>
      LayoutClass layout a
  where
  -- | 运行布局计算，将工作空间中的窗口排列到指定矩形区域中。
  --
  -- 根据工作空间是否有窗口栈，调用 'doLayout' 或 'emptyLayout'。
  runLayout ::
       Workspace WorkspaceId (layout a) a
    -> Rectangle
    -> TX ([(a, Rectangle)], Maybe (layout a))
  runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms
  -- | 对非空栈执行布局计算。
  --
  -- 返回每个窗口分配到的矩形区域，以及可能的布局变化。
  -- 默认实现直接调用 'pureLayout' 并返回 'Nothing'（无布局变化）。
  doLayout ::
       layout a
    -> Rectangle
    -> Stack a
    -> TX ([(a, Rectangle)], Maybe (layout a))
  doLayout l r s = return (pureLayout l r s, Nothing)
  -- | 纯布局函数：给定屏幕矩形和窗口栈，返回窗口到矩形的映射。
  --
  -- 默认将所有窗口放在同一个矩形中（即 'Full' 布局的效果）。
  pureLayout :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
  pureLayout _ r s = [(focus s, r)]
  -- | 空工作空间的布局计算。
  --
  -- 默认返回空列表和 'Nothing'（无布局变化）。
  emptyLayout ::
       layout a -> Rectangle -> TX ([(a, Rectangle)], Maybe (layout a))
  emptyLayout _ _ = return ([], Nothing)
  -- | 处理布局消息（如切换布局、调整大小等）。
  --
  -- 默认委托给 'pureMessage'，返回可能的布局变更。
  handleMessage :: layout a -> SomeMessage -> TX (Maybe (layout a))
  handleMessage l = return . pureMessage l
  -- | 纯消息处理函数（无 IO 副作用）。
  --
  -- 默认忽略所有消息返回 'Nothing'（无变化）。
  pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
  pureMessage _ _ = Nothing
  -- | 布局的描述字符串，用于终端显示。
  --
  -- 默认使用 'show' 输出。
  description :: layout a -> String
  description = show

-- | 'Layout' 的 'Show' 实例：委托给内部布局的 'Show' 实现。
instance Show (Layout a) where
  show (Layout l) = show l

-- | 'Layout' 的 'LayoutClass' 实例。
--
-- 将 'LayoutClass' 的所有调用委托给内部包装的具体布局类型，
-- 并使用 'fmap Layout' 包装可能的布局变更返回值。
instance LayoutClass Layout Window where
  runLayout (Workspace i (Layout l) ms) r =
    fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
  doLayout (Layout l) r s = fmap (fmap Layout) `fmap` doLayout l r s
  emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
  handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
  description (Layout l) = description l

-- | 消息类型类：可被布局接收和处理的消息。
--
-- 要求实现 'Typeable' 以支持运行时类型转换（'cast'）。
class Typeable a =>
      Message a


-- | 存在量化的消息包装类型。
--
-- 使用存在类型擦除具体消息类型，使得不同类型的消息可以
-- 在同一通道中传递。
data SomeMessage =
  forall a. Message a =>
            SomeMessage a

-- | 尝试从 'SomeMessage' 中提取特定类型的消息。
--
-- 如果消息的实际类型与期望类型匹配，返回 'Just' 包装的值；
-- 否则返回 'Nothing'。
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- | 'Event' 消息实例：允许事件（按键）作为消息传递。
instance Message Event

-- | 布局系统内部消息。
--
--   * 'Hide'：通知布局隐藏窗口
--   * 'ReleaseResources'：通知布局释放资源
data LayoutMessages
  = Hide              -- ^ 隐藏布局窗口
  | ReleaseResources  -- ^ 释放布局资源
  deriving (Typeable, Eq)

-- | 'LayoutMessages' 消息实例。
instance Message LayoutMessages

-- | 条件执行：当 monadic 条件为 'True' 时执行指定操作。
--
-- 这是 'when' 在 'TX' monad 中的变体，条件本身也是 monadic 的。
whenTX :: TX Bool -> TX () -> TX ()
whenTX a f = a >>= \b -> when b f

-- | Maybe 条件执行：若 'Maybe' 值非空则执行。
--
-- 类似于 'Data.Foldable.for_'，但仅对 'Maybe' 值操作。
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | 将 'IO' 操作提升到 'TX' monad 中。
--
-- 实际上是 'liftIO' 的简化别名。
io :: MonadIO m => IO a -> m a
io = liftIO

-- | 对所有工作空间执行指定操作。
--
-- 依次处理隐藏的工作空间、当前屏幕和可见屏幕的工作空间，
-- 并更新窗口集。常用于更新布局状态。
runOnWorkSpaces :: (WindowSpace -> TX WindowSpace) -> TX ()
runOnWorkSpaces job = do
  ws    <- gets windowset
  h     <- mapM job $ hidden ws
  c : v <-
    mapM (\s -> (\w -> s { workspace = w }) <$> job (workspace s))
    $ current ws
    : visible ws
  modify $ \s -> s { windowset = ws { current = c, visible = v, hidden = h } }

-- | 以当前窗口集为参数执行 monadic 操作。
--
-- 从状态中获取当前 'WindowSet'，并将其传给指定的操作。
-- 常用于查询窗口集信息（如 'peek'、'lookupWorkspace' 等）。
withWindowSet :: (WindowSet -> TX a) -> TX a
withWindowSet f = gets windowset >>= f
