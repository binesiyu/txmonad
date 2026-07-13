{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | 布局算法模块。
--
-- 提供了 txmonad 内置的布局类型：'Full'、'Tall'、'Mirror'、'Choose'，
-- 以及用于布局切换和缩放的 'ChangeLayout'、'Resize'、'IncMasterN'
-- 等消息类型。
module TXMonad.Layout
  ( Full(..)
  , Tall(..)
  , Mirror(..)
  , Choose
  , ChangeLayout(..)
  , Resize(..)
  , IncMasterN(..)
  , (|||)
  )
where

import           Control.Arrow                  ( second
                                                , (***)
                                                )
import           Control.Monad
import           Data.Maybe                     ( fromMaybe )
import           TXMonad.Core
import           TXMonad.StackSet              as W

-- | 主区域缩放消息。
--
--   * 'Shrink'：缩小主区域比例
--   * 'Expand'：扩大主区域比例
data Resize
  = Shrink  -- ^ 缩小主区域
  | Expand  -- ^ 扩大主区域
  deriving (Typeable)

-- | 主区域窗口数量调整消息。
--
-- 正整数增加主区域窗口数，负整数减少。
data IncMasterN =
  IncMasterN Int  -- ^ 主区域窗口增减量
  deriving (Typeable)

instance Message Resize

instance Message IncMasterN

-- | Full 布局：单窗口全屏显示。
--
-- 所有窗口都占据整个屏幕区域，但只有聚焦窗口可见。
data Full a =
  Full
  deriving (Show, Read)

-- | 'Full' 使用默认 'LayoutClass' 实现（所有窗口全屏）。
instance LayoutClass Full a

-- | Tall 布局：主区域 + 从区域的垂直分割布局。
--
--   * @tallNMaster@：主区域窗口数
--   * @tallRatioIncrement@：比例调整步长
--   * @tallRatio@：主区域所占比例（0 到 1 之间）
data Tall a = Tall
  { tallNMaster        :: Int       -- ^ 主区域窗口数量
  , tallRatioIncrement :: Rational  -- ^ 每次调整比例的步长
  , tallRatio          :: Rational  -- ^ 主区域所占比例
  } deriving (Show, Read)

-- | 'Tall' 布局的 'LayoutClass' 实现。
--
-- 'pureLayout' 将窗口分为主区域（nmaster 个，垂直排列）和
-- 从区域（其余窗口，水平排列）。
-- 'pureMessage' 处理 'Resize'（调整比例）和
-- 'IncMasterN'（调整主区域窗口数）消息。
instance LayoutClass Tall a where
  pureLayout (Tall nmaster _ frac) r s = zip ws rs
   where
    ws = W.integrate s
    rs = tile frac r nmaster (length ws)
  pureMessage (Tall nmaster delta frac) m = msum
    [fmap resize (fromMessage m), fmap incmastern (fromMessage m)]
   where
    resize Shrink = Tall nmaster delta (max 0 $ frac - delta)
    resize Expand = Tall nmaster delta (min 1 $ frac + delta)
    incmastern (IncMasterN d) = Tall (max 0 (nmaster + d)) delta frac

-- | Tall 布局的核心平铺算法。
--
-- 将 @n@ 个窗口分配到给定的矩形区域 @r@ 中：
--   前 @nmaster@ 个窗口在主区域（垂直分割），
--   其余在从区域（水平分割），主区域占比为 @f@。
tile :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile f r nmaster n = if n <= nmaster || nmaster == 0
  then splitVertically n r
  else splitVertically nmaster r1 ++ splitVertically (n - nmaster) r2
  where (r1, r2) = splitHorizontallyBy f r

-- | 将矩形垂直分割为 /n/ 个等高的子矩形。
splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) =
  Rectangle sx sy sw smallh
    : splitVertically (n - 1) (Rectangle sx (sy + smallh) sw (sh - smallh))
  where smallh = sh `div` n

-- | 将矩形水平分割为 /n/ 个等宽的子矩形（通过镜像实现）。
splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

-- | 按比例水平分割矩形。
splitHorizontallyBy, splitVerticallyBy
  :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
  (Rectangle sx sy leftw sh, Rectangle (sx + leftw) sy (sw - leftw) sh)
  where leftw = floor $ fromIntegral sw * f

-- | 按比例垂直分割矩形（通过镜像实现）。
splitVerticallyBy f =
  (mirrorRect *** mirrorRect) . splitHorizontallyBy f . mirrorRect

-- | 交换矩形的宽和高（用于 Mirror 布局实现）。
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = Rectangle ry rx rh rw

-- | 镜像布局包装器：将内部布局旋转 90 度。
--
-- 例如，@Mirror Tall@ 将 Tall 的垂直分割变为水平分割。
newtype Mirror l a =
  Mirror (l a)
  deriving (Show, Read)

-- | 'Mirror' 布局的 'LayoutClass' 实现。
--
-- 通过矩形镜像（交换宽高）和结果坐标镜像来实现旋转效果。
instance LayoutClass l a => LayoutClass (Mirror l) a where
  runLayout (W.Workspace i (Mirror l) ms) r =
    (map (second mirrorRect) *** fmap Mirror)
      `fmap` runLayout (W.Workspace i l ms) (mirrorRect r)
  handleMessage (Mirror l) = fmap (fmap Mirror) . handleMessage l
  description (Mirror l) = "Mirror " ++ description l

-- | 布局组合运算符：将两个布局串联为 'Choose'。
--
-- 初始状态选择左侧布局。用法示例：@Tall ||| Full@。
(|||) :: l a -> r a -> Choose l r a
(|||) = Choose L

infixr 5 |||

-- | 布局选择：在左右两个布局之间切换的组合布局。
--
-- 使用 @LR@ 标记当前活跃的布局侧。
data Choose l r a =
  Choose LR          -- ^ 当前激活哪一侧
         (l a)       -- ^ 左侧布局
         (r a)       -- ^ 右侧布局
  deriving (Read, Show)

-- | 左/右标记，用于 'Choose' 布局中的方向选择。
data LR
  = L  -- ^ 左侧
  | R  -- ^ 右侧
  deriving (Read, Show, Eq)

-- | 布局切换消息。
--
--   * 'FirstLayout'：切换到第一个布局
--   * 'NextLayout'：切换到下一个布局
data ChangeLayout
  = FirstLayout  -- ^ 切换到第一个布局
  | NextLayout   -- ^ 切换到下一个布局
  deriving (Eq, Show, Typeable)

instance Message ChangeLayout

-- | 内部消息：切换到下一个布局（不包裹）。
data NextNoWrap =
  NextNoWrap
  deriving (Eq, Show, Typeable)

instance Message NextNoWrap

-- | 向布局发送消息的便捷函数。
handle :: (LayoutClass l a, Message m) => l a -> m -> TX (Maybe (l a))
handle l m = handleMessage l (SomeMessage m)

-- | 'Choose' 布局的核心辅助函数：处理布局选择的方向切换。
--
-- 根据当前方向 @d@ 和目标方向 @d'@，决定是否隐藏旧布局
-- 并激活新布局。隐藏时发送 'Hide' 消息。
choose
  :: (LayoutClass l a, LayoutClass r a)
  => Choose l r a
  -> LR
  -> Maybe (l a)
  -> Maybe (r a)
  -> TX (Maybe (Choose l r a))
choose (Choose d _ _) d' Nothing Nothing | d == d' = return Nothing
choose (Choose d l r) d' ml mr                     = f lr
 where
  (l', r') = (fromMaybe l ml, fromMaybe r mr)
  lr       = case (d, d') of
    (L, R) -> (hide l', return r')
    (R, L) -> (return l', hide r')
    (_, _) -> (return l', return r')
  f (x, y) = Just <$> liftM2 (Choose d') x y
  hide x = fromMaybe x <$> handle x Hide

-- | 'Choose' 布局的 'LayoutClass' 实例。
--
-- 'runLayout' 委托给当前活跃侧的布局，
-- 'handleMessage' 处理 'NextLayout'、'FirstLayout'、
-- 'NextNoWrap' 和 'ReleaseResources' 消息，
-- 实现布局之间的切换、隐藏和资源释放逻辑。
instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
  runLayout (W.Workspace i (Choose L l r) ms) =
    fmap (second . fmap $ flip (Choose L) r) . runLayout (W.Workspace i l ms)
  runLayout (W.Workspace i (Choose R l r) ms) =
    fmap (second . fmap $ Choose R l) . runLayout (W.Workspace i r ms)
  description (Choose L l _) = description l
  description (Choose R _ r) = description r
  handleMessage lr m | Just NextLayout <- fromMessage m = do
    mlr' <- handle lr NextNoWrap
    maybe (handle lr FirstLayout) (return . Just) mlr'
  handleMessage c@(Choose d l r) m | Just NextNoWrap <- fromMessage m =
    case d of
      L -> do
        ml <- handle l NextNoWrap
        case ml of
          Just _  -> choose c L ml Nothing
          Nothing -> choose c R Nothing =<< handle r FirstLayout
      R -> choose c R Nothing =<< handle r NextNoWrap
  handleMessage c@(Choose _ l _) m | Just FirstLayout <- fromMessage m =
    flip (choose c L) Nothing =<< handle l FirstLayout
  handleMessage c@(Choose d l r) m | Just ReleaseResources <- fromMessage m =
    join $ liftM2 (choose c d)
                  (handle l ReleaseResources)
                  (handle r ReleaseResources)
  handleMessage c@(Choose d l r) m = do
    ml' <- case d of
      L -> handleMessage l m
      R -> return Nothing
    mr' <- case d of
      L -> return Nothing
      R -> handleMessage r m
    choose c d ml' mr'
