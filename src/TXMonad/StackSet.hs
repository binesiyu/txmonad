-- | 核心数据结构模块：StackSet 及其操作。
--
-- 定义了 'StackSet'、'Workspace'、'Screen'、'Stack' 等
-- 函数式数据结构（zipper-like 焦点管理），以及视图切换、
-- 窗口插入/删除、焦点移动、屏幕管理等功能。
module TXMonad.StackSet
  ( -- * 数据类型定义
    StackSet(..)
  , Workspace(..)
  , Screen(..)
  , Stack(..)
    -- * Stack 操作
  , integrate
  , integrate'
    -- * StackSet 构造
  , new
    -- * StackSet 查询
  , peek
  , allWindows
  , screens
  , lookupWorkspace
    -- * StackSet 视图切换
  , view
  , greedyView
    -- * StackSet 栈修改
  , modify'
    -- * StackSet 窗口管理
  , insertUp
  , delete
    -- * StackSet 焦点操作
  , focusUp
  , focusDown
  , focusMaster
    -- * StackSet 排序操作
  , swapUp
  , swapDown
  , swapMaster
    -- * StackSet 跨工作空间操作
  , shift
  )
where

import qualified Data.List                     as L
                                                ( deleteBy
                                                , filter
                                                , find
                                                , nub
                                                , splitAt
                                                )
import           Data.Maybe                     ( isJust
                                                , listToMaybe
                                                )
import           Prelude                 hiding ( filter )

-- =====================================================================
-- 数据类型定义
-- =====================================================================

-- | 窗口管理器的核心数据结构。
--
-- 包含当前屏幕、可见屏幕列表和隐藏工作空间列表。
-- 类似于 zipper 结构，始终有一个聚焦的"当前"屏幕。
data StackSet i l a sid sd = StackSet
  { current :: Screen i l a sid sd      -- ^ 当前聚焦的屏幕
  , visible :: [Screen i l a sid sd]    -- ^ 可见但未聚焦的屏幕列表
  , hidden  :: [Workspace i l a]        -- ^ 隐藏（不可见）的工作空间列表
  } deriving (Show, Read, Eq)

-- | 物理屏幕，包含工作空间、屏幕标识和细节信息。
data Screen i l a sid sd = Screen
  { workspace    :: Workspace i l a  -- ^ 该屏幕上显示的工作空间
  , screen       :: sid              -- ^ 屏幕标识符
  , screenDetail :: sd               -- ^ 屏幕细节（位置、尺寸等）
  } deriving (Show, Read, Eq)

-- | 工作空间：包含标签、布局和可选的窗口栈。
data Workspace i l a = Workspace
  { tag    :: i              -- ^ 工作空间标签（名称）
  , layout :: l              -- ^ 当前布局算法
  , stack  :: Maybe (Stack a) -- ^ 窗口栈（'Nothing' 表示空工作空间）
  } deriving (Show, Read, Eq)

-- | 窗口栈：Zipper 结构用于焦点管理。
--
-- 聚焦元素始终在 @focus@ 位置，@up@ 为焦点之上的元素，
-- @down@ 为焦点之下的元素。完整窗口顺序为 @reverse up ++ focus : down@。
data Stack a = Stack
  { focus :: a   -- ^ 当前聚焦的元素
  , up    :: [a]  -- ^ 焦点之上的元素列表
  , down  :: [a]  -- ^ 焦点之下的元素列表
  } deriving (Show, Read, Eq)

-- =====================================================================
-- Stack 操作（纯栈级别，不涉及 StackSet）
-- =====================================================================

-- | 将 'Stack' 展开为平面列表（按焦点顺序排列）。
--
-- 顺序为：up 元素（反转）→ focus → down 元素。
integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r

-- | 安全版本的 'integrate'：空栈返回空列表。
integrate' :: Maybe (Stack a) -> [a]
integrate' = maybe [] integrate

-- =====================================================================
-- StackSet 构造
-- =====================================================================

-- | 创建新的 'StackSet'。
--
-- 给定布局 @l@、工作空间 ID 列表和屏幕细节列表，
-- 构建初始窗口集。多余的窗口 ID 放入隐藏列表。
new :: (Integral s) => l -> [i] -> [sd] -> StackSet i l a s sd
new l wids m = StackSet cur visi unseen
 where
  (seen, unseen) =
    L.splitAt (length m) $ map (\i -> Workspace i l Nothing) wids
  (cur : visi) = [ Screen i s sd | (i, s, sd) <- zip3 seen [0 ..] m ]

-- =====================================================================
-- StackSet 查询
-- =====================================================================

-- | 查看当前聚焦的窗口，空栈时返回 'Nothing'。
peek :: StackSet i l a s sd -> Maybe a
peek = with Nothing (return . focus)

-- | 获取窗口集中所有窗口的列表（去重后）。
allWindows :: Eq a => StackSet i l a s sd -> [a]
allWindows = L.nub . concatMap (integrate' . stack) . workspaces

-- | 获取所有屏幕的列表。
screens :: StackSet i l a s sd -> [Screen i l a s sd]
screens s = current s : visible s

-- | 查找 Xinerama 屏幕 @sc@ 上可见的工作空间的标签。
-- 若屏幕超出范围则返回 'Nothing'。
lookupWorkspace :: Eq s => s -> StackSet i l a s sd -> Maybe i
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s _ <- current w : visible w, s == sc ]

-- =====================================================================
-- StackSet 视图切换
-- =====================================================================

-- | 将指定工作空间切换到当前屏幕。
--
-- 若该工作空间已在当前屏幕，不做任何操作；
-- 若在可见列表中，将其提升到当前屏幕（交换屏幕）；
-- 若在隐藏列表中，将其提升到当前屏幕。
view :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
view i s
  | i == currentTag s = s
  |
    -- current
    Just x <- L.find ((i ==) . tag . workspace) (visible s)
    -- if it is visible, it is just raised
                                                            = s
    { current = x
    , visible = current s : L.deleteBy (equating screen) x (visible s)
    }
  | Just x <- L.find ((i ==) . tag) (hidden s) -- must be hidden then
    -- 若该工作空间处于隐藏状态，则将其提升至当前使用的 Xinerama 屏幕
                                               = s
    { current = (current s) { workspace = x }
    , hidden  = workspace (current s) : L.deleteBy (equating tag) x (hidden s)
    }
  | otherwise = s -- not a member of the stackset
  where equating f = \x y -> f x == f y

-- |
-- 将焦点设置到给定工作空间。
--
--   若该工作空间不存在，返回原 'StackSet'。
--   若该工作空间为 'hidden'，则将其显示在当前屏幕上，
--   并将当前工作空间移至 'hidden'。
--   若该工作空间在另一屏幕上 'visible'，
--   则交换两个屏幕的工作空间。
greedyView :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
greedyView w ws
  | any wTag (hidden ws) = view w ws
  | (Just s) <- L.find (wTag . workspace) (visible ws) = ws
    { current = (current ws) { workspace = workspace s }
    , visible = s { workspace = workspace (current ws) }
                  : L.filter (not . wTag . workspace) (visible ws)
    }
  | otherwise = ws
  where wTag = (w ==) . tag

-- =====================================================================
-- StackSet 栈修改（修改当前工作空间的窗口栈）
-- =====================================================================

-- | 'modify' 的简化版本，不需要处理 'Nothing' 栈情况。
modify' :: (Stack a -> Stack a) -> StackSet i l a s sd -> StackSet i l a s sd
modify' f = modify Nothing (Just . f)

-- =====================================================================
-- StackSet 窗口管理（插入 / 删除）
-- =====================================================================

-- | 在焦点之上插入一个新窗口。
--
-- 若窗口已存在则不重复插入。新窗口插入后成为新的焦点。
insertUp :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertUp a s = if member a s then s else insert
 where
  insert =
    modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack a l (t : r)) s

-- |
-- /当前窗口 O(1)，一般情况 O(n)/。删除窗口 @w@（若存在）。
--
-- 四种情况：
--
--   * 在 'Nothing' 工作空间上删除，保持 'Nothing'
--
--   * 否则，尝试将焦点移到下方
--
--   * 否则，尝试将焦点移到上方
--
--   * 否则，工作空间为空，变为 'Nothing'
--
-- Master 相关行为：
--
--   * 删除 master 窗口会将 master 重置为新聚焦的窗口
--
--   * 否则，删除不影响 master
--
delete :: (Ord a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete = delete'

-- =====================================================================
-- StackSet 焦点操作（移动焦点位置）
-- =====================================================================

-- | 将焦点上移一位。包裹式：焦点到底时回到顶部。
focusUp :: StackSet i l a s sd -> StackSet i l a s sd
focusUp = modify' focusUp'

-- | 将焦点下移一位。包裹式：焦点到顶时回到底部。
focusDown :: StackSet i l a s sd -> StackSet i l a s sd
focusDown = modify' focusDown'

-- | 将焦点移到 master 窗口。
focusMaster :: StackSet i l a s sd -> StackSet i l a s sd
focusMaster = modify' $ \c -> case c of
  Stack _ [] _  -> c
  Stack t ls rs -> Stack x [] (xs ++ t : rs) where (x : xs) = reverse ls

-- =====================================================================
-- StackSet 排序操作（交换窗口位置）
-- =====================================================================

-- | 将聚焦窗口与其上方的窗口交换位置。
swapUp :: StackSet i l a s sd -> StackSet i l a s sd
swapUp = modify' swapUp'

-- | 将聚焦窗口与其下方的窗口交换位置。
swapDown :: StackSet i l a s sd -> StackSet i l a s sd
swapDown = modify' (reverseStack . swapUp' . reverseStack)

-- | 将聚焦窗口与 master 窗口交换。master 窗口保持不变。
swapMaster :: StackSet i l a s sd -> StackSet i l a s sd
swapMaster = modify' $ \c -> case c of
  Stack _ [] _  -> c -- already master.
  Stack t ls rs -> Stack t [] (xs ++ x : rs) where (x : xs) = reverse ls

-- =====================================================================
-- StackSet 跨工作空间操作
-- =====================================================================

-- | /O(w)/。将当前栈中的聚焦元素移动到工作空间 @n@，
-- 并使其成为该工作空间的聚焦元素。
--
-- 插入位置在该工作空间当前的聚焦元素之上。
-- 实际聚焦的工作空间不改变。若当前栈中没有元素，
-- 则返回原始的 'StackSet'。
shift :: (Ord a, Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift n s = maybe s (\w -> shiftWin n w s) (peek s)

-- =====================================================================
-- 内部辅助函数（不导出）
-- =====================================================================

-- 栈修改辅助 -----------------------------------------------------------

-- | 辅助函数：对当前聚焦的栈执行操作，空栈时返回默认值。
with :: b -> (Stack a -> b) -> StackSet i l a sid sd -> b
with dflt f = maybe dflt f . stack . workspace . current

-- | 修改当前工作空间的窗口栈。
--
-- 接收默认值和修改函数，更新当前窗口栈。
modify
  :: Maybe (Stack a)
  -> (Stack a -> Maybe (Stack a))
  -> StackSet i l a s sd
  -> StackSet i l a s sd
modify d f s = s
  { current =
    (current s) { workspace = (workspace (current s)) { stack = with d f s } }
  }

-- 查询辅助 -------------------------------------------------------------

-- | 获取当前工作空间的标签。
currentTag :: StackSet i l a s sd -> i
currentTag = tag . workspace . current

-- | 获取所有工作空间的列表（当前 + 可见 + 隐藏）。
workspaces :: StackSet i l a s sd -> [Workspace i l a]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

-- | 查找指定窗口所在工作空间的标签。
findTag :: Eq a => a -> StackSet i l a s sd -> Maybe i
findTag a s = listToMaybe [ tag w | w <- workspaces s, has a (stack w) ]
 where
  has _ Nothing              = False
  has x (Just (Stack t l r)) = x `elem` (t : l ++ r)

-- | 检查窗口是否存在于窗口集中。
member :: Eq a => a -> StackSet i l a s sd -> Bool
member a s = isJust (findTag a s)

-- | 检查给定的标签是否存在于 'StackSet' 中。
tagMember :: Eq i => i -> StackSet i l a s sd -> Bool
tagMember t = elem t . map tag . workspaces

-- 窗口删除辅助 ---------------------------------------------------------

-- | 仅临时从栈中移除窗口，不破坏 'StackSet' 中保存的特殊信息
delete' :: (Eq a) => a -> StackSet i l a s sd -> StackSet i l a s sd
delete' w s = s { current = removeFromScreen (current s)
                , visible = map removeFromScreen (visible s)
                , hidden  = map removeFromWorkspace (hidden s)
                }
 where
  removeFromWorkspace ws = ws { stack = stack ws >>= filter (/= w) }
  removeFromScreen scr =
    scr { workspace = removeFromWorkspace (workspace scr) }

-- 跨工作空间辅助 -------------------------------------------------------

-- | /O(n)/。在所有工作空间中搜索指定窗口 @w@，
-- 将其移动到工作空间 @n@ 并设为聚焦元素。
shiftWin
  :: (Ord a, Eq s, Eq i) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin n w s = case findTag w s of
  Just from | n `tagMember` s && n /= from -> go from s
  _ -> s
  where go from = onWorkspace n (insertUp w) . onWorkspace from (delete' w)

-- | 在指定工作空间上执行操作，完成后切换回原工作空间。
onWorkspace
  :: (Eq i, Eq s)
  => i
  -> (StackSet i l a s sd -> StackSet i l a s sd)
  -> (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (currentTag s) . f . view n $ s

-- Stack 内部操作 -------------------------------------------------------

-- | /O(n)/。'filter p s' 返回栈中满足谓词 @p@ 的元素。
-- 保持顺序，焦点移动方式与 'delete' 相同。
filter :: (a -> Bool) -> Stack a -> Maybe (Stack a)
filter p (Stack f ls rs) = case L.filter p (f : rs) of
  f' : rs' -> Just $ Stack f' (L.filter p ls) rs' -- 可能将焦点下移
  []       -> case L.filter p ls of
    f' : ls' -> Just $ Stack f' ls' [] -- 否则上移
    []       -> Nothing -- 结果为空栈

-- | 将焦点向上移动（栈操作）。
focusUp' :: Stack a -> Stack a
focusUp' (Stack t (l : ls) rs) = Stack l ls (t : rs)
focusUp' (Stack t []       rs) = Stack x xs [] where (x : xs) = reverse (t : rs)

-- | 将焦点向下移动（栈操作）。
focusDown' :: Stack a -> Stack a
focusDown' = reverseStack . focusUp' . reverseStack

-- | 将聚焦元素与上方元素交换（栈操作）。
swapUp' :: Stack a -> Stack a
swapUp' (Stack t (l : ls) rs) = Stack t ls (l : rs)
swapUp' (Stack t []       rs) = Stack t (reverse rs) []

-- | 反转栈：将 up 和 down 互换。
reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls
