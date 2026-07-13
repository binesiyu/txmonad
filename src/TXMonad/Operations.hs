-- | 窗口操作与终端渲染模块。
--
-- 提供窗口管理操作（添加、删除、聚焦、消息发送）、
-- 基于 ANSI 终端的可视化渲染，以及屏幕布局更新逻辑。
module TXMonad.Operations where

import           TXMonad.Core
import           TXMonad.Layout                 ( Full(..) )
import qualified TXMonad.StackSet              as W

import           Data.Array
import           Data.List                      ( find
                                                , intercalate
                                                , nub
                                                , (\\)
                                                )
import           Data.Maybe
import           Data.Monoid                    ( Endo(..) )
import           System.Console.ANSI
import           System.IO
import qualified Text.Read                     as T

import           Control.Monad.Reader
import           Control.Monad.State

-- | 添加一个新窗口到当前工作空间。
--
-- 生成唯一的窗口 ID（递增计数器），然后调用 'manage' 插入窗口。
addWindow :: TX ()
addWindow = do
  TXState { uniqueCnt = x } <- get
  modify (\s -> s { uniqueCnt = x + 1 })
  manage (show x)

-- | 删除当前聚焦的窗口。
deleteWindow :: TX ()
deleteWindow = withFocused unmanage

-- | 将窗口插入到窗口集中（调用用户钩子后）。
manage :: Window -> TX ()
manage w = do
  let f = W.insertUp w
  g <- appEndo <$> userCodeDef (Endo id) (return (Endo id))
  windows (g . f)

-- | Unmanage. 当窗口不存在时，将其从所有工作空间的窗口列表中移除。
--
unmanage :: Window -> TX ()
unmanage = windows . W.delete

-- | 对窗口集执行修改操作的便捷函数。
windows :: (WindowSet -> WindowSet) -> TX ()
windows = modifyWindowSet

-- | 将屏幕转化为字符串表示（头部 + 详细行列表）。
--
-- 返回 (头部行, 详细行列表) 元组。
screenString
  :: Char
  -> Char
  -> Char
  -> Char
  -> ([(Window, Rectangle)], WindowScreen)
  -> (String, [String])
screenString u d l r (rect, w) = (hdr, detail)
 where
  hdr    = screenHead w
  detail = screenDetail u d l r w rect

-- | 生成屏幕头部行字符串（屏幕编号 + 工作空间名称）。
screenHead :: WindowScreen -> String
screenHead (W.Screen w sid sd) =
  "Screen: " ++ show (1 + fromIntegral sid :: Int) ++ " Workspace: " ++ W.tag w

-- | 生成屏幕详细内容的字符矩阵。
--
-- 使用二维数组构建，填充窗口边框字符和窗口名称。
screenDetail
  :: Char
  -> Char
  -> Char
  -> Char
  -> WindowScreen
  -> [(Window, Rectangle)]
  -> [String]
screenDetail u d l r (W.Screen _ _ (SD (Rectangle x y w h))) rects
  | w == 0 || h == 0
  = []
  | otherwise
  = [ [ res ! (i, j) | i <- [x .. x + w - 1] ] | j <- [y .. y + h - 1] ]
 where
  initArr = array
    ((x, y), (x + w - 1, y + h - 1))
    [ ((i, j), ' ') | i <- [x .. x + w - 1], j <- [y .. y + h - 1] ]
  f a t = a // windowDetail u d l r t
  res = foldl f initArr rects

-- | 构建单个窗口的边框字符和名称的字符坐标映射。
--
-- 根据窗口矩形大小决定绘制哪些边框元素：
-- 上边框、下边框、左边框、右边框、窗口名称。
windowDetail
  :: Char -> Char -> Char -> Char -> (Window, Rectangle) -> [((Int, Int), Char)]
windowDetail u d l r (ws, Rectangle x y w h)
  | w == 0 || h == 0 = []
  | h == 1           = up
  | h == 2           = up ++ down
  | h >= 3 && w == 1 = up ++ down ++ left
  | h >= 3 && w == 2 = up ++ down ++ left ++ right
  | otherwise        = up ++ down ++ left ++ right ++ name
 where
  up    = [ ((x + i, y), u) | i <- [0 .. w - 1] ]
  down  = [ ((x + i, y + h - 1), d) | i <- [0 .. w - 1] ]
  left  = [ ((x, y + i), l) | i <- [1 .. h - 2] ]
  right = [ ((x + w - 1, y + i), r) | i <- [1 .. h - 2] ]
  name  = zip [ (x + i, y + 1) | i <- [1 .. w - 2] ] ws

-- | 使用指定颜色执行 IO 操作，并在前后设置/重置 SGR 属性。
printWithColor :: Color -> IO () -> IO ()
printWithColor c action = do
  setSGR [SetColor Foreground Dull c]
  action
  setSGR [Reset]

-- | 打印一行文本，其中中间部分（聚焦行）使用聚焦色，其余使用普通色。
printFocusLine :: Int -> Int -> Color -> Color -> String -> IO ()
printFocusLine x w fbc nbc s = do
  printWithColor nbc $ putStr left
  printWithColor fbc $ putStr mid
  printWithColor nbc $ putStrLn right
 where
  (left, midright) = splitAt x s
  (mid , right   ) = splitAt w midright

-- | 打印聚焦屏幕的完整视图（头部 + 上部分 + 聚焦中部分 + 下部分）。
printFocus :: Color -> Color -> Rectangle -> (String, [String]) -> IO ()
printFocus fbc nbc (Rectangle x y w h) (hdr, detail) = do
  printWithColor fbc $ putStrLn hdr
  printWithColor nbc $ putStr $ unlines up
  printMid
  printWithColor nbc $ putStr $ unlines down
 where
  (up , middown) = splitAt y detail
  (mid, down   ) = splitAt h middown
  printMid       = mapM_ (printFocusLine x w fbc nbc) mid

-- | 使用默认（非聚焦）颜色打印屏幕列表。
printDefault :: Color -> [(String, [String])] -> IO ()
printDefault nbc = mapM_ printAll
 where
  printAll (h, d) = printWithColor nbc $ putStrLn $ unlines (h : d)

-- | 打印所有屏幕，其中聚焦屏幕使用聚焦色高亮显示。
printAllWithFocus
  :: [(String, [String])] -> Maybe Rectangle -> Color -> Color -> IO ()
printAllWithFocus res            Nothing  _   nbc = printDefault nbc res
printAllWithFocus (res : allRes) (Just r) fbc nbc = do
  printFocus fbc nbc r res
  printDefault nbc allRes

-- | 显示帮助信息并等待用户输入。
helpCommand :: String -> TX ()
helpCommand s = io $ do
  setCursorPosition 0 0
  clearScreen
  putStrLn s
  inputLine

-- | 显示命令行提示符并刷新输出。
inputLine :: IO ()
inputLine = do
  putStrLn "press h for help"
  putStr "txmonad> "
  hFlush stdout

-- | 渲染并输出完整的屏幕显示。
--
-- 这是终端渲染的主函数：遍历所有屏幕计算布局，
-- 确定聚焦窗口，绘制边框字符，并使用 ANSI 颜色输出。
printScreen :: TX ()
printScreen = do
  TXState { windowset = ws } <- get
  conf                       <- asks config
  let allScreens = W.screens ws
  rects <- forM allScreens $ \w -> do
    let wsp      = W.workspace w
        n        = W.tag wsp
        this     = W.view n ws
        tiled    = W.stack . W.workspace . W.current $ this
        viewrect = screenRect $ W.screenDetail w
    (rs, ml') <- runLayout wsp { W.stack = tiled } viewrect
    updateLayout n ml'
    return (rs, w)
  let fw    = W.peek ws
      frect = do
        fwid <- fw
        snd <$> find ((== fwid) . fst) (fst $ head rects)
      u   = upBorder conf
      d   = downBorder conf
      l   = leftBorder conf
      r   = rightBorder conf
      fbc = fromMaybe Red $ T.readMaybe (focusedBorderColor conf)
      nbc = fromMaybe Blue $ T.readMaybe (normalBorderColor conf)
  io (setCursorPosition 0 0)
  io clearScreen
  io $ printAllWithFocus (fmap (screenString u d l r) rects) frect fbc nbc
  io inputLine

-- | 更新指定工作空间的布局。
--
-- 若布局发生变化（@ml@ 非空），在所有工作空间中查找匹配的标签并更新布局。
updateLayout :: WorkspaceId -> Maybe (Layout Window) -> TX ()
updateLayout i ml = whenJust ml $ \l -> runOnWorkSpaces
  $ \ww -> return $ if W.tag ww == i then ww { W.layout = l } else ww

-- | 向当前工作空间的布局发送消息。
--
-- 将消息包装为 'SomeMessage' 并传递给当前布局的 'handleMessage'，
-- 若布局发生变化则更新窗口集。
sendMessage :: Message a => a -> TX ()
sendMessage a = do
  w   <- W.workspace . W.current <$> gets windowset
  ml' <- handleMessage (W.layout w) (SomeMessage a)
  whenJust ml' $ \l' -> modifyWindowSet $ \ws -> ws
    { W.current =
      (W.current ws) { W.workspace = (W.workspace $ W.current ws)
                       { W.layout = l'
                       }
                     }
    }
  return ()

-- | 通过函数修改窗口集。
modifyWindowSet :: (WindowSet -> WindowSet) -> TX ()
modifyWindowSet f = modify $ \xst -> xst { windowset = f (windowset xst) }

-- | 返回屏幕 'sc' 上可见的工作空间，若不存在则返回 'Nothing'。
screenWorkspace :: ScreenId -> TX (Maybe WorkspaceId)
screenWorkspace sc = withWindowSet $ return . W.lookupWorkspace sc

-- | 对当前聚焦的窗口应用 'TX' 操作，若不存在则不执行。
withFocused :: (Window -> TX ()) -> TX ()
withFocused f = withWindowSet $ \w -> whenJust (W.peek w) f
