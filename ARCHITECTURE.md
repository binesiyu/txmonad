# txmonad 架构文档

## 1. 项目概述

**txmonad** 是经典平铺式窗口管理器 [xmonad](https://xmonad.org) 的 CLI（命令行界面）模拟实现。它不使用 X11 协议，而是在终端中以 ASCII/Unicode 字符渲染模拟窗口和屏幕布局，通过标准输入（stdin）接收键盘事件来驱动窗口操作。

### 设计目标

- **教育性**：以可运行的代码展示 xmonad 的核心架构思想——纯函数式数据结构、Monad Transformer 栈、存在量化布局系统
- **可移植性**：不依赖任何 GUI 环境，仅需 ANSI 终端即可运行
- **可扩展性**：通过 `LayoutClass` 类型类支持自定义布局算法，通过 `Message` 系统支持布局间通信

### 技术栈

| 组件 | 选型 |
|------|------|
| 编译器 | GHC 9.4.8（LTS-21.25） |
| 构建系统 | Stack + hpack（`package.yaml`） |
| 终端渲染 | `ansi-terminal`（ANSI 转义序列 / SGR 颜色控制） |
| 核心依赖 | `mtl`（Monad Transformer）、`containers`（Map）、`array`（二维字符缓冲区） |

---

## 2. 模块架构

### 2.1 模块依赖图

```
                       ┌─────────────┐
                       │  app/Main   │
                       └──────┬──────┘
                              │ import
                       ┌──────▼──────┐
                       │  TXMonad    │  (顶层重导出)
                       └──────┬──────┘
                              │ re-export
              ┌───────────────┼───────────────┐
              │               │               │
       ┌──────▼──────┐ ┌──────▼──────┐ ┌──────▼──────┐
       │ TXMonad.Main│ │TXMonad.Core │ │TXMonad.Config│
       │  (主循环)    │ │ (核心类型)  │ │ (默认配置)   │
       └──────┬──────┘ └──────┬──────┘ └──────┬──────┘
              │               │               │
              │        ┌──────▼──────┐        │
              ├───────►│TXMonad.Stack│◄───────┤
              │        │    Set      │        │
              │        └──────┬──────┘        │
              │               │               │
       ┌──────▼──────┐ ┌──────▼──────┐        │
       │TXMonad.Opers│ │TXMonad.Layout│◄──────┤
       │ (操作/渲染) │ │  (布局算法)  │        │
       └─────────────┘ └─────────────┘        │
              ▲                               │
              └───────────────────────────────┘
```

依赖方向为**自下而上**，下层模块不依赖上层模块：

| 层级 | 模块 | 职责 | 依赖 |
|------|------|------|------|
| 数据层 | `TXMonad.StackSet` | 纯函数式窗口集数据结构 | 仅 `base` |
| 抽象层 | `TXMonad.Core` | TX monad、LayoutClass 类型类、Message 系统 | StackSet + mtl |
| 算法层 | `TXMonad.Layout` | 内置布局（Full、Tall、Mirror、Choose） | Core + StackSet |
| 操作层 | `TXMonad.Operations` | 窗口操作 + ANSI 终端渲染 | Core + Layout + StackSet |
| 配置层 | `TXMonad.Config` | 默认配置、按键映射、钩子 | Operations + Layout |
| 入口层 | `TXMonad.Main` | 主循环、事件分发 | Config + Operations |
| 重导出 | `TXMonad` | 聚合公开 API | Main |

### 2.2 文件清单

```
txmonad/
├── app/Main.hs                  # 可执行文件入口（3 行）
├── src/
│   ├── TXMonad.hs               # 库顶层重导出模块
│   └── TXMonad/
│       ├── StackSet.hs           # 核心数据结构（337 行）
│       ├── Core.hs               # TX monad 与 LayoutClass（358 行）
│       ├── Layout.hs             # 内置布局算法（247 行）
│       ├── Operations.hs         # 窗口操作与渲染（244 行）
│       ├── Config.hs             # 默认配置与按键映射（169 行）
│       └── Main.hs               # 主循环与事件处理（71 行）
├── test/Spec.hs                  # 测试文件
├── package.yaml                  # hpack 构建描述
├── stack.yaml                    # Stack 解析器配置
└── txmonad.cabal                 # 自动生成的 Cabal 文件
```

---

## 3. 核心设计

### 3.1 Monad Transformer 栈

txmonad 的所有副作用操作都在 `TX` monad 中执行，其定义为三层 transformer 栈：

```haskell
newtype TX a = TX (ReaderT TXConf (StateT TXState IO) a)
```

| 层 | 类型 | 作用 |
|----|------|------|
| 底层 | `IO` | 终端 I/O（读写 stdin/stdout、ANSI 控制） |
| 中层 | `StateT TXState` | 可变窗口集状态和窗口 ID 计数器 |
| 顶层 | `ReaderT TXConf` | 只读配置环境（已解析的按键表、用户配置） |

**设计优势**：
- 通过 `MonadReader` / `MonadState` 自动派生，操作配置和状态无需手动传递
- `liftIO` 将纯 IO 操作提升到 `TX` 中，保持类型安全
- 所有状态修改显式通过 monad 栈传递，易于追踪和测试

### 3.2 核心数据结构：StackSet

`StackSet` 是整个窗口管理器的状态核心，用纯函数式 zipper 结构管理窗口焦点：

```
StackSet i l a sid sd
├── current :: Screen i l a sid sd          ← 当前聚焦的物理屏幕
├── visible :: [Screen i l a sid sd]         ← 可见但未聚焦的屏幕
└── hidden  :: [Workspace i l a]            ← 隐藏的工作空间

Screen i l a sid sd
├── workspace    :: Workspace i l a          ← 该屏幕显示的工作空间
├── screen       :: sid                      ← 屏幕 ID
└── screenDetail :: sd                       ← 屏幕几何信息

Workspace i l a
├── tag    :: i                              ← 工作空间名称
├── layout :: l                              ← 当前布局
└── stack  :: Maybe (Stack a)                ← 窗口栈（Nothing = 空）

Stack a  (Zipper 结构)
├── focus :: a                               ← 当前焦点窗口
├── up    :: [a]                             ← 焦点之上的窗口
└── down  :: [a]                             ← 焦点之下的窗口
```

**Zipper 设计**：`Stack a` 是一个 zipper——`focus` 始终是焦点元素，完整窗口顺序为 `reverse up ++ [focus] ++ down`。这种设计使得焦点上下移动、窗口交换等操作均为 O(1) 时间复杂度。

### 3.3 布局系统

#### LayoutClass 类型类

所有布局算法必须实现 `LayoutClass` 类型类（定义在 `Core.hs` 中）：

```haskell
class Show (layout a) => LayoutClass layout a where
  runLayout     :: Workspace WorkspaceId (layout a) a -> Rectangle -> TX (...)
  doLayout      :: layout a -> Rectangle -> Stack a -> TX (...)
  pureLayout    :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
  emptyLayout   :: layout a -> Rectangle -> TX (...)
  handleMessage :: layout a -> SomeMessage -> TX (Maybe (layout a))
  pureMessage   :: layout a -> SomeMessage -> Maybe (layout a)
  description   :: layout a -> String
```

每个方法都有合理的默认实现，布局类型仅需覆盖关心的部分。

#### 内置布局

| 布局 | 数据结构 | 算法 |
|------|----------|------|
| `Full` | 无参数 | 所有窗口全屏，仅聚焦窗口可见 |
| `Tall` | `nmaster`, `ratio`, `delta` | 垂直分割：前 N 个窗口在主区域，其余在从区域 |
| `Mirror l` | 包装任意布局 | 将内部布局旋转 90°（交换宽高） |
| `Choose l r` | 左右两个子布局 + 方向标记 | 在左右布局间切换的组合布局 |

布局通过 `|||` 运算符（infixr 5）组合：

```haskell
layout = tiled ||| Mirror tiled ||| Full
-- 初始为 Tall，按 NextLayout 依次切换到 Mirror Tall → Full → 循环回 Tall
```

#### 存在量化（ExistentialQuantification）

`Layout a` 和 `SomeMessage` 使用存在类型擦除具体类型：

```haskell
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

data SomeMessage = forall a. Message a => SomeMessage a
```

这使得不同类型的布局可以存放在同一容器中，不同类型的消息可以在同一通道中传递，是实现**可扩展布局系统**的关键技术。

### 3.4 Message 协议

布局间的通信通过 `Message` 类型类和 `SomeMessage` 存在包装实现：

```
sender                         receiver
  │                               │
  │  sendMessage (SomeMessage     │
  │    Resize)                    │
  ├──────────────────────────────►│
  │                               ├─ fromMessage :: SomeMessage → Maybe Resize
  │                               │  (通过 Typeable 的 cast 尝试类型转换)
  │                               ├─ 匹配成功 → 处理消息，返回新布局
  │                               └─ 不匹配   → 返回 Nothing（布局不变）
```

`fromMessage` 利用 `Data.Typeable` 的 `cast` 在运行时安全地进行类型转换。如果消息类型匹配，布局处理消息并返回修改后的自身；否则忽略。

---

## 4. 数据流

### 4.1 启动流程

```
app/Main.hs
  └─ main = txmonad def
       │
       ▼
TXMonad.Main.launch config
  │
  ├─ 1. 将用户布局钩子包装为 Layout 存在类型
  ├─ 2. 创建初始 WindowSet（填充工作空间和屏幕）
  ├─ 3. 构建 TXConf（配置 + 按键映射表）
  ├─ 4. 运行 TX monad:
  │     ├─ printScreen        ← 首次渲染
  │     └─ forever loop:      ← 主循环
  │          ├─ io getLine     ← 读取 stdin 输入
  │          └─ handleWithHook ← 事件分发
  └─ 返回 IO ()
```

### 4.2 事件处理流程

```
stdin 输入（如 "j"）
  │
  ▼
handleWithHook event
  ├─ 1. 执行 handleEventHook → 返回 All True/False
  │     └─ False → 跳过事件处理
  ├─ 2. handle event:
  │     └─ 查找 keyActions Map → 执行对应 TX 操作
  │         ├─ "j" → windows W.focusDown   → 修改 StackSet 焦点
  │         ├─ "n" → sendMessage NextLayout → 切换布局 # 见注
  │         ├─ "a" → addWindow             → 创建新窗口
  │         └─ "q" → io exitSuccess        → 退出程序
  ├─ 3. 执行 screenEventHook → 返回 All True/False
  │     └─ False → 跳过屏幕重绘
  └─ 4. printScreen → 重新计算布局并渲染
```

> **注**：`sendMessage NextLayout` 通过 `LayoutClass.handleMessage` 发送给当前布局的 `Choose` 实例，`Choose` 内部调用子布局的 `handleMessage` 进行链式路由。

### 4.3 渲染流程

```
printScreen
  ├─ 获取 windowset 和 config
  ├─ 遍历所有屏幕:
  │   ├─ 获取工作空间和窗口栈
  │   ├─ runLayout → 调用布局的纯布局函数
  │   │   ├─ Tall:  tile 算法（主/从区域分割）
  │   │   ├─ Full:  所有窗口分配同一矩形
  │   │   └─ Mirror: 镜像矩形后委托内部布局
  │   └─ 返回 [(Window, Rectangle)] 映射
  ├─ screenString: 将矩形映射转换为字符矩阵
  │   ├─ windowDetail: 为每个窗口生成边框字符坐标
  │   └─ 构建二维 Array，填充边框字符和窗口名称
  ├─ 确定聚焦窗口和对应矩形
  └─ printAllWithFocus:
      ├─ 聚焦屏幕: 聚焦区域使用 focusedBorderColor
      └─ 非聚焦屏幕: 使用 normalBorderColor
```

---

## 5. 关键设计模式

### 5.1 纯函数式状态管理

所有 `StackSet` 操作（`view`、`insertUp`、`focusDown`、`shift` 等）都是**纯函数**：接收旧状态，返回新状态，无副作用。状态变更通过 `TX` monad 的 `modify` / `modifyWindowSet` 提交。

```haskell
-- 纯函数：不修改任何外部状态
view :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd

-- Monadic 包装：将纯函数提升到 TX 中修改状态
windows :: (WindowSet -> WindowSet) -> TX ()
windows f = modify $ \xst -> xst { windowset = f (windowset xst) }
```

### 5.2 组合式布局

`Choose` 布局通过左/右组合实现了布局的任意嵌套。`|||` 运算符使布局声明简洁：

```haskell
layout = tiled ||| Mirror tiled ||| Full
-- 等价于: Choose L tiled (Choose L (Mirror tiled) Full)
```

`NextLayout` 消息在 `Choose` 树的节点间路由，实现了布局切换的级联逻辑。

### 5.3 可扩展钩子系统

`TXConfig` 提供了两个钩子点，用户可在不修改核心代码的前提下定制行为：

| 钩子 | 类型 | 触发时机 | 返回值语义 |
|------|------|----------|------------|
| `handleEventHook` | `Event → TX All` | 事件分发前 | `All False` 阻止事件处理 |
| `screenEventHook` | `Event → TX All` | 事件处理后 | `All False` 阻止屏幕重绘 |

钩子通过 `userCode` / `userCodeDef` 包装执行，确保钩子中的异常不会导致整个管理器崩溃。

### 5.4 安全的用户代码执行

```haskell
userCode :: TX a → TX (Maybe a)
userCode a = catchTX (Just `liftM` a) (return Nothing)

userCodeDef :: a → TX a → TX a
userCodeDef defValue a = fromMaybe defValue `liftM` userCode a
```

所有用户提供的代码都通过 `catchTX` 包装执行，异常被静默捕获，保证了系统的健壮性。

---

## 6. 构建与运行

### 构建

```bash
stack build
```

### 运行

```bash
stack exec txmonad-exe
```

### 交互

程序启动后显示终端窗口布局，在 `txmonad>` 提示符下输入按键命令进行操作。按 `h` 查看帮助，按 `q` 退出。

### 测试

```bash
stack test
```

---

## 7. 类型别名一览

为简化类型签名，在 `Core.hs` 中定义了以下别名：

```haskell
type WorkspaceId  = String               -- 工作空间标识符
type Window       = String               -- 窗口标识符
type Event        = String               -- 事件（按键组合）
type WindowSet    = StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowScreen = Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace  = Workspace WorkspaceId (Layout Window) Window
```

`ScreenId` 为 `newtype S Int`，派生 `Num`/`Enum`/`Integral` 实例以方便索引操作。

---

## 8. GHC 扩展使用说明

| 扩展 | 用途 | 使用位置 |
|------|------|----------|
| `ExistentialQuantification` | `Layout a` 和 `SomeMessage` 的存在类型包装 | Core.hs |
| `GeneralizedNewtypeDeriving` | `TX` newtype 的自动实例派生 | Core.hs |
| `FlexibleInstances` | `Default (TXConfig a)` 中的类型族约束 | Config.hs / Layout.hs |
| `MultiParamTypeClasses` | `LayoutClass layout a` 多参数类型类 | Core.hs / Layout.hs |
| `TypeSynonymInstances` | 允许类型别名作为实例头 | Core.hs |
| `TypeFamilies` | `Default` 实例中的 `~` 类型等式约束 | Config.hs |
| `FlexibleContexts` | 放宽函数上下文约束 | Main.hs |
