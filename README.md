# txmonad: A toy xmonad
[txmonad]() is a toy version of [xmonad](https://github.com/xmonad/xmonad), which is a wonderful tiling window manager written in Haskell. txmonad simulates some of xmonad's major features in CLI mode. The purpose of starting this project is that xmonad is not only a wonderful tiling window manager, but also a famous Haskell production-level code tutorial. However, xmonad is tightly coupled with X11, which makes it harder for Haskell beginners on different platforms/systems to play around with it.

Here, txmonad imitates xmonad's architecture and type design to offer a playground for Haskell beginners. So you can play with, modify, and run the code to see the results swiftly.

## Quick Start
We use [stack](https://github.com/commercialhaskell/stack) to build the project.
After installing stack, simply run under the project folder:
```
stack build
stack exec txmonad-exe
```
Press `h` to view more supported commands.

## Screenshots
![demo](https://i.postimg.cc/rwPYZSZK/txmonad.jpg)


## What do txmonad and xmonad have in common
* Architecture design for layout.
* Functional data structure design for workspaces.
* Type-level programming and type design.
* Product-level code design.

## How txmonad differs from xmonad
* txmonad is NOT a window manager.
* txmonad doesn't depend on X11.
* txmonad simplifies the event and message handling process for operations. (Maybe over-simplified!)

## Future development plan
* User-configurable config and custom layout algorithms.
* QuickCheck support.
* A tutorial for Haskell beginners.
