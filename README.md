# 312-Julia-Set-Visualizer

### Private project repo for CPSC 312

## How to Run simple image generator:

cd to directory:
`cd src`

Install deps:
`cabal update && cabal install hip`

To Run without building:
`./Visualizer`

To Build and Run (Do this if you have made changes to the source Visualizer.hs):
`ghc Visualizer.hs && ./Visualizer`

## How to Run animated version (openGL):

cd to directory:
`cd OpenGLsrc`

Install deps:
`cabal install Graphics.UI.GLFW && cabal install GLFW-b`

Build:
`ghc Main.hs`  or `ghc -dynamic Main.hs` if packages dynamically linked

Run:
`./Main`

## Packages used (docs):

[Graphics.Image](http://hackage.haskell.org/package/hip-1.5.3.0/docs/Graphics-Image.html)

[Data.Complex](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Complex.html)

[Data.Time.Clock](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)

[GLFW-B](https://hackage.haskell.org/package/GLFW-b)

[Graphics.UI.GLFW](https://hackage.haskell.org/package/GLFW)
