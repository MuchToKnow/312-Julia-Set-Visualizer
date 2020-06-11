# Haskell-Julia-Set-Visualizer

![Sample animation (using juliaShader3.fs.glsl as the fragment shader)](https://external-sea1-1.xx.fbcdn.net/safe_image.php?d=AQA89pSZ_m1E_yj8&url=https%3A%2F%2Fthumbs.gfycat.com%2FUnsightlyJampackedGnu-size_restricted.gif&ext=gif&_nc_hash=AQAOxy9Xi8eDFup6)

## How to Run simple image generator:

cd to directory:
`cd src`

Install deps:
`cabal update && cabal install hip`

To Run without building:
`./Visualizer`

To Build and Run (Do this if you have made changes to the source Visualizer.hs):
`ghc Visualizer.hs && ./Visualizer`

Images will be written as a .png with an epoch timestamp as their name.

![Sample image](https://raw.githubusercontent.com/MuchToKnow/312-Julia-Set-Visualizer/master/out/1571887196.png)

## How to Run animated version (openGL):

We provide a couple different shaders to experiment with in the OpenGLsrc directory that create different effects.  
They are loaded on lines 43 (Vertex shader) and 45 (Fragment shader) if you would like to replace them.

cd to directory:
`cd OpenGLsrc`

Install deps:
`cabal install Graphics.UI.GLFW && cabal install GLFW-b`

Build:
`ghc Main.hs`  or `ghc -dynamic Main.hs` if packages dynamically linked

Run:
`./Main`

![Sample animation (using juliaShader3.fs.glsl as the fragment shader)](https://external-sea1-1.xx.fbcdn.net/safe_image.php?d=AQA89pSZ_m1E_yj8&url=https%3A%2F%2Fthumbs.gfycat.com%2FUnsightlyJampackedGnu-size_restricted.gif&ext=gif&_nc_hash=AQAOxy9Xi8eDFup6)

## Packages used (docs):

[Graphics.Image](http://hackage.haskell.org/package/hip-1.5.3.0/docs/Graphics-Image.html)

[Data.Complex](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Complex.html)

[Data.Time.Clock](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)

[GLFW-B](https://hackage.haskell.org/package/GLFW-b)

[Graphics.UI.GLFW](https://hackage.haskell.org/package/GLFW)
