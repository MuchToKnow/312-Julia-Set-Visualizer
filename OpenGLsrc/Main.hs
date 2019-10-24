import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Foreign.Marshal.Array               (withArray)
import Foreign.Storable                    (sizeOf)
import Foreign.Ptr                         (plusPtr, nullPtr, Ptr)
import System.Exit (exitWith, ExitCode(..) )

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

type Point      = (Double, Double)

toVertex4 :: Point -> Vertex4 Double
toVertex4 (k, l)   = Vertex4 k l 0 1

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _ _ _ _ _ = return () -- if we want to add other key responses

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitWith ExitSuccess
    return ()

    

main :: IO ()
main = do
    GLFW.init
    GLFW.defaultWindowHints
    Just win <- GLFW.createWindow 1200 700 "Julia Set Visualizer" Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setKeyCallback win (Just keyPressed)
    
    GLFW.swapInterval 1 -- vsync

    -- Load Shaders: juliaShader.frag, juliaShader.vert
    vss <- getSource "juliaShader.vs.glsl"
    vs <- GL.createShader GL.VertexShader
    fss <- getSource "juliaShader.fs.glsl"
    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS vs $= vss
    GL.shaderSourceBS fs $= fss
    GL.compileShader vs
    GL.compileShader fs
    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs
    GL.linkProgram program
    GL.currentProgram $= (Just program)

    -- Set the entire screen as the thing to draw on
    let points = [(0.5,0.5),(-0.5,0.5),(-0.5,-0.5),(0.5,-0.5)]
    let vertices = map toVertex4 points
    let indices = [0,1,3, 1,2,3]
    let numVertices = length vertices
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    --
    -- Declaring VBO: vertices
    --

    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vertices))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 Double 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    GL.clearColor $= Color4 0 0 0 1
    

    forever $ do
        GLFW.pollEvents -- check for key presses and respond to them
        GL.clear [ColorBuffer]
        bindVertexArrayObject $= Just triangles
        drawArrays Triangles firstIndex 4
        GLFW.swapBuffers win -- actually display it on the screen


-- Helper functions for loading shaders:
getSource :: String -> IO B.ByteString
getSource path = B.readFile path

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral