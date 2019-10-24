import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
    GLFW.init
    GLFW.defaultWindowHints
    Just win <- GLFW.createWindow 800 600 "Julia Set Visualizer" Nothing Nothing
    GLFW.makeContextCurrent (Just win)

    forever $ do
        GLFW.pollEvents
        GLFW.swapBuffers win