import Prelude as P
import Graphics.Image as I
import Data.Complex as C
import Data.Time.Clock.POSIX
main = do
    x <- getInt "Input an integral x size"
    y <- getInt "Input an integral y size"
    itr <- getInt "How many iterations when creating Julia Set"
    time <- round `fmap` getPOSIXTime
    let c = (0.3) :+ (0.01) :: Complex Double
    let jSet = makeImageR RPU (x, y) (\(i, j) -> PixelY $ if inSet ((fromIntegral i)*2/(fromIntegral x), (fromIntegral j)*2/(fromIntegral y)) c itr then 0 else 1) :: Image RPU Y Double
    writeImage ("../out/" ++ (show time) ++ ".png") jSet

getInt :: (Read b, Integral b) => String -> IO b
getInt s = do
    putStrLn s
    i <- getLine
    return (read i)

-- Returns True if given pixel (x,y) is in the Julia set, false otherwise
inSet :: (Double,Double) -> Complex Double -> Int -> Bool
inSet (x,y) c itr = C.magnitude(juliaIterate (x :+ y) c itr) < 2

-- Updates the complex number Z itr times according to z = z^2 + c
juliaIterate :: Complex Double -> Complex Double -> Int -> Complex Double
juliaIterate z c 0 = z
juliaIterate z c itr = juliaIterate (z^2 + c) c (itr-1)
