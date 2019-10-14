import Prelude as P
import Graphics.Image as I
import Data.Complex as C
import Data.Time.Clock.POSIX

-- Main function that takes input from user and writes a png image in the out folder
main = do
    x <- getInt "Input an integral width"
    y <- getInt "Input an integral height"
    itr <- getInt "Input an integral number of iterations to update z = z^2 + c"
    time <- round `fmap` getPOSIXTime
    -- C value for iterating function z = z^2 + c
    -- Changing this value will change the fractal
    -- TODO: Allow this to be set by the user from the commandline or a UI
    let c = (-0.7269) :+ (0.1889) :: Complex Double
    -- jSet is the image array produced by outputting black (0) if the pixel is in the set and white (1) otherwise
    let jSet = makeImageR VU (x, y) (\(i, j) -> if inSet (normalizer (x,y) (i,j)) c itr
                                     then PixelRGB ((fromIntegral i)/(fromIntegral x)) ((fromIntegral j)/(fromIntegral y)) (fromIntegral 0)
                                     else PixelRGB (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)) :: Image VU RGB Double
    writeImage ("../out/" ++ (show time) ++ ".png") jSet

-- Normalizes the pixel (i,j) (i-th row, j-th column) from -2 to 2 given the total dimension of the output (x,y) where there are
-- x columns and y rows
normalizer :: (Int, Int) -> (Int, Int) -> (Double, Double)
normalizer (x,y) (i,j) = do
    -- Double versions of x and y for division
    let dx = fromIntegral x  / 2.0 :: Double
    let dy = fromIntegral y / 2.0 :: Double
    (fromIntegral (y-(i*2)) / dy, fromIntegral (x-(j*2)) / dx)

-- Reads an Int from standard in
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
