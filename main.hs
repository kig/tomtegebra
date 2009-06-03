-- GLUT Hello World

-- Sets up an animated 60fps render loop with a spinning wheel.
-- The left and right arrow keys control the spin direction.
-- Press q to quit.

-- Demonstrates:
--   - doublebuffered drawing
--   - animation timers
--   - keyboard handling
--   - using VBOs (with an interleaved vertex & texCoord array)
--   - using Textures

import Control.Exception
import Data.IORef
import GHC.Num
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
import System.Time

import Matrix
import VBO
import Models
import Texture

main = do 
    initialWindowSize $= Size 600 600
    initialDisplayMode $= [
            RGBAMode,
            DoubleBuffered,
            WithDepthBuffer,
            WithStencilBuffer,
            Multisampling
        ]
    initialDisplayCapabilities $= [ With DisplaySamples ] -- not that freeglut 2.4 has this but eh

    (progname, _) <- getArgsAndInitialize
    wnd <- createWindow "Hello World"

    direction <- newIORef 1.0
    time <- newIORef 0.0
    width <- newIORef 600
    height <- newIORef 600

    hex <- createHexModel
    tex <- loadTextureFromPNG "tex.png"

    clearColor $= Color4 1 1 1 1

    multisample $= Enabled

    displayCallback $= display width height hex tex time
    reshapeCallback $= Just (reshape width height)
    keyboardMouseCallback $= Just (keyboardMouse direction)

    addTimerCallback 24 $ timerProc direction time (display width height hex tex)

    mainLoop
    destroyWindow wnd

exitLoop = throwIO $ ExitException ExitSuccess

timerProc direction time m = do
    d <- get direction
    t <- get time
    t0 <- getClockTime
    m time
    t1 <- getClockTime
    let elapsed = fromInteger (elapsedMs t0 t1) :: Int
    time $= t + d * 0.016
    addTimerCallback (max 1 (16-elapsed)) (timerProc direction time m)

elapsedMs t0 t1 = (tdPicosec $ diffClockTimes t1 t0) `quotInteger` 1000000000

display width height hex tex time = do
    t <- get time
    w <- get width
    h <- get height
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    color $ (Color4 1 1 1 0.2 :: Color4 GLfloat)
    texture Texture2D $= Disabled
    fillScreen
    blend $= Disabled
    let m = axleMatrix w h t in do
        glLoadMatrix m
        drawModel hex (Just tex)
        mapM_ (\i -> do
                glLoadMatrix $ matrixMul m (tm i)
                drawInstance hex ) [1..6]
    bindBuffer ArrayBuffer $= Nothing
    swapBuffers
    where axleMatrix w h t = matrixMul (cameraMatrix w h) $ rotationMatrix (-t) [0.0, 0.3, 1.0]
          cameraMatrix w h = matrixMul (p w h) l
          tm i = translationMatrix [1.9*cos (2*pi*i/6), 1.9*sin (2*pi*i/6), 0]
          p w h = perspectiveMatrix 30 (w/h) 0.1 100
          l = lookAtMatrix [4.0, 1.0, 3.0] [0.0, 0.0, 0.0] [0, 1, 0]

reshape width height s@(Size w h) = do
    viewport $= (Position 0 0, s)
    width $= fromIntegral w
    height $= fromIntegral h
    clear [ColorBuffer]
    postRedisplay Nothing

keyboardMouse direction key state modifiers position = do
    keyboardAct direction key state

keyboardAct dir (SpecialKey KeyLeft) Down = dir $= (-1.0)
keyboardAct dir (SpecialKey KeyRight) Down = dir $= 1.0
keyboardAct dir (Char 'q') Down = exitLoop
keyboardAct dir (Char ' ') Down = dir $= 0.0
keyboardAct _ _ _ = return ()



