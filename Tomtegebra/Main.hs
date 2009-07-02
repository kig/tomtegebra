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
import Game
import RenderGame
import Algebra
import Graphics.UI.Gtk.General.General

main :: IO ()
main = do
    initGUI
    initialWindowSize $= Size 900 570
    initialDisplayMode $= [
            RGBAMode,
            DoubleBuffered,
            WithDepthBuffer,
            WithStencilBuffer,
            Multisampling
        ]
    initialDisplayCapabilities $= [
            With DisplaySamples, -- not that freeglut 2.4 has this but eh
            With DisplayDouble
        ]

    (progname, _) <- getArgsAndInitialize
    wnd <- createWindow "Tomtegebra"

    clearColor $= Color4 1 1 1 1

    clear [ColorBuffer]
    multisample $= Enabled
    
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    state <- newGame

    displayCallback $= display state
    reshapeCallback $= Just (reshape state)
    keyboardMouseCallback $= Just (keyboardMouse state)

    addTimerCallback 24 $ timerProc state (display state)

    mainLoop
    destroyWindow wnd

exitLoop :: IO ()
exitLoop = throwIO $ ExitException ExitSuccess

timerProc :: IORef AppState -> IO () -> IO ()
timerProc state m = do
    t0 <- getClockTime
    m
    t1 <- getClockTime
    addTimerCallback (max 1 (16 - elapsed t0 t1)) (timerProc state m)
    where elapsed t0 t1 = fromInteger (elapsedMs t0 t1) :: Int

elapsedMs :: ClockTime -> ClockTime -> Integer
elapsedMs t0 t1 = (tdPicosec $ diffClockTimes t1 t0) `quotInteger` 1000000000

display :: IORef AppState -> IO ()
display state = do
    clear [ColorBuffer]
    st <- get state
    let w = width st
        h = height st
        perspective = perspectiveMatrix 80 (w/h) 0.1 100
        lookat = lookAtMatrix [0.0, -2.0, 10.0] [0.0, 3.0, 0.0] [0, 1, 0]
        camera = matrixMul perspective lookat
        in do
    if gameOver st
        then drawTitleScreen camera st
        else drawLevel camera st
    swapBuffers

reshape :: IORef AppState -> Size -> IO ()
reshape state s@(Size w h) = do
    viewport $= (Position 0 0, s)
    st <- get state
    state $= st {width = fromIntegral w, height = fromIntegral h}
    postRedisplay Nothing


keyboardMouse :: IORef AppState -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse appstate key Down modifiers position = keyDown appstate key
keyboardMouse appstate key Up modifiers position = keyUp appstate key

keyDown :: IORef AppState -> Key -> IO ()
keyDown st (SpecialKey KeyLeft) = mapRef moveCursorLeft st
keyDown st (SpecialKey KeyRight) = mapRef moveCursorRight st

keyDown st (SpecialKey KeyDown) = mapRef scrollInventoryDown st
keyDown st (SpecialKey KeyUp) = mapRef scrollInventoryUp st

keyDown st (Char ' ') = do
    sta <- get st
    let sta' = if gameOver sta then nextLevel $ resetGame sta else sta
        sta'' = if equationCompleted sta' then nextEquation sta' else applyCurrentRule sta'
        in do
    st $= sta''

keyDown st (Char 'f') = fullScreen
keyDown st (Char 'q') = exitLoop
keyDown _ _ = return ()

keyUp :: IORef AppState -> Key -> IO ()
keyUp _ _ = return ()

mapRef :: (a -> a) -> IORef a -> IO ()
mapRef f a = do
    v <- get a
    a $= f v
