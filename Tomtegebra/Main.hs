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

import Paths_tomtegebra

main :: IO ()
main = do
    initGUI
    initialWindowSize $= Size 720 480
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

    (_,_,cursor) <- loadImage "cursor.png"
    (_,_,mushroom) <- loadImage "mushroom.png"
    (_,_,cherry) <- loadImage "cherry.png"
    (_,_,orange) <- loadImage "orange.png"
    (_,_,emptyGreen) <- loadImage "empty_green.png"
    (_,_,emptyRed) <- loadImage "empty_red.png"
    (_,_,emptyPurple) <- loadImage "empty_purple.png"
    (_,_,literal) <- loadImage "literal.png"
    (_,_,eq) <- loadImage "eq.png"
    (_,_,plusbun) <- loadImage "plusbun.png"
    (_,_,mulbun) <- loadImage "mulbun.png"
    (_,_,cat) <- loadImage "cat.png"
    (_,_,frog) <- loadImage "frog.png"
    (_,_,neutral) <- loadImage "neutral.png"
    (_,_,inverse) <- loadImage "neutral.png"

    tomtegebra <- createTextModel "<span font=\"Trebuchet MS 36\">Tomtegebra</span>"
    pressSpace <- createTextModel "<span font=\"Trebuchet MS 24\">Press space to play</span>"
    bothEqual <- createTextModel "<span font=\"Sans 24\">Make both sides equal</span>"
    bindNeutral <- createTextModel "<span font=\"Sans 24\">Reduce one side to circled element</span>"
    bindInverse <- createTextModel "<span font=\"Sans 24\">Reduce one side to mushroom's inverse</span>"

    state <- newIORef (initGame [("bothEqual", bothEqual)
                                ,("bindNeutral", bindNeutral)
                                ,("bindInverse", bindInverse)
                                ,("pressSpace", pressSpace)
                                ,("title", tomtegebra)]
                                
                                [(">", cursor),
                                 ("A", mushroom), 
                                 ("B", cherry), 
                                 ("C", orange), 
                                 ("X", emptyGreen), 
                                 ("Y", emptyRed), 
                                 ("Z", emptyPurple), 
                                 ("L", literal), 
                                 ("E", neutral), 
                                 ("I", inverse), 
                                 ("=", eq), 
                                 ("+", plusbun), 
                                 ("x", mulbun), 
                                 ("f", frog), 
                                 ("o", cat)])

    displayCallback $= display state
    reshapeCallback $= Just (reshape state)
    keyboardMouseCallback $= Just (keyboardMouse state)

    addTimerCallback 24 $ timerProc state (display state)

    mainLoop
    destroyWindow wnd
    where loadImage fn = do
            dfn <- getDataFileName fn
            createImageModel dfn

exitLoop :: IO ()
exitLoop = throwIO $ ExitException ExitSuccess

timerProc :: IORef AppState -> IO () -> IO ()
timerProc state m = do
    st <- get state
    let t = angle st
        d = dir st in do
    t0 <- getClockTime
    m
    t1 <- getClockTime
    let elapsed = fromInteger (elapsedMs t0 t1) :: Int
    st <- get state
    state $= st {angle = t + d * 0.016}
    addTimerCallback (max 1 (16-elapsed)) (timerProc state m)

elapsedMs :: ClockTime -> ClockTime -> Integer
elapsedMs t0 t1 = (tdPicosec $ diffClockTimes t1 t0) `quotInteger` 1000000000

display :: IORef AppState -> IO ()
display state = do
    st <- get state
    if gameOver st then drawVictory >> exitLoop else return ()
    clear [ColorBuffer]
    let t = angle st
        w = width st
        h = height st
        perspective = perspectiveMatrix 80 (w/h) 0.1 100 
        lookat = lookAtMatrix [0.0, -2.0, 10.0] [0.0, 3.0, 0.0] [0, 1, 0] 
        camera = matrixMul perspective lookat
        in do
--     drawBackground camera st (tomtegebra st)
    drawLevel camera st
    swapBuffers

drawBackground :: Matrix4x4 -> AppState -> (Int, Int, Model) -> IO ()
drawBackground cam st (w,h,hex) = do
    color (Color4 1 1 1 1 :: Color4 GLfloat)
    glLoadMatrix $ matrixMul m (scalingMatrix [1, 1/ratio, 1])
    drawModel hex
    mapM_ (\i -> do
            glLoadMatrix $ matrixMul m (tm i)
            drawInstance hex ) [1..6]
    bindBuffer ArrayBuffer $= Nothing
    where t = angle st
          m = matrixMul cam $ matrixMul (scalingMatrix [5,5,5]) (rotationMatrix (-t) [0.0, 0.3, 1.0])
          tm i = matrixMul (translationMatrix [1.9*cos (2*pi*i/6), 1.9*sin (2*pi*i/6), 0])
                           (scalingMatrix [1, 1/ratio, 1])
          ratio = fromIntegral w / fromIntegral h

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
    flip mapRef st (\sta ->
        let sta' = applyCurrentRule sta in
        if equationCompleted sta'
            then nextEquation sta'
            else sta')

keyDown st (Char 'q') = exitLoop
keyDown _ _ = return ()

keyUp :: IORef AppState -> Key -> IO ()
keyUp _ _ = return ()

mapRef :: (a -> a) -> IORef a -> IO ()
mapRef f a = do
    v <- get a
    a $= f v
