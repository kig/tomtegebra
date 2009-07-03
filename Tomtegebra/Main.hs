{- |
    Tomtegebra is a small puzzle game (built around algebra's group axioms.)

    It uses OpenGL for rendering,
    GLUT for handling windows and events, Gdk.Pixbuf for loading images,
    Cairo for creating textures and Pango for drawing text.
-}
module Main where

import Control.Exception
import Data.IORef
import GHC.Num
import Graphics.UI.GLUT

import System.Exit
import System.Time

import Matrix
import Game
import RenderGame
import Graphics.UI.Gtk.General.General

-- | Sets up Gtk, GLUT and GLUT callbacks, creates the game window,
--   sets some initial GL settings, creates a new game and goes into the main loop.
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

    (_, _) <- getArgsAndInitialize
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

-- | Throws an IO exception to exit the main loop.
exitLoop :: IO ()
exitLoop = throwIO $ ExitException ExitSuccess

-- | Timer to redraw the game window at 60 Hz.
timerProc :: IORef AppState -> IO () -> IO ()
timerProc state m = do
    t0 <- getClockTime
    m
    t1 <- getClockTime
    addTimerCallback (max 1 (16 - elapsed t0 t1)) (timerProc state m)
    where elapsed t0 t1 = fromInteger (elapsedMs t0 t1) :: Int

-- | Integer difference between two 'ClockTime' in milliseconds.
elapsedMs :: ClockTime -> ClockTime -> Integer
elapsedMs t0 t1 = (tdPicosec $ diffClockTimes t1 t0) `quotInteger` 1000000000

-- | Display callback. Clears screen, sets up camera matrix and draws the game.
display :: IORef AppState -> IO ()
display state = do
    clear [ColorBuffer]
    st <- get state
    let w = width st
        h = height st
        pmat = perspectiveMatrix 80 (w/h) 0.1 100
        lookat = lookAtMatrix [0.0, -2.0, 10.0] [0.0, 3.0, 0.0] [0, 1, 0]
        camera = matrixMul pmat lookat
        in do
    if gameOver st
        then drawTitleScreen camera st
        else drawLevel camera st
    swapBuffers

-- | Window reshape callback. Sets viewport to new dimensions, updates the game
--   state width and height and posts a redisplay request.
reshape :: IORef AppState -> Size -> IO ()
reshape state s@(Size w h) = do
    viewport $= (Position 0 0, s)
    st <- get state
    state $= st {width = fromIntegral w, height = fromIntegral h}
    postRedisplay Nothing


-- | Callback for keyboard and mouse events. Sends 'Down' events to 'keyDown'
--   and 'Up' events to 'keyUp'.
keyboardMouse :: IORef AppState -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse appstate key Down _ _ = keyDown appstate key
keyboardMouse appstate key Up _ _ = keyUp appstate key

-- | Handler for keyboard 'Down' events.
--   All the UI logic happens here.
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

keyDown _ (Char 'f') = fullScreen
keyDown _ (Char 'q') = exitLoop
keyDown _ _ = return ()

-- | Empty handler for keyboard 'Up' events.
keyUp :: IORef AppState -> Key -> IO ()
keyUp _ _ = return ()

-- | Maps an IORef through a function.
mapRef :: (a -> a) -> IORef a -> IO ()
mapRef f a = modifyIORef a f
