module RenderGame where
import Game
import Algebra

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Matrix
import VBO
import Models

drawVictory :: IO ()
drawVictory = putStrLn "VICTORY!!! HUZZAH!!! BANZAI!!!"

drawLevel :: AppState -> IO ()
drawLevel st =
    let equ = equation st
        cloc = cursorLocation st
        inv = inventory st
        invIdx = inventoryIndex st in do
    drawInventory (inventoryFor cloc equ inv) (model st) invIdx
    drawEquation equ (model st) cloc

drawEquation :: CheckableRule -> Model -> Int -> IO ()
drawEquation (_,rule) models idx = drawExpr (toExpr rule) models idx

drawExpr :: Expr -> Model -> Int -> IO ()
drawExpr e models idx = drawScene $ buildScene models e idx

drawScene = mapM_ drawTransformModel

drawTransformModel (transform, model) = do
    glLoadMatrix transform
    drawModel model

buildScene model expr@(Expr (o, l, r)) idx = [(identityMatrix, model)]
buildScene model (Inv (o, e)) idx = [(identityMatrix, model)]
buildScene model (Neutral o) idx = [(identityMatrix, model)]
buildScene model A idx = [(identityMatrix, model)]
buildScene model B idx = [(identityMatrix, model)]
buildScene model C idx = [(identityMatrix, model)]
buildScene model (Literal i) idx = [(identityMatrix, model)]

drawInventory :: ProofInventory -> Model -> Int -> IO ()
drawInventory inventory models idx =
    let idx' = idx `mod` length inventory in do
    putStrLn "\nInventory:"
    mapM_ (putStrLn.show) $ take idx' inventory
    putStrLn $ "--" ++ show (inventory !! idx')
    mapM_ (putStrLn.show) $ drop (idx'+1) inventory
