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

type Scene = [(Matrix4x4, Model)]
type Models = [(String, Model)]

drawLevel :: Matrix4x4 -> AppState -> IO ()
drawLevel pMatrix st =
    let equ = equation st
        cloc = cursorLocation st
        inv = inventory st
        invIdx = inventoryIndex st in do
    drawInventory pMatrix (inventoryFor cloc equ inv) (models st) invIdx
    drawEquation pMatrix equ (models st) cloc

drawEquation :: Matrix4x4 -> CheckableRule -> Models -> Int -> IO ()
drawEquation m (_,rule) models idx = drawExpr m (toExpr rule) models idx

drawExpr :: Matrix4x4 -> Expr -> Models -> Int -> IO ()
drawExpr m e models idx = drawScene m $ buildScene models e idx

drawScene :: Matrix4x4 -> Scene -> IO ()
drawScene m = mapM_ (drawTransformModel m)

drawTransformModel :: Matrix4x4 -> (Matrix4x4, Model) -> IO ()
drawTransformModel m (transform, model) = do
    glLoadMatrix (matrixMul m transform)
    drawModel model

buildScene :: Models -> Expr -> Int -> Scene
buildScene models expr@(Expr (o, l, r)) idx =
    move (-leftWidth) 0 leftModel ++ 
    move 0 0 centerModel ++
    move centerWidth 0 rightModel
    where totalWidth = centerWidth + leftWidth + rightWidth
          (centerWidth, centerModel) = opModel models o
          (leftWidth, leftModel) = buildScene' models l idx
          (rightWidth, rightModel) = buildScene' models r idx

buildScene' :: Models -> Expr -> Int -> (GLfloat, Scene)
buildScene' models expr@(Expr (o, l, r)) idx = 
    (totalWidth, drawList)
    where drawList = move 0 (-1) leftModel ++ 
                     move leftWidth 0 centerModel ++
                     move (leftWidth+centerWidth) (-1) rightModel
          totalWidth = centerWidth + leftWidth + rightWidth
          (centerWidth, centerModel) = opModel models o
          (leftWidth, leftModel) = buildScene' models l idx
          (rightWidth, rightModel) = buildScene' models r idx
buildScene' models (Inv (o, e)) idx = invert o $ buildScene' models e idx
buildScene' models (Neutral o) idx = neutral $ opModel models o
buildScene' models A idx = (1, [(identityMatrix, getModel models "A")])
buildScene' models B idx = (1, [(identityMatrix, getModel models "B")])
buildScene' models C idx = (1, [(identityMatrix, getModel models "C")])
buildScene' models (Literal i) idx = (1, [(identityMatrix, getModel models "L")])

neutral = id
invert o = id

opModel :: Models -> Op -> (GLfloat, Scene)
opModel models o = (1, [(identityMatrix, getModel models o)])

getModel :: Models -> String -> Model
getModel models k = maybe (snd $ head models) id (lookup k models)

move :: GLfloat -> GLfloat -> Scene -> Scene
move x y = map (\(tr, model) -> (matrixMul tr m, model))
           where m = translationMatrix [x, y, 0]

drawInventory :: Matrix4x4 -> ProofInventory -> Models -> Int -> IO ()
drawInventory camera inventory models idx =
    let idx' = idx `mod` length inventory in do
    putStrLn "\nInventory:"
    mapM_ (putStrLn.show) $ take idx' inventory
    putStrLn $ "--" ++ show (inventory !! idx')
    mapM_ (putStrLn.show) $ drop (idx'+1) inventory
