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
drawEquation m (_,rule) models idx = drawRule m rule models idx

drawRule :: Matrix4x4 -> Rule -> Models -> Int -> IO ()
drawRule m rule models idx = drawExpr m (toExpr rule) models idx

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
          (centerIdx, centerWidth, centerModel) = opModel models o leftIdx
          (leftIdx, leftWidth, leftModel) = buildScene' models l idx
          (rightIdx, rightWidth, rightModel) = buildScene' models r centerIdx

buildScene' :: Models -> Expr -> Int -> (Int, GLfloat, Scene)
buildScene' models expr@(Expr (o, l, r)) idx = 
    (rightIdx, totalWidth, drawList)
    where drawList = move 0 (-1) leftModel ++ 
                     move leftWidth 0 centerModel ++
                     move (leftWidth+centerWidth) (-1) rightModel
          totalWidth = centerWidth + leftWidth + rightWidth
          (centerIdx, centerWidth, centerModel) = opModel models o leftIdx
          (leftIdx, leftWidth, leftModel) = buildScene' models l idx
          (rightIdx, rightWidth, rightModel) = buildScene' models r centerIdx

buildScene' models (Inv (o, e)) idx = inverse models o $ buildScene' models e idx
buildScene' models (Neutral o) idx = neutral models $ opModel models o idx
buildScene' models A idx = (idx-1, 1, getModel models "A" idx)
buildScene' models B idx = (idx-1, 1, getModel models "B" idx)
buildScene' models C idx = (idx-1, 1, getModel models "C" idx)
buildScene' models X idx = (idx-1, 1, getModel models "X" idx)
buildScene' models Y idx = (idx-1, 1, getModel models "Y" idx)
buildScene' models Z idx = (idx-1, 1, getModel models "Z" idx)
buildScene' models (Literal i) idx = (idx-1, 1, getModel models "L" idx)

neutral :: Models -> (Int, GLfloat, Scene) -> (Int, GLfloat, Scene)
neutral models (i,w, (m,opm):xs) =
    (i,w, (m,nm):(m2,opm):xs)
    where nm = lookupOrFirst "E" models
          ms = scalingMatrix [0.75, 0.75, 0.75]
          m2 = matrixMul (translationMatrix [0.125, 0.125, 0.0]) ms

inverse :: Models -> Op -> (Int, GLfloat, Scene) -> (Int, GLfloat, Scene)
inverse models o (i,w, (m,eqM):xs) =
    (i,w, (m,iM):(m2,eqM):(m3,opM):xs)
    where iM = lookupOrFirst "I" models
          opM = lookupOrFirst o models
          ms = scalingMatrix [0.75, 0.75, 0.75]
          m2 = matrixMul (translationMatrix [0.125, 0.125, 0.0]) ms
          ms2 = scalingMatrix [0.5, 0.5, 0.5]
          m3 = matrixMul (translationMatrix [0.5, -0.125, 0.0]) ms2

opModel :: Models -> Op -> Int -> (Int, GLfloat, Scene)
opModel models o idx = (idx-1, 1, getModel models o idx)

getModel :: Models -> String -> Int -> Scene
getModel models k idx = 
    [(identityMatrix, lookupOrFirst k models)] ++ cursor
    where cursor = if idx == 0 then [(identityMatrix, lookupOrFirst ">" models)]
                               else []

lookupOrFirst :: Eq a => a -> [(a, b)] -> b
lookupOrFirst k lst = maybe (snd $ head lst) id (lookup k lst)

move :: GLfloat -> GLfloat -> Scene -> Scene
move x y = map (\(tr, model) -> (matrixMul m tr, model))
           where m = translationMatrix [x, y, 0]

drawInventory :: Matrix4x4 -> ProofInventory -> Models -> Int -> IO ()
drawInventory camera [] models idx = return ()
drawInventory camera inventory models idx = do
    drawInventoryEntry cameran models lLength (0, selected)
    mapM_ (drawInventoryEntry cameran models (-1)) $ zip [1..] tlst
    mapM_ (drawInventoryEntry cameran models (-1)) $ zip [1+tlen..] rlst
    where idx' = idx `mod` len
          len = length inventory
          selected = inventory !! idx'
          lLength = leftLength selected
          tlen = fromIntegral $ length tlst :: GLfloat
          rlst = reverse $ drop (idx'+1) inventory
          tlst = reverse $ take idx' inventory
          cameran = matrixMul camerat (scalingMatrix [0.66, 0.66, 0.66])
          camerat = matrixMul camera (translationMatrix [0, 3, 0])

drawInventoryEntry :: Matrix4x4 -> Models -> Int -> (GLfloat, Rule) -> IO ()
drawInventoryEntry camera models cursorIdx (offset, rule) =
    drawInventoryRule matrix rule models cursorIdx
    where matrix = matrixMul camera tr
          tr = translationMatrix [0, 4*offset, 0]

leftLength :: Rule -> Int
leftLength (Rule (Expr ("=",l,_), Expr ("=",_,_))) = exprLength l
leftLength (Rule (l, r)) = exprLength l

drawInventoryRule :: Matrix4x4 -> Rule -> Models -> Int -> IO ()
drawInventoryRule m (Rule (r, Expr("=",_,_))) models idx =
    drawRule m (maybe (Rule (A,A)) id (toRule r)) models idx
drawInventoryRule m rule models idx = drawRule m rule models idx