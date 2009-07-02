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
type Texts = [(String, (Int,Int,Model))]

-- | Draws the current level (i.e. the equation and matching part of inventory)
--   transformed by pMatrix.
drawLevel :: Matrix4x4 -> AppState -> IO ()
drawLevel pMatrix st =
    let equ = equation st
        cloc = cursorLocation st
        inv = inventory st
        invIdx = inventoryIndex st in do
    drawInventory pMatrix (inventoryFor cloc equ inv) (models st) invIdx
    drawCheckableRule pMatrix equ (texts st) (models st) cloc

-- | Draws a CheckableRule transformed by m, with the given models, equation
--   cursor drawn at idx. Wrapper around drawRule (and drawExpr.)
drawCheckableRule :: Matrix4x4 -> CheckableRule -> Texts -> Models -> Int -> IO ()
drawCheckableRule m ((name,pred),rule) texts models idx = do
    drawName m texts name
    drawRule m rule models idx

drawName :: Matrix4x4 -> Texts -> String -> IO ()
drawName m texts name = do
    glLoadMatrix $ matrixMul m sm
    drawModel model
    where (w,h,model) = lookupOrFirst name texts
          sm = matrixMul (scalingMatrix [0.5*ratio, 0.5, 0.5])
                         (translationMatrix [-0.5, 2.3, 0])
          ratio = fromIntegral w / fromIntegral h

-- | Draws a Rule transformed by m, with the given models, equation
--   cursor drawn at idx. Wrapper around drawExpr.
drawRule :: Matrix4x4 -> Rule -> Models -> Int -> IO ()
drawRule m rule models idx = drawExpr m (toExpr rule) models idx

-- | Draws an Expr transformed by m, with the given models, equation
--   cursor drawn at idx.
--   First calls buildScene to create the draw list, then draws it using drawScene.
drawExpr :: Matrix4x4 -> Expr -> Models -> Int -> IO ()
drawExpr m e models idx = drawScene m $ buildScene models e idx

-- | Draws the Scene transformed by m.
--   Maps over the Scene with drawTransformModel.
drawScene :: Matrix4x4 -> Scene -> IO ()
drawScene m = mapM_ (drawTransformModel m)

-- | Draws a (transform, Model) -pair, transformed by m.
--   First loads the matrix (m x transform), then draws the model.
drawTransformModel :: Matrix4x4 -> (Matrix4x4, Model) -> IO ()
drawTransformModel m (transform, model) = do
    glLoadMatrix (matrixMul m transform)
    drawModel model

-- | Creates the Scene for an Expr, using Models, cursor drawn at position idx.
--   The created Scene is centered around the root operator.
buildScene :: Models -> Expr -> Int -> Scene
buildScene models expr@(Expr (o, l, r)) idx =
    move (-leftWidth-centerWidth/2) 0 leftModel ++ 
    move (-centerWidth/2) 0 centerModel ++
    move (centerWidth/2) 0 rightModel
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

-- | Returns the neutral model for an operator.
--   The neutral model is the "E" model over the operator model.
neutral :: Models -> (Int, GLfloat, Scene) -> (Int, GLfloat, Scene)
neutral models (i,w, (m,opm):xs) =
    (i,w, (m,nm):(m2,opm):xs)
    where nm = lookupOrFirst "E" models
          ms = scalingMatrix [0.75, 0.75, 0.75]
          m2 = matrixMul (translationMatrix [0.125, 0.125, 0.0]) ms

-- | Returns the inverse model for an operator and variable.
--   The inverse model is the "I" model over the variable model,
--   with a small version of the operator model in the bottom-right corner.
inverse :: Models -> Op -> (Int, GLfloat, Scene) -> (Int, GLfloat, Scene)
inverse models o (i,w, (m,eqM):xs) =
    (i,w, (m,iM):(m2,eqM):(m3,opM):xs)
    where iM = lookupOrFirst "I" models
          opM = lookupOrFirst o models
          ms = scalingMatrix [0.75, 0.75, 0.75]
          m2 = matrixMul (translationMatrix [0.125, 0.125, 0.0]) ms
          ms2 = scalingMatrix [0.5, 0.5, 0.5]
          m3 = matrixMul (translationMatrix [0.5, -0.125, 0.0]) ms2

-- | Returns the model for an operator.
opModel :: Models -> Op -> Int -> (Int, GLfloat, Scene)
opModel models o idx = (idx-1, 1, getModel models o idx)

-- | Returns the Scene for a model in Models matching the key k.
--   Appends the cursor model to the scene if idx is zero.
--   The cursor model is the model with key ">".
getModel :: Models -> String -> Int -> Scene
getModel models k idx = 
    [(identityMatrix, lookupOrFirst k models)] ++ cursor
    where cursor = if idx == 0 then [(identityMatrix, lookupOrFirst ">" models)]
                               else []

-- | Looks up the first match to k in the association list lst.
--   Returns the first element value of lst if no match is found for k.
lookupOrFirst :: Eq a => a -> [(a, b)] -> b
lookupOrFirst k lst = maybe (snd $ head lst) id (lookup k lst)

-- | Translates the given Scene by (x,y,0).
move :: GLfloat -> GLfloat -> Scene -> Scene
move x y = map (\(tr, model) -> (matrixMul m tr, model))
           where m = translationMatrix [x, y, 0]

-- | Draws the inventory transformed by the camera matrix, using the given models.
--   The inventory is scrolled to the cursor position given by idx.
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
          camerat = matrixMul camera (translationMatrix [0, 3.2, 0])

-- | Draws an inventory entry transformed by the camera matrix.
--   See drawInventoryRule.
drawInventoryEntry :: Matrix4x4 -> Models -> Int -> (GLfloat, Rule) -> IO ()
drawInventoryEntry camera models cursorIdx (offset, rule) =
    drawInventoryRule matrix rule models cursorIdx
    where matrix = matrixMul camera tr
          tr = translationMatrix [0, 4*offset, 0]

-- | Returns the length of the left side of an inventory rule.
--   The left length of equality transforms is the length of the left side of
--   the left-side equality (equality transforms are of form (A+C=B+C) = (A=B).)
leftLength :: Rule -> Int
leftLength (Rule (Expr ("=",l,_), Expr ("=",_,_))) = exprLength l
leftLength (Rule (l, r)) = exprLength l

-- | Draws the inventory rule, transformed by m.
--   Equality transforms have only their left side drawn for clarity (e.g.
--   from (A+C = B+C) = (A = B), only A+C = B+C is drawn.)
--   See drawRule.
drawInventoryRule :: Matrix4x4 -> Rule -> Models -> Int -> IO ()
drawInventoryRule m (Rule (r, Expr("=",_,_))) models idx =
    drawRule m (maybe (Rule (A,A)) id (toRule r)) models idx
drawInventoryRule m rule models idx = drawRule m rule models idx