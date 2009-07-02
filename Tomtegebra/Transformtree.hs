module TransformTree where
import Graphics.Rendering.OpenGL
import Matrix
import Models

data TreeNode = TreeNode {
    relMatrix :: Matrix4x4,
    models :: [Model],
    children :: [TreeNode]
  }

data DrawNode = DrawNode {
    absMatrix :: Matrix4x4,
    model :: Model
  }

data DrawList = DrawList {
    sortFunc :: [DrawNode] -> [DrawNode],
    nodes :: [DrawNode]
  }

drawListsFromTree tree =
  partitionIntoDrawLists drawNodes
  where drawNodes = foldTree identityMatrix tree

partitionIntoDrawLists = id

foldTree mat node =
  foldl (++) modelNodes childNodes
  where curMat = matrixMul mat (relMatrix node)
        modelNodes = map (makeDrawNode curMat) $ models node
        childNodes = map (foldTree curMat) $ children node

makeDrawNode matrix model = DrawNode {absMatrix=matrix, model=model}

drawList dl =
  mapM_ drawNode $ (sortFunc dl) (nodes dl)

drawNode node = do
  glLoadMatrix (absMatrix node)
  drawModel (model node)
