{- |
    Helper functions for dealing with OpenGL buffer objects.
-}
module VBO (
    createVBO
) where
import Foreign.Storable
import Data.Array.Storable
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT (reportErrors)

-- | Creates a 'BufferObject' for the given list of 'Storable' elements.
-- 
--   E.g. @my_vbo \<- createVBO [1,2,3,4::GLfloat]@
createVBO :: Storable a => [a] -> IO BufferObject
createVBO elems = do
    [vbo] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just vbo
    arr <- newListArray (0, length elems - 1) elems -- Data.Array.MArray
    withStorableArray arr (\ptr ->                  -- Data.Array.Storable
        bufferData ArrayBuffer $= (ptrsize elems, ptr, StaticDraw))
    bindBuffer ArrayBuffer $= Nothing
    reportErrors
    return vbo
    where ptrsize [] = toEnum 0
          ptrsize (x:xs) = toEnum $ length elems * (sizeOf x)
          offset x = plusPtr nullPtr x
