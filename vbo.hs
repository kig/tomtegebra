module VBO where
import Data.Array.Storable
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

createVBO elems = do
    [vbo] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just vbo
    arr <- newListArray (0, length elems - 1) elems -- Data.Array.MArray
    withStorableArray arr (\ptr ->                  -- Data.Array.Storable
        bufferData ArrayBuffer $= (ptrsize, ptr, StaticDraw))
    bindBuffer ArrayBuffer $= Nothing
    reportErrors
    return vbo
    where ptrsize = toEnum $ length elems * 4       -- GLfloat = 4 bytes

offset x = plusPtr nullPtr x