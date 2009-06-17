module VBO where
import Foreign.Storable
import Data.Array.Storable
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

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