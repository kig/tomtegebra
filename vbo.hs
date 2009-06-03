module VBO where
import Data.Array.Storable
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

type Vert3 = (GLfloat, GLfloat, GLfloat)
type Tex2 = (GLfloat, GLfloat)
type Model a b = (PrimitiveMode, NumArrayIndices, BufferObject, VertexArrayDescriptor a, VertexArrayDescriptor b)

createModel :: PrimitiveMode -> [Vert3] -> [Tex2] -> IO (Model GLfloat GLfloat)
createModel vertType verts texCoords = do
    vbo <- createVBO elems
    return (vertType, fromIntegral vertCount, vbo, vertsDesc, texCoordsDesc)
    where vertCount = min (length verts) (length texCoords)
          elems = foldl1 (++) $ zipWith con verts texCoords
          vboStride = 5*4
          vertsDesc = VertexArrayDescriptor 3 Float vboStride $ offset 0
          texCoordsDesc = VertexArrayDescriptor 2 Float vboStride $ offset (3*4)
          con (x,y,z) (s,t) = [x,y,z,s,t]

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