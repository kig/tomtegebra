module Models where
import Graphics.Rendering.OpenGL
import VBO
import Texture
import Matrix

type Vert3 = (GLfloat, GLfloat, GLfloat)
type Tex2 = (GLfloat, GLfloat)
data Model = Model {
    mode::PrimitiveMode,
    count::NumArrayIndices,
    vbo::BufferObject,
    textures::[TextureObject],
    verts::VertexArrayDescriptor GLfloat,
    texCoords::VertexArrayDescriptor GLfloat}

fillScreen :: IO ()
fillScreen = do
    glLoadMatrix identityMatrix
    renderPrimitive TriangleFan $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) quadVerts

drawModel :: Model -> IO ()
drawModel model = do
    bindBuffer ArrayBuffer $= Just (vbo model)
    clientState VertexArray $= Enabled
    arrayPointer VertexArray $= verts model
    clientState TextureCoordArray $= Enabled
    arrayPointer TextureCoordArray $= texCoords model
    useTexture (textures model)
    drawInstance model

drawInstance :: Model -> IO ()
drawInstance model = drawArrays (mode model) 0 (count model)

createModel :: PrimitiveMode -> [Vert3] -> [Tex2] -> IO Model
createModel vertType verts texCoords = do
    vbo <- createVBO elems
    return Model { mode=vertType
                 , count=fromIntegral vertCount
                 , vbo=vbo
                 , textures=[]
                 , verts=vertsDesc
                 , texCoords=texCoordsDesc}
    where vertCount = min (length verts) (length texCoords)
          elems = foldl1 (++) $ zipWith con verts texCoords
          vboStride = 5*4
          vertsDesc = VertexArrayDescriptor 3 Float vboStride $ offset 0
          texCoordsDesc = VertexArrayDescriptor 2 Float vboStride $ offset (3*4)
          con (x,y,z) (s,t) = [x,y,z,s,t]

hexVerts :: [(GLfloat,GLfloat,GLfloat)]
hexVerts = map (\k -> (sin(2*pi*k/6), cos(2*pi*k/6), 0.0)) [1..6]

hexTexCoords :: [(GLfloat, GLfloat)]
hexTexCoords = map (\(x,y,z) -> ((x+1.0)*0.5, 1.0-(y+1.0)*0.5)) hexVerts

createHexModel = createModel TriangleFan hexVerts hexTexCoords

quadVerts :: [(GLfloat, GLfloat, GLfloat)]
quadVerts = [(-1,-1,0), (-1,1,0), (1,1,0), (1,-1,0)]
quadTexCoords :: [(GLfloat, GLfloat)]
quadTexCoords = [(0,0), (0,1), (1,1), (1,0)]

createQuadModel = createModel TriangleFan quadVerts quadTexCoords
