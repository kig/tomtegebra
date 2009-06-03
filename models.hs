module Models where
import Graphics.Rendering.OpenGL
import VBO
import Texture
import Matrix

fillScreen = do
    glLoadMatrix identityMatrix
    renderPrimitive TriangleFan $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) quadVerts

drawModel v@(vboType, vboSize, vbo, verts, texCoords) tex = do
    bindBuffer ArrayBuffer $= Just vbo
    clientState VertexArray $= Enabled
    arrayPointer VertexArray $= verts
    useTexture texCoords tex
    drawInstance v

drawInstance (vboType, vboSize, _,_,_) = drawArrays vboType 0 vboSize


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
