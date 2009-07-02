module Models where
import Graphics.Rendering.OpenGL
import VBO
import Texture
import Foreign.Ptr (Ptr, castPtr)
import Matrix
import Data.Int
import Data.IORef
import System.IO.Unsafe
import Foreign.Ptr

type Vert3 = (GLfloat, GLfloat, GLfloat)
type Tex2 = (GLfloat, GLfloat)
data Model = Model {
    mode::PrimitiveMode,
    count::NumArrayIndices,
    vbo::BufferObject,
    textures::[TextureObject],
    verts::VertexArrayDescriptor GLfloat,
    texCoords::VertexArrayDescriptor GLfloat}

data Vbo a = Vbo (NumArrayIndices, BufferObject, (IntegerHandling, VertexArrayDescriptor a))
data Stream a = Stream (AttribLocation, Vbo a)
data Sampler = Sampler (UniformLocation, TextureTarget, TextureObject)

data UniformSetting = UniformSetting (UniformLocation, UniformValue)

data UniformValue =
      UniformMatrix4 (Matrix4x4)
    | UniformVertex4 (Vertex4 GLfloat)
    | UniformVertex3 (Vertex3 GLfloat)
    | UniformVertex2 (Vertex2 GLfloat)
    | UniformFloat (TexCoord1 GLfloat)
    | UniformInt (TexCoord1 GLint)

data Drawable = Drawable {
    program    :: Program,
    uniforms   :: [UniformSetting],
    streamMode :: PrimitiveMode,
    streams    :: [Stream GLfloat],
    indices    :: Maybe (Vbo GLushort),
    samplers   :: [Sampler]
    }

uniformSetting :: UniformSetting -> IO ()
uniformSetting (UniformSetting(location, UniformMatrix4 value)) =
    withMatrix4x4 value (\order ptr -> uniformv location 16 (castPtr ptr :: Ptr (TexCoord1 GLfloat)))
uniformSetting (UniformSetting(location, UniformVertex4 value)) = uniform location $= value
uniformSetting (UniformSetting(location, UniformVertex3 value)) = uniform location $= value
uniformSetting (UniformSetting(location, UniformVertex2 value)) = uniform location $= value
uniformSetting (UniformSetting(location, UniformFloat value)) = uniform location $= value
uniformSetting (UniformSetting(location, UniformInt value)) = uniform location $= value

drawDrawable :: Drawable -> IO ()
drawDrawable d = do
    currentProgram $= Just (program d)
    setUniforms (uniforms d)
    setSamplers (samplers d)
    withStreams (streams d) (do
        case indices d of
            Just (Vbo (num, vbo, (_,VertexArrayDescriptor numcomp datatype stride ptr))) -> do
                bindBuffer ElementArrayBuffer $= Just vbo
                drawElements (streamMode d) num datatype ptr
                bindBuffer ElementArrayBuffer $= Nothing
            Nothing -> drawArrays (streamMode d) 0 (minNum (streams d)))
    currentProgram $= Nothing
    where minNum streams = minimum $ map (\(Stream (_,Vbo(n,_,_))) -> n) streams

withStreams :: [Stream a] -> IO () -> IO ()
withStreams streams m = do
    setStreams streams
    m
    disableStreams streams

setStreams :: [Stream a] -> IO ()
setStreams streams = 
    mapM_ (\(Stream (location, Vbo (_, vbo, value))) -> do
            bindBuffer ArrayBuffer $= Just vbo
            vertexAttribArray location $= Enabled
            vertexAttribPointer location $= value) streams

disableStreams :: [Stream a] -> IO ()
disableStreams streams =
    mapM_ (\(Stream (location,_)) -> vertexAttribArray location $= Disabled) streams

setUniforms :: [UniformSetting] -> IO ()
setUniforms uniforms = mapM_ uniformSetting uniforms

setSamplers :: [Sampler] -> IO ()
setSamplers samplers =
    mapM_ (\(i, Sampler (location, texType, tex)) -> do
            activeTexture $= TextureUnit i
            textureBinding texType $= Just tex
            uniform location $= TexCoord1 i) $ zip [0..] samplers

fillScreen :: IO ()
fillScreen = do
    glLoadMatrix identityMatrix
    texture Texture2D $= Disabled
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
          offset x = plusPtr nullPtr x

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

triVerts :: [(GLfloat, GLfloat, GLfloat)]
triVerts = [(-1,-1,0), (0,1,0), (1,-1,0)]
triTexCoords :: [(GLfloat, GLfloat)]
triTexCoords = [(0,0), (0.5,1), (1,0)]

createTriModel = createModel Triangles triVerts triTexCoords

quadImageVerts :: [(GLfloat, GLfloat, GLfloat)]
quadImageVerts = [(0,0,0), (0,1,0), (1,1,0), (1,0,0)]

quadImageTexCoords :: [(GLfloat, GLfloat)]
quadImageTexCoords = [(0,1), (0,0), (1,0), (1,1)]

createQuadImageModel = createModel TriangleFan quadImageVerts quadImageTexCoords

imageModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE imageModel #-}

getCachedImageModel = do
    im <- get imageModel
    case im of
        Just q -> return q
        Nothing -> do
            iq <- createQuadImageModel
            imageModel $= Just iq
            return iq

createImageModel fn = do
    (w,h,tex) <- loadTexture fn
    q <- getCachedImageModel
    return (w,h,q {textures = [tex]})

createTextModel alignment markup = do
    (w,h,tex) <- createTextTexture alignment markup
    q <- getCachedImageModel
    return (w,h,q {textures = [tex]})
    
