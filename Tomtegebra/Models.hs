{- |
    Helpers for creating and drawing bundled models.
-}
module Models where

import Graphics.Rendering.OpenGL hiding (Height)
import VBO
import Texture
import Foreign.Ptr (Ptr, castPtr)
import Matrix
import Data.Int
import Data.IORef
import System.IO.Unsafe
import Foreign.Ptr
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Markup (Markup)

-- | 3D Vertex
type Vert3 = (GLfloat, GLfloat, GLfloat)

-- | 2D Texture coordinate
type Tex2 = (GLfloat, GLfloat)

type Width = Int
type Height = Int

-- | Model that bundles a VBO and textures together into a drawable object.
data Model = Model {
    mode::PrimitiveMode,    -- ^Mode of the vertices, e.g. TriangleFan
    count::NumArrayIndices, -- ^Vertex count
    vbo::BufferObject,      -- ^VBO with the vertices and texcoords
    textures::[TextureObject], -- ^List of textures to bind with 'Texture.useTexture'
    verts::VertexArrayDescriptor GLfloat, -- ^Vertex array descriptor for the model vertices
    texCoords::VertexArrayDescriptor GLfloat -- ^Vertex array descriptor for the model texture coordinates
    }

-- | Fills screen with the current color.
--
--   Loads the identity matrix and disables Texture2D.
fillScreen :: IO ()
fillScreen = do
    glLoadMatrix identityMatrix
    texture Texture2D $= Disabled
    renderPrimitive TriangleFan $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) quadVerts

-- | Draws a 'Model' to the screen.
drawModel :: Model -> IO ()
drawModel model = do
    bindBuffer ArrayBuffer $= Just (vbo model)
    clientState VertexArray $= Enabled
    arrayPointer VertexArray $= verts model
    clientState TextureCoordArray $= Enabled
    arrayPointer TextureCoordArray $= texCoords model
    useTexture (textures model)
    drawInstance model

-- | Calls drawArrays on the 'Model' values.
--   Useful for drawing the same model several times without state setting
--   overhead.
drawInstance :: Model -> IO ()
drawInstance model = drawArrays (mode model) 0 (count model)

-- | Creates a model from a list of vertices and texture coords.
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

-- | Vertices for a TriangleFan hexagon model centered on the origin with a radius of 1.
hexVerts :: [(GLfloat,GLfloat,GLfloat)]
hexVerts = map (\k -> (sin(2*pi*k/6), cos(2*pi*k/6), 0.0)) [1..6]

-- | Texture coords for the hexagon model.
hexTexCoords :: [(GLfloat, GLfloat)]
hexTexCoords = map (\(x,y,z) -> ((x+1.0)*0.5, 1.0-(y+1.0)*0.5)) hexVerts

-- | Gets a cached copy of the hexagon model.
getHexModel :: IO Model
getHexModel =
    getCachedModel hexModel (createModel TriangleFan hexVerts hexTexCoords)

-- | Vertices for a TriangleFan quad model centered on the origin with a radius of 1.
quadVerts :: [(GLfloat, GLfloat, GLfloat)]
quadVerts = [(-1,-1,0), (-1,1,0), (1,1,0), (1,-1,0)]

-- | Texture coords for the quad model.
quadTexCoords :: [(GLfloat, GLfloat)]
quadTexCoords = [(0,0), (0,1), (1,1), (1,0)]

-- | Gets a cached copy of the quad model.
getQuadModel :: IO Model
getQuadModel =
    getCachedModel quadModel (createModel TriangleFan quadVerts quadTexCoords)

-- | Vertices for a triangle model centered on the origin with a radius of 1.
triVerts :: [(GLfloat, GLfloat, GLfloat)]
triVerts = [(-1,-1,0), (0,1,0), (1,-1,0)]

-- | Texture coords for the triangle model.
triTexCoords :: [(GLfloat, GLfloat)]
triTexCoords = [(0,0), (0.5,1), (1,0)]

-- | Gets a cached copy of the triangle model.
getTriModel :: IO Model
getTriModel =
    getCachedModel triModel (createModel Triangles triVerts triTexCoords)

-- | Vertices for a TriangleFan quad image model with bottom-left corner on
--   the origin and side length of 1.
quadImageVerts :: [(GLfloat, GLfloat, GLfloat)]
quadImageVerts = [(0,0,0), (0,1,0), (1,1,0), (1,0,0)]

-- | Texture coords for the quad image model, flipped vertically so that y-down
--   images display correctly.
quadImageTexCoords :: [(GLfloat, GLfloat)]
quadImageTexCoords = [(0,1), (0,0), (1,0), (1,1)]

-- | Gets a cached copy of the quad image model.
getQuadImageModel :: IO Model
getQuadImageModel =
    getCachedModel imageModel (createModel TriangleFan quadImageVerts quadImageTexCoords)

-- | Loads an image file as an image model.
createImageModel :: FilePath -> IO (Width, Height, Model)
createImageModel fn = do
    (w,h,tex) <- loadTexture fn
    q <- getQuadImageModel
    return (w,h,q {textures = [tex]})

-- | Creates a text image model from a 'LayoutAlignment' and 'Markup'.
createTextModel :: LayoutAlignment -> Markup -> IO (Width, Height, Model)
createTextModel alignment markup = do
    (w,h,tex) <- createTextTexture alignment markup
    q <- getQuadImageModel
    return (w,h,q {textures = [tex]})


-- model caching

imageModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE imageModel #-}
hexModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE hexModel #-}
triModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE triModel #-}
quadModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE quadModel #-}


getCachedModel ioref create = do
    im <- get ioref
    case im of
        Just q -> return q
        Nothing -> do
            iq <- create
            ioref $= Just iq
            return iq

