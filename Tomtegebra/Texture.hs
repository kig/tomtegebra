module Texture where
import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr)
import Directory (doesFileExist)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.Rendering.Cairo hiding (rotate, identityMatrix)
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Cairo
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Markup (Markup)

useTexture :: [TextureObject] -> IO ()
useTexture textures = do
    mapM_ (\(tex, i) -> do
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit i
        textureBinding Texture2D $= Just tex) $ zip textures [0..]
    activeTexture $= TextureUnit 0

loadTexture :: FilePath -> IO (Int,Int,TextureObject)
loadTexture filepath = do
    assertFile filepath
    withImageSurfaceFromPixbuf filepath (\s -> do
        (w,h) <- renderWith s $ do
            w <- imageSurfaceGetWidth s
            h <- imageSurfaceGetHeight s
            return (w,h)
        tex <- createTexture Texture2D Enabled (texImage2DSurface Nothing 0 s) 
        return (w,h,tex))

-- | Creates a non-power-of-two texture based from the given Pango text markup.
--   See http://library.gnome.org/devel/pango/unstable/PangoMarkupFormat.html
createTextTexture :: Markup -> IO (Int,Int,TextureObject)
createTextTexture markup = do
    context <- cairoCreateContext Nothing
    layout <- layoutEmpty context
    layoutSetMarkup layout markup
    (Rectangle ix iy iw ih, Rectangle x y w h) <- layoutGetPixelExtents layout
    withImageSurface FormatARGB32 (w+x) (h+y) (\s -> do
            renderWith s (do
                            moveTo (fromIntegral x) (fromIntegral y)
                            showLayout layout)
            tex <- createTexture Texture2D Disabled (texImage2DSurface Nothing 0 s)
            return (w,h,tex))

withImageSurfaceFromPixbuf :: FilePath -> (Surface -> IO a) -> IO a
withImageSurfaceFromPixbuf filepath m = do
    pixbuf <- pixbufNewFromFile filepath
    w <- pixbufGetWidth pixbuf
    h <- pixbufGetHeight pixbuf
    withImageSurface FormatARGB32 w h (\s -> do
            renderWith s (do setSourcePixbuf pixbuf 0 0
                             setOperator OperatorSource
                             paint)
            m s)

assertFile :: FilePath -> IO ()
assertFile filepath = do
    fex <- doesFileExist filepath
    if not fex
        then fail (filepath ++ " does not exist")
        else return ()

createTexture :: TextureTarget -> Capability -> IO () -> IO TextureObject
createTexture target mipmap m = do
    texture target $= Enabled
    [tex] <- genObjectNames 1
    textureBinding target $= Just tex
    textureFilter target $= ((Linear', Nothing), Linear')
    textureWrapMode target S $= (Repeated, Clamp)
    textureWrapMode target T $= (Repeated, Clamp)
    generateMipmap target $= mipmap
    m
    if mipmap == Enabled
        then textureFilter target $= ((Linear', Just Linear'), Linear')
        else return ()
    return tex

texImage2DSurface :: Maybe CubeMapTarget -> Level -> Surface -> IO ()
texImage2DSurface cubemap level imageSurface = do
    pixelData <- imageSurfaceGetData imageSurface
    (w,h) <- renderWith imageSurface $ do
        w <- imageSurfaceGetWidth imageSurface
        h <- imageSurfaceGetHeight imageSurface
        return (fromIntegral w :: GLsizei, fromIntegral h :: GLsizei)
    texImage2DByteString cubemap level RGBA8 w h BGRA UnsignedByte pixelData

texImage2DByteString :: Maybe CubeMapTarget
                     -> Level
                     -> PixelInternalFormat
                     -> GLsizei
                     -> GLsizei
                     -> PixelFormat
                     -> DataType
                     -> ByteString
                     -> IO ()
texImage2DByteString cubemap level iformat w h format ptype bytestring = do
    let (fptr, foffset, flength) = toForeignPtr bytestring
    if (fromIntegral flength) /= w * h * 4
        then fail "imageSurface dimensions don't match data length"
        else return ()
    withForeignPtr fptr $ \ptr -> do
        let optr = plusPtr ptr foffset
        texImage2D cubemap NoProxy
                    level iformat (TextureSize2D w h) 0
                    (PixelData format ptype optr)
