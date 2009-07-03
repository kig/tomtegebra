{- |
    Helper functions for loading, creating and binding textures.
-}
module Texture (
    useTexture,
    loadTexture,
    createTextTexture,
    createPangoLayoutTexture,
    withImageSurfaceFromFile,
    withImageSurface,
    createTexture,
    texImage2DSurface,
    texImage2DByteString
) where
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

-- | Binds the given list of textures to texture units so that the nth element
--   of the list is bound to the nth texture unit.
useTexture :: [TextureObject] -> IO ()
useTexture textures = do
    mapM_ (\(tex, i) -> do
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit i
        textureBinding Texture2D $= Just tex) $ zip textures [0..]
    activeTexture $= TextureUnit 0

-- | Loads a texture from an image file.
--   Uses Gdk.Pixbuf for loading the image, so a wide range of image formats is
--   supported, see 'withImageSurfaceFromFile'.
--   Returns a (width, height, TextureObject)-tuple.
loadTexture :: FilePath -> IO (Int,Int,TextureObject)
loadTexture filepath = do
    assertFile filepath
    withImageSurfaceFromFile filepath (\s -> do
        (w,h) <- renderWith s $ do
            w <- imageSurfaceGetWidth s
            h <- imageSurfaceGetHeight s
            return (w,h)
        tex <- createTexture Texture2D Enabled (texImage2DSurface Nothing 0 s) 
        return (w,h,tex))

-- | Creates a non-power-of-two texture from the given Pango text markup.
--   Returns a (width, height, TextureObject)-tuple.
--
--   See <http://library.gnome.org/devel/pango/stable/PangoMarkupFormat.html>
createTextTexture :: LayoutAlignment -> Markup -> IO (Int,Int,TextureObject)
createTextTexture alignment markup = do
    context <- cairoCreateContext Nothing
    layout <- layoutEmpty context
    layoutSetMarkup layout markup
    layoutSetAlignment layout alignment
    createPangoLayoutTexture layout

-- | Creates a non-power-of-two texture from the given Pango layout.
--   Returns a (width, height, TextureObject)-tuple.
createPangoLayoutTexture :: PangoLayout -> IO (Int, Int, TextureObject)
createPangoLayoutTexture layout = do
    (_, Rectangle x y w h) <- layoutGetPixelExtents layout
    withImageSurface FormatARGB32 (w+x) (h+y) (\s -> do
            renderWith s (do
                            moveTo (fromIntegral x) (fromIntegral y)
                            showLayout layout)
            tex <- createTexture Texture2D Disabled (texImage2DSurface Nothing 0 s)
            return (w,h,tex))

-- | Loads an image surface from an image file and passes it to the given function.
--   Uses Gdk.Pixbuf for loading the image, so a wide range of image formats is
--   supported, e.g. JPEG, PNG, GIF, TGA, TIFF, JPEG 2000, PNM, SVG, etc.
--
--   See Graphics.UI.Gtk.Gdk.Pixbuf.pixbufGetFormats.
withImageSurfaceFromFile :: FilePath -> (Surface -> IO a) -> IO a
withImageSurfaceFromFile filepath m = do
    pixbuf <- pixbufNewFromFile filepath
    w <- pixbufGetWidth pixbuf
    h <- pixbufGetHeight pixbuf
    withImageSurface FormatARGB32 w h (\s -> do
            renderWith s (do setSourcePixbuf pixbuf 0 0
                             setOperator OperatorSource
                             paint)
            m s)

-- | Raise unless filepath exists.
assertFile :: FilePath -> IO ()
assertFile filepath = do
    fex <- doesFileExist filepath
    if not fex
        then fail (filepath ++ " does not exist")
        else return ()

-- | Creates a new texture object for the given TextureTarget and runs the given
--   IO () -function to initialize it.
--   
--   If mipmap is Enabled, sets texture min & mag filters to mipmapped linear,
--   otherwise non-mipmapped linear.
--   
--   The texture wrap mode is repeated clamp on both S and T.
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

-- | Loads a Cairo surface as a texture image.
texImage2DSurface :: Maybe CubeMapTarget -> Level -> Surface -> IO ()
texImage2DSurface cubemap level imageSurface = do
    pixelData <- imageSurfaceGetData imageSurface
    (w,h) <- renderWith imageSurface $ do
        w <- imageSurfaceGetWidth imageSurface
        h <- imageSurfaceGetHeight imageSurface
        return (fromIntegral w :: GLsizei, fromIntegral h :: GLsizei)
    texImage2DByteString cubemap level RGBA8 (TextureSize2D w h) BGRA UnsignedByte pixelData

-- | Loads a ByteString as a texture image.
texImage2DByteString :: Maybe CubeMapTarget
                     -> Level
                     -> PixelInternalFormat
                     -> TextureSize2D
                     -> PixelFormat
                     -> DataType
                     -> ByteString
                     -> IO ()
texImage2DByteString cubemap level iformat (TextureSize2D w h) format ptype bytestring = do
    let (fptr, foffset, flength) = toForeignPtr bytestring
    if (fromIntegral flength) /= w * h * 4
        then fail "imageSurface dimensions don't match data length"
        else return ()
    withForeignPtr fptr $ \ptr -> do
        let optr = plusPtr ptr foffset
        texImage2D cubemap NoProxy
                    level iformat (TextureSize2D w h) 0
                    (PixelData format ptype optr)
