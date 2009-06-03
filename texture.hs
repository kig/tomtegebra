module Texture where
import Data.ByteString.Internal (toForeignPtr)
import Directory (doesFileExist)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.Rendering.Cairo hiding (rotate, identityMatrix)

useTexture _ Nothing = do
    clientState TextureCoordArray $= Disabled
    texture Texture2D $= Disabled

useTexture texCoords (Just tex) = do
    clientState TextureCoordArray $= Enabled
    texture Texture2D $= Enabled
    textureBinding Texture2D $= Just tex
    arrayPointer TextureCoordArray $= texCoords

loadTextureFromPNG filepath = do
    fex <- doesFileExist filepath
    if not fex
        then fail (filepath ++ " does not exist")
        else return ()
    [tex] <- genObjectNames 1
    textureBinding Texture2D $= Just tex
    texture Texture2D $= Enabled
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, Clamp)
    textureWrapMode Texture2D T $= (Repeated, Clamp)
    generateMipmap Texture2D $= Enabled
    withImageSurfaceFromPNG filepath (texImage2DSurface 0)
    textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
    textureBinding Texture2D $= Nothing
    return tex

texImage2DSurface level imageSurface = do
    pixelData <- imageSurfaceGetData imageSurface
    (w,h) <- renderWith imageSurface $ do
        w <- imageSurfaceGetWidth imageSurface
        h <- imageSurfaceGetHeight imageSurface
        return (fromIntegral w :: GLsizei, fromIntegral h :: GLsizei)
    texImage2DPixelData level RGBA8 w h BGRA UnsignedByte pixelData

texImage2DPixelData level iformat w h format ptype pixelData = do
    let (fptr, foffset, flength) = toForeignPtr pixelData
    if (fromIntegral flength) /= w * h * 4
        then fail "imageSurface dimensions don't match data length"
        else return ()
    withForeignPtr fptr $ \ptr -> do
        let optr = plusPtr ptr foffset
        texImage2D Nothing NoProxy
                    level iformat (TextureSize2D w h) 0
                    (PixelData format ptype optr)

