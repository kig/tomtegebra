module Shaders where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


-- Make sure that GLSL is supported by the driver, either directly by the core
-- or via an extension.
checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

readAndCompileShader :: Shader s => FilePath -> IO s
readAndCompileShader filePath = do
   src <- readFile filePath
   [shader] <- genObjectNames 1
   shaderSource shader $= [src]
   compileShader shader
   reportErrors
   ok <- get (compileStatus shader)
   infoLog <- get (shaderInfoLog shader)
   mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""]
   unless ok $ do
      deleteObjectNames [shader]
      ioError (userError "shader compilation failed")
   return shader

createProgram :: [VertexShader] -> [FragmentShader] -> IO Program
createProgram vs fs = do
   [program] <- genObjectNames 1
   attachedShaders program $= (vs, fs)
   linkProgram program
   reportErrors
   ok <- get (linkStatus program)
   infoLog <- get (programInfoLog program)
   mapM_ putStrLn ["Program info log:", infoLog, ""]
   unless ok $ do
      deleteObjectNames [program]
      ioError (userError "linking failed")
   return program

loadProgram :: FilePath -> FilePath -> IO Program
loadProgram vertexShader fragmentShader = loadProgramMulti [vertexShader] [fragmentShader]

loadProgramMulti :: [FilePath] -> [FilePath] -> IO Program
loadProgramMulti vertexShaders fragmentShaders = do
    vs <- mapM readAndCompileShader vertexShaders
    fs <- mapM readAndCompileShader fragmentShaders
    createProgram vs fs

setUniform :: Uniform a => Program -> String -> IO (a ->  IO ())
setUniform program name = do
    location <- get (uniformLocation program name)
    reportErrors
    return (\val -> uniform location $= val)

setAttribute :: Program -> String -> IO ((IntegerHandling, VertexArrayDescriptor a) -> IO ())
setAttribute program name = do
    location <- get (attribLocation program name)
    reportErrors
    return (\val -> vertexAttribPointer location $= val)

--    setUniform "BrickColor" (Color3 1.0 0.3 (0.2 :: GLfloat))
--    setUniform "MortarColor" (Color3 0.85 0.86 (0.84 :: GLfloat))
--    setUniform "BrickSize" (Vertex2 0.30 (0.15 :: GLfloat))
--    setUniform "BrickPct" (Vertex2 0.90 (0.85 :: GLfloat))
--    setUniform "LightPosition" (Vertex3 0 0 (4 :: GLfloat))
-- 
