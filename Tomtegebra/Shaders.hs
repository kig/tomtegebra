{- |
    Shader loading helpers, adapted from the OrangeBook ogl2brick example in the
    Haskell GLUT binding.
-}
module Shaders where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT (reportErrors)


-- | Make sure that GLSL is supported by the driver, either directly by the core
--   or via an extension.
checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

-- | Loads and compiles a GLSL shader from a 'FilePath'.
--
--   Raises an ioError if shader compilation fails.
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

-- | Creates a GLSL program from the given lists of vertex shaders and fragment shaders.
--   Attaches the shaders to the program and links the program.
--   
--   Raises an ioError if the linking fails.
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

-- | Loads and links a program from a vertex shader file and fragment shader file.
loadProgram :: FilePath -> FilePath -> IO Program
loadProgram vertexShader fragmentShader = loadProgramMulti [vertexShader] [fragmentShader]

-- | Loads and links a program from lists of vertex shader files and fragment shader files.
loadProgramMulti :: [FilePath] -> [FilePath] -> IO Program
loadProgramMulti vertexShaders fragmentShaders = do
    vs <- mapM readAndCompileShader vertexShaders
    fs <- mapM readAndCompileShader fragmentShaders
    createProgram vs fs

-- | Sets program uniform to the given value.
--
--   Returns a setter function, in hopes that it would avoid subsequent calls
--   to uniformLocation.
setUniform :: Uniform a => Program -> String -> IO (a ->  IO ())
setUniform program name = do
    location <- get (uniformLocation program name)
    reportErrors
    return (\val -> uniform location $= val)

-- | Sets program attribute to the given value.
--
--   Returns a setter function, in hopes that it would avoid subsequent calls
--   to attribLocation.
setAttribute :: Program -> String -> IO ((IntegerHandling, VertexArrayDescriptor a) -> IO ())
setAttribute program name = do
    location <- get (attribLocation program name)
    reportErrors
    return (\val -> vertexAttribPointer location $= val)

