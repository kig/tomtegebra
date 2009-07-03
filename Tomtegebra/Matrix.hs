{- |
    Matrix math functions for doing the OpenGL transformation matrices in Haskell.
-}
module Matrix where

import Data.List
import Graphics.Rendering.OpenGL
import Foreign.Ptr

-- | 4x4 Matrix in the OpenGL orientation: translation column is the last 4 elements.
type Matrix4x4 = [Vec4]
-- | 3x3 Matrix in the OpenGL orientation.
type Matrix3x3 = [Vec3]
-- | Four element GLfloat vector.
type Vec4 = [GLfloat]
-- | Three element GLfloat vector.
type Vec3 = [GLfloat]

-- | Multiplies the current OpenGL matrix with the given 'Matrix4x4'.
glMultMatrix :: Matrix4x4 -> IO ()
glMultMatrix m = do
    gm <- glMatrix m
    multMatrix gm

-- | Loads the given 'Matrix4x4' as the current OpenGL matrix.
glLoadMatrix :: Matrix4x4 -> IO ()
glLoadMatrix m = do
    gm <- glMatrix m
    matrix Nothing $= gm

-- | Converts the 'Matrix4x4' into a 'GLmatrix' 'GLfloat'
glMatrix :: Matrix4x4 -> IO (GLmatrix GLfloat)
glMatrix m = newMatrix ColumnMajor $ concat m :: IO (GLmatrix GLfloat)

-- | 'withMatrix' wrapper for 'withMatrix4x4'
withMatrix4x4 :: Matrix4x4 -> (MatrixOrder -> Ptr GLfloat -> IO a) -> IO a
withMatrix4x4 mat4 m = do
    mat <- glMatrix mat4
    withMatrix mat m

-- | The 'Matrix4x4' identity matrix.
identityMatrix :: Matrix4x4
identityMatrix =
    [
        [1,0,0,0],
        [0,1,0,0],
        [0,0,1,0],
        [0,0,0,1]
    ]

-- | Multiplies two matrices together.
matrixMul :: Matrix4x4 -> Matrix4x4 -> Matrix4x4
matrixMul a b =
    map (\row -> map (dotVec row) at) b
    where at = transpose a

-- | Multiplies a vector by a matrix.
matrixMulVec :: Matrix4x4 -> Vec4 -> Vec4
matrixMulVec m v = map (dotVec v) (transpose m)

-- | Returns the upper-left 3x3 matrix of a 4x4 matrix.
matrix4x4To3x3 :: Matrix4x4 -> Matrix3x3
matrix4x4To3x3 m = take 3 $ map vec4To3 m

-- | Pads the 3x3 matrix to a 4x4 matrix with a 1 in bottom right corner and 0 elsewhere.
matrix3x3To4x4 :: Matrix3x3 -> Matrix4x4
matrix3x3To4x4 [x,y,z] = [x ++ [0], y ++ [0], z ++ [0], [0,0,0,1]]
matrix3x3To4x4 m = m

-- | Inverts a 4x4 orthonormal matrix with the special case trick.
invertMatrix4x4ON :: Matrix4x4 -> Matrix4x4
invertMatrix4x4ON m = -- orthonormal matrix inverse
    let [a,b,c] = transpose $ matrix4x4To3x3 m
        [_,_,_,t4] = m in
    let t = vec4To3 t4 in
    [
        vec3To4 a 0, vec3To4 b 0, vec3To4 c 0,
        [dotVec a t, dotVec b t, dotVec c t, t4 !! 3]
    ]

-- | Creates the translation matrix that translates points by the given vector.
translationMatrix :: Vec3 -> Matrix4x4
translationMatrix [x,y,z] = [[1,0,0,0], [0,1,0,0], [0,0,1,0], [x,y,z,1]]
translationMatrix _ = identityMatrix

-- | Creates the scaling matrix that scales points by the factors given by the
--   vector components.
scalingMatrix :: Vec3 -> Matrix4x4
scalingMatrix [x,y,z] = [[x,0,0,0], [0,y,0,0], [0,0,z,0], [0,0,0,1]]
scalingMatrix _ = identityMatrix

-- | Creates a rotation matrix from the given angle and axis.
rotationMatrix :: GLfloat -> Vec3 -> Matrix4x4
rotationMatrix angle axis =
    let [x,y,z] = normalizeVec axis
        c = cos angle
        s = sin angle in
    let c1 = 1-c in
    [
      [x*x*c1+c, y*x*c1+z*s, z*x*c1-y*s, 0],
      [x*y*c1-z*s, y*y*c1+c, y*z*c1+x*s, 0],
      [x*z*c1+y*s, y*z*c1-x*s, z*z*c1+c, 0],
      [0,0,0,1]
    ]

-- | Creates a lookAt matrix from three vectors: the eye position, the point the
--   eye is looking at and the up vector of the eye.
lookAtMatrix :: Vec3 -> Vec3 -> Vec3 -> Matrix4x4
lookAtMatrix eye center up =
    let z = directionVec eye center in
    let x = normalizeVec $ crossVec3 up z in
    let y = normalizeVec $ crossVec3 z x in
    matrixMul (matrix3x3To4x4 $ transpose [x,y,z]) (translationMatrix (negateVec eye))

-- | Creates a frustumMatrix from the given left, right, bottom, top, znear and zfar
--   values for the view frustum.
frustumMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix4x4
frustumMatrix left right bottom top znear zfar =
    let x = 2*znear/(right-left)
        y = 2*znear/(top-bottom)
        a = (right+left)/(right-left)
        b = (top+bottom)/(top-bottom)
        c = -(zfar+znear)/(zfar-znear)
        d = -2*zfar*znear/(zfar-znear) in
    [
      [x, 0, 0, 0],
      [0, y, 0, 0],
      [a, b, c, -1],
      [0, 0, d, 0]
    ]

-- | Creates a perspective projection matrix for the given field-of-view,
--   screen aspect ratio, znear and zfar.
perspectiveMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix4x4
perspectiveMatrix fovy aspect znear zfar =
    let ymax = znear * tan (fovy * pi / 360.0) in
    let ymin = -ymax in
    let xmin = ymin * aspect
        xmax = ymax * aspect in
    frustumMatrix xmin xmax ymin ymax znear zfar

-- | Normalizes a vector to a unit vector.
normalizeVec :: [GLfloat] -> [GLfloat]
normalizeVec v = scaleVec (recip $ lengthVec v) v
-- | Scales a vector by a scalar
scaleVec :: GLfloat -> [GLfloat] -> [GLfloat]
scaleVec s v = map ((*) s) v
-- | Computes the length of a vector.
lengthVec :: [GLfloat] -> GLfloat
lengthVec v = sqrt.sum $ map square v

-- | Inner product of two vectors.
innerVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
innerVec = zipWith (*)
-- | Adds two vectors together.
addVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
addVec = zipWith (+)
-- | Subtracts a vector from another.
subVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
subVec = zipWith (-)
-- | Negates a vector.
negateVec :: [GLfloat] -> [GLfloat]
negateVec = map negate
-- | Computes the direction unit vector between two vectors.
directionVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
directionVec u v = normalizeVec (subVec u v)
-- | Vector dot product.
dotVec :: [GLfloat] -> [GLfloat] -> GLfloat
dotVec a b = sum $ innerVec a b
-- | Cross product of two 3-vectors.
crossVec3 :: [GLfloat] -> [GLfloat] -> [GLfloat]
crossVec3 [u0,u1,u2] [v0,v1,v2] = [u1*v2-u2*v1, u2*v0-u0*v2, u0*v1-u1*v0]
crossVec3 _ _ = [0,0,1]

-- | Converts a 4-vector into a 3-vector by dropping the fourth element.
vec4To3 :: Vec4 -> Vec3
vec4To3 = take 3

-- | Converts a 3-vector into a 4-vector by appending the given value to it.
vec3To4 :: Vec3 -> GLfloat -> Vec4
vec3To4 v i = v ++ [i]

-- | Multiplies a GLfloat by itself.
square :: GLfloat -> GLfloat
square x = x * x
