module Matrix where

import Data.List
import Graphics.Rendering.OpenGL
import Foreign.Ptr

type Matrix4x4 = [Vec4]
type Matrix3x3 = [Vec3]
type Vec4 = [GLfloat]
type Vec3 = [GLfloat]

-- Matrices are in the OpenGL orientation:
-- "translation column is the last 4 elements"

glMultMatrix :: Matrix4x4 -> IO ()
glMultMatrix m = do
    gm <- glMatrix m
    multMatrix gm

glLoadMatrix :: Matrix4x4 -> IO ()
glLoadMatrix m = do
    gm <- glMatrix m
    matrix Nothing $= gm

glMatrix :: Matrix4x4 -> IO (GLmatrix GLfloat)
glMatrix m = newMatrix ColumnMajor $ flatten m :: IO (GLmatrix GLfloat)

withMatrix4x4 :: Matrix4x4 -> (MatrixOrder -> Ptr GLfloat -> IO a) -> IO a
withMatrix4x4 matrix m = do
    mat <- glMatrix matrix
    withMatrix mat m

flatten :: [[a]] -> [a]
flatten = foldl1 (++)

identityMatrix :: Matrix4x4
identityMatrix =
    [
        [1,0,0,0],
        [0,1,0,0],
        [0,0,1,0],
        [0,0,0,1]
    ]

matrixMul :: Matrix4x4 -> Matrix4x4 -> Matrix4x4
matrixMul a b =
    map (\row -> map (dotVec row) at) b
    where at = transpose a

matrixMulVec :: Matrix4x4 -> Vec4 -> Vec4
matrixMulVec m v = map (dotVec v) (transpose m)

matrix4x4To3x3 :: Matrix4x4 -> Matrix3x3
matrix4x4To3x3 m = take 3 $ map vec4To3 m

matrix3x3To4x4 :: Matrix3x3 -> Matrix4x4
matrix3x3To4x4 [x,y,z] = [x ++ [0], y ++ [0], z ++ [0], [0,0,0,1]]

invertMatrix4x4ON :: Matrix4x4 -> Matrix4x4
invertMatrix4x4ON m = -- orthonormal matrix inverse
    let [a,b,c] = transpose $ matrix4x4To3x3 m
        [_,_,_,t4] = m in
    let t = vec4To3 t4 in
    [
        vec3To4 a 0, vec3To4 b 0, vec3To4 c 0,
        [dotVec a t, dotVec b t, dotVec c t, t4 !! 3]
    ]

translationMatrix :: Vec3 -> Matrix4x4
translationMatrix [x,y,z] = [[1,0,0,0], [0,1,0,0], [0,0,1,0], [x,y,z,1]]

scalingMatrix :: Vec3 -> Matrix4x4
scalingMatrix [x,y,z] = [[x,0,0,0], [0,y,0,0], [0,0,z,0], [0,0,0,1]]

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

lookAtMatrix :: Vec3 -> Vec3 -> Vec3 -> Matrix4x4
lookAtMatrix eye center up =
    let z = directionVec eye center in
    let x = normalizeVec $ crossVec3 up z in
    let y = normalizeVec $ crossVec3 z x in
    matrixMul (matrix3x3To4x4 $ transpose [x,y,z]) (translationMatrix (negateVec eye))

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

perspectiveMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix4x4
perspectiveMatrix fovy aspect znear zfar =
    let ymax = znear * tan (fovy * pi / 360.0) in
    let ymin = -ymax in
    let xmin = ymin * aspect
        xmax = ymax * aspect in
    frustumMatrix xmin xmax ymin ymax znear zfar

normalizeVec v = scaleVec (recip $ lengthVec v) v
scaleVec s v = map ((*) s) v
lengthVec v = sqrt.sum $ map square v

innerVec = zipWith (*)
addVec = zipWith (+)
subVec = zipWith (-)
negateVec = map negate
directionVec u v = normalizeVec (subVec u v)
dotVec a b = sum $ innerVec a b
crossVec3 [u0,u1,u2] [v0,v1,v2] = [u1*v2-u2*v1, u2*v0-u0*v2, u0*v1-u1*v0]

vec4To3 :: Vec4 -> Vec3
vec4To3 = take 3

vec3To4 :: Vec3 -> GLfloat -> Vec4
vec3To4 v i = v ++ [i]

square :: GLfloat -> GLfloat
square x = x * x
