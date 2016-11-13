{-# LANGUAGE DataKinds #-}

module Main where

import Data.Vect.Float
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.D3

import Common
import Program

scene :: Float -> Layer
scene time = let -- We could have used the 'mesh' function, but that requires
                 -- a Texture, and we're not using that in our shaders. Note
                 -- that the transformation functions (e.g. trans, rotZ) modify
                 -- the Transform3 Global.
                 obj :: Object '[ Time, Transform3 ] Geometry3D
                 obj =     trans (Vec3 0 0 (- 1.1))
                       .   rotY (time / 2)
                       .   rotX 0.1
                       .   scaleV (Vec3 0.04 0.16 0.04)
                       $   Time -= time
                       :~> Transform3 -= idmtx
                       :~> geom para

                 grp = viewPersp 0.1 100000 100 idmtx [obj]
             in layer prg grp
        where para = splitCube 20

prg :: Program Uniforms Attributes
prg = program vertexShader fragmentShader

{-
Generate a cube where the lateral quads are split vertically into n rectangles.
        +-------+
       /       /|
      /       / +
     +-------+ /|    n = 2
     |       |/ +
     +-------+ /
     |       |/
     +-------+
This lets us deform the object horizontally.
-}
splitCube :: GLES => Int -> Geometry Geometry3D
splitCube n = mkGeometry3D (concat vertices)
                           (replicate (n * 4) (Vec2 0 0)) -- We don't need UVs
                           (replicate (n * 4) (Vec3 0 0 0)) -- Neither normals
                           (concat floors ++ concat walls)

        where vertices = [ [ Vec3 (-1) y (-1)
                           , Vec3 1    y (-1)
                           , Vec3 (-1) y 1
                           , Vec3 1    y 1
                           ]
                         | y <- [-1, -1 + 2 / (fromIntegral n - 1) .. 1] ]
                        
              walls = [ quadToTris [y + l, y + r, y + 4 + l, y + 4 + r]
                      | (l, r) <- [(0, 1), (1, 3), (3, 2), (2, 0)]
                      , y <- [0, 4 .. (fromIntegral n - 2) * 4] ]

              floors = [ quadToTris [y, y + 1, y + 2, y + 3]
                       | y <- [0, fromIntegral (n - 1) * 4] ]

              quadToTris [a, b, c, d] = [a, b, c, b, c, d]

main :: IO ()
main = animation scene
