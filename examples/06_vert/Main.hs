{-# LANGUAGE DataKinds #-}

module Main where

import Control.Applicative
import Control.Monad (forM_)
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.D3
import Graphics.Rendering.Ombra.Vector

import Utils.Play
import Program

scene :: Float -> Layer
scene time = let -- We could have used the 'mesh' function, but that requires
                 -- a Texture, and we're not using that in our shaders. Note
                 -- that the transformation functions (e.g. trans, rotZ) modify
                 -- the Transform3 Global.
                 obj :: Object '[ Time, Transform3 ] Geometry3D
                 obj =     trans (Vec3 0 0 (- 0.3))
                       .   rotY (time / 2)
                       .   rotX 0.1
                       .   scaleV (Vec3 0.04 0.16 0.04)
                       $   Time -= time
                       :~> Transform3 -= idmtx
                       :~> geom para

                 grp = viewPersp 0.1 100000 100 idmtx [obj]
             in layer prg grp
        where para = splitCube 20

prg :: Program Uniforms Geometry3D
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
splitCube n = buildGeometry $
        do vertices <- mapM verticesAt floors

           quad $ head vertices
           quad $ last vertices

           forM_ (zip vertices (tail vertices)) $
                   \( (bottomNW, bottomNE, bottomSW, bottomSE)
                    , (topNW, topNE, topSW, topSE)
                    ) ->
                            do quad (bottomNW, bottomNE, topNW, topNE)
                               quad (bottomNE, bottomSE, topNE, topSE)
                               quad (bottomSE, bottomSW, topSE, topSW)
                               quad (bottomSW, bottomNW, topSW, topNW)

        where -- We don't need normals and UV coordinates in this example so we
              -- just use the 'zeroV' vector. Alternatively we could have used
              -- our own Geometry type without those attributes. It would be
              -- more efficient but also more complex.
              verticesAt y = (,,,) <$> vertex3D (Vec3 (-1) y (-1)) zeroV zeroV
                                   <*> vertex3D (Vec3 1    y (-1)) zeroV zeroV
                                   <*> vertex3D (Vec3 (-1) y 1   ) zeroV zeroV
                                   <*> vertex3D (Vec3 1    y 1   ) zeroV zeroV
              floors = [-1, -1 + 2 / (fromIntegral n - 1) .. 1]
              floorCouples = zip floors (tail floors)
              quad (a, b, c, d) = triangle a b c >> triangle b c d

main :: IO ()
main = animation scene
