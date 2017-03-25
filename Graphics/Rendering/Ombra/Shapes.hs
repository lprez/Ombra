module Graphics.Rendering.Ombra.Shapes where

import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Internal.GL (GLES)
import Graphics.Rendering.Ombra.Vector

rectGeometry :: GLES => Geometry Geometry2D
rectGeometry = mkGeometry2DInd [ (Vec2 (-0.5) (-0.5), Vec2 0 0)
                               , (Vec2 0.5    (-0.5), Vec2 1 0)
                               , (Vec2 0.5    0.5   , Vec2 1 1)
                               , (Vec2 (-0.5) 0.5   , Vec2 0 1)
                               ]
                               [ Triangle 0 1 2
                               , Triangle 0 3 2
                               ]

cubeGeometry :: GLES => Geometry Geometry3D
cubeGeometry = mkGeometry3DInd [ (Vec3 1 1 (-1), Vec2 1 0, Vec3 0 0 (-1))
                               , (Vec3 1 (-1) (-1), Vec2 1 1, Vec3 0 0 (-1))
                               , (Vec3 (-1) 1 (-1), Vec2 0 0, Vec3 0 0 (-1))
                               , (Vec3 (-1) (-1) 1, Vec2 0 1, Vec3 (-1) 0 0)
                               , (Vec3 (-1) 1 1, Vec2 0 0, Vec3 (-1) 0 0)
                               , (Vec3 (-1) (-1) (-1), Vec2 1 1, Vec3 (-1) 0 0)
                               , (Vec3 (-1) (-1) 1, Vec2 0 0, Vec3 0 0 1)
                               , (Vec3 1 (-1) 1, Vec2 1 0, Vec3 0 0 1)
                               , (Vec3 (-1) 1 1, Vec2 0 1, Vec3 0 0 1)
                               , (Vec3 1 (-1) 1, Vec2 0 0, Vec3 1 0 0)
                               , (Vec3 1 (-1) (-1), Vec2 1 0, Vec3 1 0 0)
                               , (Vec3 1 1 1, Vec2 0 1, Vec3 1 0 0)
                               , (Vec3 1 1 (-1), Vec2 1 1, Vec3 0 1 0)
                               , (Vec3 (-1) 1 (-1), Vec2 0 1, Vec3 0 1 0)
                               , (Vec3 1 1 1, Vec2 1 0, Vec3 0 1 0)
                               , (Vec3 1 (-1) (-1), Vec2 1 1, Vec3 0 (-1) 0)
                               , (Vec3 1 (-1) 1, Vec2 0 1, Vec3 0 (-1) 0)
                               , (Vec3 (-1) (-1) (-1), Vec2 1 0, Vec3 0 (-1) 0)
                               , (Vec3 (-1) (-1) (-1), Vec2 0 1, Vec3 0 0 (-1))
                               , (Vec3 (-1) 1 (-1), Vec2 1 0, Vec3 (-1) 0 0)
                               , (Vec3 1 1 1, Vec2 1 1, Vec3 0 0 1)
                               , (Vec3 1 1 (-1), Vec2 1 1, Vec3 1 0 0)
                               , (Vec3 (-1) 1 1, Vec2 0 0, Vec3 0 1 0)
                               , (Vec3 (-1) (-1) 1, Vec2 0 0, Vec3 0 (-1) 0)
                               ]
                               [ Triangle 0 1 2
                               , Triangle 3 4 5
                               , Triangle 6 7 8
                               , Triangle 9 10 11
                               , Triangle 12 13 14
                               , Triangle 15 16 17
                               , Triangle 1 18 2
                               , Triangle 4 19 5
                               , Triangle 7 20 8
                               , Triangle 10 21 11
                               , Triangle 13 22 14
                               , Triangle 16 23 17
                               ]
