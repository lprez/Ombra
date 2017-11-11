module Utils.Shapes where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Vector
import Utils.Vertex3D

cube :: GLES => Geometry Triangle GVertex3D
cube = buildGeometry $
        do let vertex3D pos norm uv = vertex $ Vertex3D pos norm uv

           v0 <- vertex3D  (Vec3 1 1 (-1))       (Vec3 0 0 (-1)) (Vec2 1 0)
           v1 <- vertex3D  (Vec3 1 (-1) (-1))    (Vec3 0 0 (-1)) (Vec2 1 1) 
           v2 <- vertex3D  (Vec3 (-1) 1 (-1))    (Vec3 0 0 (-1)) (Vec2 0 0) 
           v3 <- vertex3D  (Vec3 (-1) (-1) 1)    (Vec3 (-1) 0 0) (Vec2 0 1) 
           v4 <- vertex3D  (Vec3 (-1) 1 1)       (Vec3 (-1) 0 0) (Vec2 0 0) 
           v5 <- vertex3D  (Vec3 (-1) (-1) (-1)) (Vec3 (-1) 0 0) (Vec2 1 1) 
           v6 <- vertex3D  (Vec3 (-1) (-1) 1)    (Vec3 0 0 1)    (Vec2 0 0) 
           v7 <- vertex3D  (Vec3 1 (-1) 1)       (Vec3 0 0 1)    (Vec2 1 0) 
           v8 <- vertex3D  (Vec3 (-1) 1 1)       (Vec3 0 0 1)    (Vec2 0 1) 
           v9 <- vertex3D  (Vec3 1 (-1) 1)       (Vec3 1 0 0)    (Vec2 0 0) 
           v10 <- vertex3D (Vec3 1 (-1) (-1))    (Vec3 1 0 0)    (Vec2 1 0) 
           v11 <- vertex3D (Vec3 1 1 1)          (Vec3 1 0 0)    (Vec2 0 1) 
           v12 <- vertex3D (Vec3 1 1 (-1))       (Vec3 0 1 0)    (Vec2 1 1) 
           v13 <- vertex3D (Vec3 (-1) 1 (-1))    (Vec3 0 1 0)    (Vec2 0 1) 
           v14 <- vertex3D (Vec3 1 1 1)          (Vec3 0 1 0)    (Vec2 1 0) 
           v15 <- vertex3D (Vec3 1 (-1) (-1))    (Vec3 0 (-1) 0) (Vec2 1 1) 
           v16 <- vertex3D (Vec3 1 (-1) 1)       (Vec3 0 (-1) 0) (Vec2 0 1) 
           v17 <- vertex3D (Vec3 (-1) (-1) (-1)) (Vec3 0 (-1) 0) (Vec2 1 0) 
           v18 <- vertex3D (Vec3 (-1) (-1) (-1)) (Vec3 0 0 (-1)) (Vec2 0 1) 
           v19 <- vertex3D (Vec3 (-1) 1 (-1))    (Vec3 (-1) 0 0) (Vec2 1 0) 
           v20 <- vertex3D (Vec3 1 1 1)          (Vec3 0 0 1)    (Vec2 1 1) 
           v21 <- vertex3D (Vec3 1 1 (-1))       (Vec3 1 0 0)    (Vec2 1 1) 
           v22 <- vertex3D (Vec3 (-1) 1 1)       (Vec3 0 1 0)    (Vec2 0 0) 
           v23 <- vertex3D (Vec3 (-1) (-1) 1)    (Vec3 0 (-1) 0) (Vec2 0 0) 

           triangle v0 v1 v2
           triangle v3 v4 v5
           triangle v6 v7 v8
           triangle v9 v10 v11
           triangle v12 v13 v14
           triangle v15 v16 v17
           triangle v1 v18 v2
           triangle v4 v19 v5
           triangle v7 v20 v8
           triangle v10 v21 v11
           triangle v13 v22 v14
           triangle v16 v23 v17
