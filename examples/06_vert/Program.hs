{-# LANGUAGE DataKinds, RebindableSyntax, DeriveGeneric, GADTs #-}

module Program (
        module Graphics.Rendering.Ombra.Shader.Default3D,
        Uniforms,
        Time(..),
        vertexShader,
        fragmentShader
) where

import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Default3D
        hiding (Uniforms, Texture2, vertexShader, fragmentShader)

data Time = Time Float deriving Generic
type Uniforms = '[View3, Time, Transform3]

-- The vertex shader operates on the vertices.
vertexShader ::
        VertexShader Uniforms                    -- List of uniforms
                     '[ Position3, UV, Normal3 ] -- List of attributes (i.e.
                                                 -- values associated with the
                                                 -- vertices of the geometry,
                                                 -- like the vertex position).
                                                 -- We don't need UV and
                                                 -- Normal3 but we keep them
                                                 -- for compatibility with
                                                 -- Geometry3D).
                     '[ Position3 ]              -- List of outputs (they
                                                 -- become the inputs of the
                                                 -- fragment shader of course).
vertexShader (View3 viewMatrix :- Time time :- Transform3 modelMatrix :- N)
             (pos@(Position3 (Vec3 x y z)) :- _) =
             let spread = store $ 0.6 * sin time 
                 x' = x + spread * sin (y * 5)
                 z' = z + spread * cos (y * 5)
                 v = store $ viewMatrix * modelMatrix * Vec4 x' y z' 1.0
             in Vertex v :- pos :- N

fragmentShader :: FragmentShader '[] '[ Position3 ]
fragmentShader N (Position3 (Vec3 x y z) :- N) =
        Fragment (abs $ Vec4 (x + z) (x * z) (x - y) 1.0) :- N
