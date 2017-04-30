{-# LANGUAGE DataKinds, DeriveGeneric, GADTs #-}

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

data Time = Time GFloat deriving Generic
type Uniforms = '[Project3, View3, Time, Transform3]

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
vertexShader (Project3 projMatrix :- _ :- Time t :- Transform3 modelMatrix :- N)
             (pos@(Position3 (GVec3 x y z)) :- _) =
             let spread = store $ 0.6 * sin t 
                 x' = x + spread * sin (y * 5)
                 z' = z + spread * cos (y * 5)
                 v = store $ projMatrix .*. modelMatrix .* GVec4 x' y z' 1.0
             in Vertex v :- pos :- N

fragmentShader :: FragmentShader '[] '[ Position3 ]
fragmentShader N (Position3 (GVec3 x y z) :- N) =
        Fragment (GVec4 (abs $ x + z) (abs $ x * z) (abs $ x - y) 1.0) :- N
