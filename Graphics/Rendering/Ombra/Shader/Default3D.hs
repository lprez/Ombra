{-# LANGUAGE DataKinds, DeriveGeneric, GADTs #-}

module Graphics.Rendering.Ombra.Shader.Default3D (
        Uniforms,
        Attributes,
        Texture2(..),
        Transform3(..),
        View3(..),
        Position3(..),
        UV(..),
        Normal3(..),
        vertexShader,
        fragmentShader
) where

import Graphics.Rendering.Ombra.Shader

type Uniforms = '[View3, Transform3, Texture2]
type Attributes = '[Position3, UV, Normal3]

data Texture2 = Texture2 GSampler2D deriving Generic

data Transform3 = Transform3 GMat4 deriving Generic

data View3 = View3 GMat4 deriving Generic

data Position3 = Position3 GVec3 deriving Generic

data Normal3 = Normal3 GVec3 deriving Generic

vertexShader :: VertexShader '[ Transform3, View3 ]
                             '[ Position3, UV, Normal3 ]
                             '[ Position3, UV, Normal3 ]
vertexShader (Transform3 modelGMatrix :- View3 viewGMatrix :- N)
             (Position3 pos :- uv :- Normal3 norm :- N) =
             let worldPos = store $ modelGMatrix .* (pos ^| 1.0)
                 viewPos = viewGMatrix .* worldPos
                 worldNorm = extract $ modelGMatrix .* (norm ^| 0)
             in Vertex viewPos :- Position3 (extract worldPos) :-
                uv :- Normal3 worldNorm :- N

fragmentShader :: FragmentShader '[ Texture2 ] [ Position3, UV, Normal3 ]
fragmentShader (Texture2 sampler :- N) (_ :- UV (GVec2 s t) :- _ :- N) =
        Fragment (texture2D sampler $ GVec2 s (1 - t)) :- N
