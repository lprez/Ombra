{-# LANGUAGE DataKinds, RebindableSyntax, DeriveGeneric, GADTs #-}

module Graphics.Rendering.Ombra.Shader.Default3D where

import Graphics.Rendering.Ombra.Shader

type Uniforms = '[View3, Transform3, Texture2]
type Attributes = '[Position3, UV, Normal3]

data Texture2 = Texture2 Sampler2D deriving Generic

data Transform3 = Transform3 Mat4 deriving Generic

data View3 = View3 Mat4 deriving Generic

data Position3 = Position3 Vec3 deriving Generic

data Normal3 = Normal3 Vec3 deriving Generic

data UV = UV Vec2 deriving Generic

vertexShader :: VertexShader '[ Transform3, View3 ]
                             '[ Position3, UV, Normal3 ]
                             '[ Position3, UV, Normal3 ]
vertexShader (Transform3 modelMatrix :- View3 viewMatrix :- N)
             (Position3 pos :- uv :- Normal3 norm :- N) =
             let worldPos = modelMatrix * vec4 (pos # 1.0)
                 viewPos = viewMatrix * worldPos
                 worldNorm = vec3 $ modelMatrix * vec4 (norm # 0.0)
             in Vertex viewPos :- Position3 (vec3 worldPos) :-
                uv :- Normal3 worldNorm :- N

fragmentShader :: FragmentShader '[ Texture2 ] [ Position3, UV, Normal3 ]
fragmentShader (Texture2 sampler :- N) (_ :- UV (Vec2 s t) :- _ :- N) =
                Fragment (texture2D sampler $ Vec2 s (1 - t)) :- N
