{-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, GADTs #-}

module Graphics.Rendering.Ombra.Shader.Default3D where

import Graphics.Rendering.Ombra.Shader

type Uniforms = '[View3, Transform3, Texture2]
type Attributes = '[Position3, UV, Normal3]

newtype Texture2 = Texture2 Sampler2D
        deriving (Typeable, ShaderType, UniformCPU CSampler2D)

newtype Transform3 = Transform3 Mat4
        deriving (Typeable, ShaderType, UniformCPU CMat4)

newtype View3 = View3 Mat4
        deriving (Typeable, ShaderType, UniformCPU CMat4)

newtype Position3 = Position3 Vec3
        deriving (Typeable, ShaderType, AttributeCPU CVec3)

newtype Normal3 = Normal3 Vec3
        deriving (Typeable, ShaderType, AttributeCPU CVec3)

newtype UV = UV Vec2
        deriving (Typeable, ShaderType, AttributeCPU CVec2)

vertexShader :: VertexShader '[ Transform3, View3 ]
                             '[ Position3, UV, Normal3 ]
                             '[ UV, Normal3 ]
vertexShader (Transform3 modelMatrix :- View3 viewMatrix :- N)
             (Position3 (Vec3 x y z) :- uv@(UV _) :- norm@(Normal3 _) :- N) =
             let v = viewMatrix * modelMatrix * Vec4 x y z 1.0
             in Vertex v :- uv :- norm :- N

fragmentShader :: FragmentShader '[ Texture2 ] [ UV, Normal3 ]
fragmentShader (Texture2 sampler :- N) (UV (Vec2 s t) :- Normal3 _ :- N) =
                Fragment (texture2D sampler $ Vec2 s (1 - t)) :- N
