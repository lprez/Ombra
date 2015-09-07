{-# LANGUAGE DataKinds, RebindableSyntax, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, GADTs #-}

module Graphics.Rendering.Ombra.Shader.Default2D where

import Graphics.Rendering.Ombra.Shader

type Uniforms = '[View2, Image, Depth, Transform2]
type Attributes = '[Position2, UV]

-- | An uniform that represents the texture used in the default 2D shader.
newtype Image = Image Sampler2D
        deriving (Typeable, ShaderType, UniformCPU CSampler2D)

-- | An uniform that represents the depth used in the default 2D shader.
newtype Depth = Depth Float
        deriving (Typeable, ShaderType, UniformCPU CFloat)

-- | An uniform that represents the transformation matrix used in the default
-- 2D shader.
newtype Transform2 = Transform2 Mat3
        deriving (Typeable, ShaderType, UniformCPU CMat3)

-- | An uniform that represents the view matrix used in the default 2D shader.
newtype View2 = View2 Mat3
        deriving (Typeable, ShaderType, UniformCPU CMat3)

newtype Position2 = Position2 Vec2
        deriving (Typeable, ShaderType, AttributeCPU CVec2)

newtype UV = UV Vec2
        deriving (Typeable, ShaderType, AttributeCPU CVec2)

vertexShader :: VertexShader '[Transform2, View2, Depth]
                             '[Position2, UV] '[UV]
vertexShader (Transform2 trans :- View2 view :- Depth z :- N)
             (Position2 (Vec2 x y) :- uv@(UV _) :- N) =
                let Vec3 x' y' _ = view * trans * Vec3 x y 1
                in Vertex (Vec4 x' y' z 1) :- uv :- N

fragmentShader :: FragmentShader '[Image] '[UV]
fragmentShader (Image sampler :- N) (UV (Vec2 s t) :- N) =
                Fragment (texture2D sampler (Vec2 s $ 1 - t)) :- N
