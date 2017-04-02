{-# LANGUAGE DataKinds, DeriveGeneric, GADTs #-}

module Graphics.Rendering.Ombra.Shader.Default2D where

import Graphics.Rendering.Ombra.Shader

type Uniforms = '[View2, Image, Depth, Transform2]
type Attributes = '[Position2, UV]

-- | An uniform that represents the texture used in the default 2D shader.
data Image = Image GSampler2D deriving Generic

-- | An uniform that represents the depth used in the default 2D shader.
data Depth = Depth GFloat deriving Generic

-- | An uniform that represents the transformation matrix used in the default
-- 2D shader.
data Transform2 = Transform2 GMat3 deriving Generic
-- | An uniform that represents the view matrix used in the default 2D shader.
data View2 = View2 GMat3 deriving Generic

data Position2 = Position2 GVec2 deriving Generic

data UV = UV GVec2 deriving Generic

vertexShader :: VertexShader '[Transform2, View2, Depth]
                             '[Position2, UV] '[UV]
vertexShader (Transform2 trans :- View2 view :- Depth z :- N)
             (Position2 pos :- uv@(UV _) :- N) =
                let GVec3 x' y' _ = view .*. trans .* (pos ^| 1)
                in Vertex (GVec4 x' y' z 1) :- uv :- N

fragmentShader :: FragmentShader '[Image] '[UV]
fragmentShader (Image sampler :- N) (UV (GVec2 s t) :- N) =
                Fragment (texture2D sampler (GVec2 s $ 1 - t)) :- N
