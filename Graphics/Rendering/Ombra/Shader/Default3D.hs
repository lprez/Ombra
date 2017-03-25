{-# LANGUAGE DataKinds, DeriveGeneric, GADTs #-}

module Graphics.Rendering.Ombra.Shader.Default3D where

import Graphics.Rendering.Ombra.Shader

type Uniforms = '[View3, Transform3, Texture2]
type Attributes = '[Position3, UV, Normal3]

data Texture2 = Texture2 GSampler2D deriving Generic

data Transform3 = Transform3 GMat4 deriving Generic

data View3 = View3 GMat4 deriving Generic

data Position3 = Position3 GVec3 deriving Generic

data Normal3 = Normal3 GVec3 deriving Generic

data UV = UV GVec2 deriving Generic

vertexShader :: VertexShader '[ Transform3, View3 ]
                             '[ Position3, UV, Normal3 ]
                             '[ Position3, UV, Normal3 ]
vertexShader (Transform3 modelGMatrix :- View3 viewGMatrix :- N)
             (Position3 (GVec3 x y z) :- uv :- Normal3 (GVec3 nx ny nz) :- N) =
             let worldPos@(GVec4 wx wy wz _) =
                     modelGMatrix .* GVec4 x y z 1.0
                 viewPos = viewGMatrix .* worldPos
                 (GVec4 wnx wny wnz _) = modelGMatrix .* GVec4 nx ny nz 0
             in Vertex viewPos :- Position3 (GVec3 wx wy wz) :-
                uv :- Normal3 (GVec3 wnx wny wnz) :- N

fragmentShader :: FragmentShader '[ Texture2 ] [ Position3, UV, Normal3 ]
fragmentShader (Texture2 sampler :- N) (_ :- UV (GVec2 s t) :- _ :- N) =
                Fragment (texture2D sampler $ GVec2 s (1 - t)) :- N
