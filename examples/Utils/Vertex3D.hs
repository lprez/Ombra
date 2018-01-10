{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Utils.Vertex3D (
        Vertex3D(..),
        GVertex3D(..),
        GHVertex3D(..),
) where

import GHC.Generics
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Vector

data Vertex3D = Vertex3D Vec3 Vec3 Vec2 deriving Generic
data GVertex3D = GVertex3D GVec3 GVec3 GVec2 deriving Generic
data GHVertex3D = GHVertex3D GVec4 GVec4 GVec2 deriving Generic

instance MultiShaderType GVertex3D
instance ShaderInput GVertex3D
instance GLES => GeometryVertex GVertex3D where
        type Vertex GVertex3D = Vertex3D

instance MultiShaderType GHVertex3D
instance ShaderInput GHVertex3D
