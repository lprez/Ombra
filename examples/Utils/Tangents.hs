{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Utils.Tangents where

import GHC.Generics
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Vector

import Utils.Vertex3D

data Vertex3DT = Vertex3DT Vec3 Vec3 Vec2 Vec3 deriving Generic
data GVertex3DT = GVertex3DT GVec3 GVec3 GVec2 GVec3 deriving Generic
data GHVertex3DT = GVertex3DT GVec4 GVec4 GVec2 GVec4 deriving Generic

instance MultiShaderType GHVertex3DT
instance ShaderInput GHVertex3DT

instance MultiShaderType GVertex3DT
instance ShaderInput GVertex3DT
instance GLES => GeometryVertex GVertex3DT where
        type Vertex GVertex3DT = Vertex3DT

addTangents :: GLES
            => Geometry Triangle GVertex3D
            -> Geometry Triangle GVertex3DT
addTangents = mapGeometry calcTangent addTangent
        where addTangent ts (Vertex3D p n u) = Vertex3DT p n u $ foldl1 (^+^) ts

calcTangent :: Triangle Vertex3D -> Vec3
calcTangent (Triangle (Vertex3D p1 n1 u1)
                      (Vertex3D p2 n2 u2)
                      (Vertex3D p3 n3 u3)) =
        let dp1 = p2 ^-^ p1
            dp2 = p3 ^-^ p1
            Vec2 du1x du1y = u2 ^-^ u1
            Vec2 du2x du2y = u3 ^-^ u1
            f = 1 / (du1x * du2y - du1y * du2x)
            tangent = normalized $ f *^ (du2y *^ dp1 ^-^ du1y *^ dp2)
            bitangent = normalized $ f *^ (du1x *^ dp2 ^-^ du2x *^ dp1)
            handedness = signum $ cross3 n1 tangent <.> bitangent
            tangent' = handedness *^ tangent
        in tangent'
