{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Main where

import Control.Arrow
import Data.MemoTrie
import GHC.Generics
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Geometry.Internal
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Language.Types (Expr)
import Graphics.Rendering.Ombra.Stream
import Graphics.Rendering.Ombra.Vector
import Utils.Play

data Vertex2D = Vertex2D Vec2 Vec3 deriving Generic
data GVertex2D = GVertex2D GVec2 GVec3 deriving Generic

instance MultiShaderType GVertex2D
instance ShaderInput GVertex2D
instance GeometryVertex GVertex2D where
        type Vertex GVertex2D = Vertex2D

tris :: Geometry GVertex2D
tris = mkGeometry [Triangle (Vertex2D (Vec2 0 0) (Vec3 1 0 0))
                            (Vertex2D (Vec2 0.5 0.5) (Vec3 0 1 0))
                            (Vertex2D (Vec2 0 0.5) (Vec3 0 0 1))]

render :: Float -> Draw ()
render time = draw $ fragmentStream (vs $ UniformSetter time) fs tris
        where vs = shader1 . uniform' id . arr $ \(gtime, GVertex2D p c) -> 
                        ( (p ^+^ (GVec2 (sin gtime) (cos gtime / 8))) ^| 0 ^| 1
                        , c)
              fs = shader . arr $ \c -> [c ^| 1]

main :: IO ()
main = animation render
