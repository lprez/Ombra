{-# LANGUAGE DeriveGeneric, DataKinds, TypeOperators, TypeFamilies #-}

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

data Vertex2D = Vertex2D Vec2 Vec3

data GVertex2D = GVertex2D GVec2 GVec3 deriving Generic

data Vertex2DExpr = Vertex2DExpr Expr Expr deriving Generic

instance HasTrie Vertex2DExpr where
        newtype (Vertex2DExpr :->: b) =
                Vertex2DTrieExpr { unVertex2DExprTrie
                                        :: Reg Vertex2DExpr :->: b }
        trie = trieGeneric Vertex2DTrieExpr
        untrie = untrieGeneric unVertex2DExprTrie
        enumerate = enumerateGeneric unVertex2DExprTrie

instance MultiShaderType GVertex2D where
        type ExprMST GVertex2D = Vertex2DExpr
        mapMST f (GVertex2D p c) = GVertex2D (f p) (f c)
        foldrMST f s (GVertex2D p c) = f p (f c s)
        toExprMST (GVertex2D p c) = Vertex2DExpr (toExprMST p) (toExprMST c)
        fromExprMST (Vertex2DExpr p c) = GVertex2D (fromExprMST p)
                                                  (fromExprMST c)

instance ShaderInput GVertex2D where
        buildMST f i = (GVertex2D (f i) (f $ i + 1), i + 2)

instance GVertex GVertex2D where
        type AttributeTypes GVertex2D = '[GVec2, GVec3]
        type Vertex GVertex2D = Vertex2D
        toVertexAttributes (Vertex2D p c) = Attr p :~ Attr c
        fromVertexAttributes (Attr p :~ Attr c) = Vertex2D p c

tris :: Geometry GVertex2D
tris = mkGeometry [Triangle (Vertex2D (Vec2 0 0) (Vec3 1 0 0))
                            (Vertex2D (Vec2 0.5 0.5) (Vec3 0 1 0))
                            (Vertex2D (Vec2 0 0.5) (Vec3 0 0 1))]

render :: Float -> Draw ()
render time = draw $ fragmentStream (vs $ UniformSetter time) fs tris
        where vs = shader1 . uniform' id . arr $ \(gtime, GVertex2D p c) -> 
                        ( (p ^+^ (GVec2 (gtime / 2) (gtime / 8))) ^| 0 ^| 1
                        , c)
              fs = shader . arr $ \c -> [c ^| 1]

main :: IO ()
main = animation render
