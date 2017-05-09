{-# LANGUAGE GADTs, KindSignatures, DataKinds, ScopedTypeVariables,
             TypeOperators, FlexibleContexts #-}

module Graphics.Rendering.Ombra.Geometry (
        Geometry,
        Vertex(..),
        Triangle(..),
        mkGeometry,
        removeAttribute,
        decompose,
        -- * Geometry builder
        Attributes,
        AttrVertex,
        GeometryBuilder,
        GeometryBuilderT,
        vertex,
        triangle,
        buildGeometry,
        buildGeometryT,
        -- * 2D and 3D geometries
        D2.Geometry2D,
        D3.Geometry3D,
        mkGeometry2D,
        mkGeometry3D,
        mkGeometry2D',
        mkGeometry3D',
        vertex2D,
        vertex3D
) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable (foldrM)
import Data.Hashable
import Data.Proxy
import Data.Word (Word16)
import qualified Data.HashTable.ST.Basic as H
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Internal.TList (Append)
import Graphics.Rendering.Ombra.Geometry.Internal
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Default2D (Position2)
import Graphics.Rendering.Ombra.Shader.Default3D (Position3, Normal3)
import qualified Graphics.Rendering.Ombra.Shader.Default2D as D2
import qualified Graphics.Rendering.Ombra.Shader.Default3D as D3
import Graphics.Rendering.Ombra.Vector

-- | Create a 3D 'Geometry'.
mkGeometry3D :: GLES
             => [Triangle (Vec3, Vec2, Vec3)] -- ^ (Position, UV, Normal)
             -> Geometry D3.Geometry3D
mkGeometry3D = mkGeometry . map (fmap $ \(v, u, n) -> mkVertex3D v u n)

-- | Create an extended 3D 'Geometry'.
mkGeometry3D' :: (GLES, Attributes (Append (i ': is) D3.Geometry3D))
              => [Triangle (Vertex (i ': is), Vec3, Vec2, Vec3)]
              -> Geometry (Append (i ': is) D3.Geometry3D)
mkGeometry3D' = mkGeometry .
                map (fmap $ \(e, v, u, n) -> extend e $ mkVertex3D v u n)

-- | Create a 2D 'Geometry'.
mkGeometry2D :: GLES
             => [Triangle (Vec2, Vec2)] -- ^ (Position, Texture UV coordinates)
             -> Geometry D2.Geometry2D
mkGeometry2D = mkGeometry . map (fmap $ \(v, u) -> mkVertex2D v u)

-- | Create an extended 2D 'Geometry'.
mkGeometry2D' :: (GLES, Attributes (Append (i ': is) D2.Geometry2D))
              => [Triangle (Vertex (i ': is), Vec2, Vec2)]
              -> Geometry (Append (i ': is) D2.Geometry2D)
mkGeometry2D' = mkGeometry .
                map (fmap $ \(e, v, u) -> extend e $ mkVertex2D v u)

-- | Create a new 2D vertex.
vertex2D :: (GLES, Monad m)
         => Vec2
         -> Vec2
         -> GeometryBuilderT D2.Geometry2D m (AttrVertex D2.Geometry2D)
vertex2D p u = vertex $ mkVertex2D p u

-- | Create a new 3D vertex.
vertex3D :: (GLES, Monad m)
         => Vec3
         -> Vec2
         -> Vec3
         -> GeometryBuilderT D3.Geometry3D m (AttrVertex D3.Geometry3D)
vertex3D p u n = vertex $ mkVertex3D p u n

mkVertex3D :: GLES => Vec3 -> Vec2 -> Vec3 -> Vertex D3.Geometry3D
mkVertex3D p u n = Attr D3.Position3 p :~ Attr D3.UV u :~ Attr D3.Normal3 n

mkVertex2D :: GLES => Vec2 -> Vec2 -> Vertex D2.Geometry2D
mkVertex2D p u = Attr D2.Position2 p :~ Attr D2.UV u

extend :: Vertex is -> Vertex is' -> Vertex (Append is is')
extend (Attr c x) v = Attr c x :~ v
extend (Attr c x :~ v') v = Attr c x :~ extend v' v
