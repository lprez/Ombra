{-# LANGUAGE GADTs, KindSignatures, DataKinds, ScopedTypeVariables,
             TypeOperators, FlexibleContexts #-}

module Graphics.Rendering.Ombra.Geometry (
        Geometry,
        Vertex(..),
        Triangle(..),
        mkGeometry,
        mkGeometryInd,
        removeAttribute,
        -- * 2D and 3D geometries
        Geometry2D,
        Geometry3D,
        positionOnly,
        mkGeometry2D,
        mkGeometry3D,
        mkGeometry2D',
        mkGeometry3D',
        mkGeometry2DInd,
        mkGeometry3DInd,
        mkGeometry2DInd',
        mkGeometry3DInd',
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
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Default2D (Position2)
import Graphics.Rendering.Ombra.Shader.Default3D (Position3, Normal3)
import qualified Graphics.Rendering.Ombra.Shader.Default2D as D2
import qualified Graphics.Rendering.Ombra.Shader.Default3D as D3
import Graphics.Rendering.Ombra.Vector

data Triangle a = Triangle a a a

-- | A list of the attributes of a vertex.
--
-- For instance: @Attr Position3 p :~ Attr UV u :~ Attr Normal3 n@
data Vertex (is :: [*]) where
        Attr :: (Hashable (CPU S i), Attribute S i)
             => (a -> i)
             -> CPU S i
             -> Vertex '[i]
        (:~) :: Vertex '[i] -> Vertex is -> Vertex (i ': is)

infixr 5 :~

-- | A 3D geometry.
type Geometry3D = '[Position3, D3.UV, D3.Normal3]

-- | A 2D geometry.
type Geometry2D = '[Position2, D2.UV]

-- | Create a generic 'Geometry'.
mkGeometry :: (GLES, Attributes is)
           => [Triangle (Vertex is)]
           -> Geometry is
mkGeometry (ts :: [Triangle (Vertex is)]) =
        geometry attrList (ElemData (map (lastElem -) elemList) (hash elemList))
        where (attrList, elemList, lastElem) =
                runST (H.new >>= \table ->
                        foldrM (\(Triangle x y z) -> addVertex table x <=<
                                                     addVertex table y <=<
                                                     addVertex table z)
                               (emptyAttrList (Proxy :: Proxy is), [], 0)
                               ts)

              addVertex :: H.HashTable s Int Word16
                        -> Vertex is
                        -> (AttrList is, [Word16], Word16)
                        -> ST s (AttrList is, [Word16], Word16)
              addVertex t v (attrList, elemList, lastElem) =
                      do melem <- H.lookup t $ hash v
                         (newElem, attrList') <-
                                case melem of
                                     Just elem -> return (elem, attrList)
                                     Nothing -> do H.insert t (hash v)
                                                              (lastElem + 1)
                                                   return ( lastElem + 1
                                                          , addAttrList v
                                                                attrList
                                                          )
                         let lastElem' = max lastElem newElem
                         return (attrList', newElem : elemList, lastElem')

-- | Create a 'Geometry' using a list of indices to a list of vertices. This
-- is faster than 'mkGeometry'.
mkGeometryInd :: (GLES, Attributes is)
              => [Vertex is]
              -> [Triangle Word16]
              -> Geometry is
mkGeometryInd (vs :: [Vertex is]) ts = 
        geometry attrList $ ElemData elemList (hash elemList)
        where elemList = foldr (\(Triangle x y z) l -> x : y : z : l) [] ts
              attrList = foldr addAttrList
                               (emptyAttrList (Proxy :: Proxy is))
                               vs
        
addAttrList :: Vertex is -> AttrList is -> AttrList is
addAttrList (Attr _ x) (AttrListCons (AttrData xs h) rest) =
        AttrListCons (AttrData (x : xs) $ hashWithSalt (hash x) h) rest
addAttrList (Attr _ x :~ v') (AttrListCons (AttrData xs h) rest) =
        AttrListCons (AttrData (x : xs) $ hashWithSalt (hash x) h) $
                addAttrList v' rest

-- | Create a 3D 'Geometry'.
mkGeometry3D :: GLES
             => [Triangle (Vec3, Vec2, Vec3)] -- ^ (Position, UV, Normal)
             -> Geometry Geometry3D
mkGeometry3D = mkGeometry . map (fmap $ \(v, u, n) -> vertex3D v u n)

-- | Create an extended 3D 'Geometry'.
mkGeometry3D' :: (GLES, Attributes (Append is Geometry3D))
              => [Triangle (Vertex is, Vec3, Vec2, Vec3)]
              -> Geometry (Append is Geometry3D)
mkGeometry3D' = mkGeometry .
                map (fmap $ \(e, v, u, n) -> extend e $ vertex3D v u n)

-- | Create a 2D 'Geometry'.
mkGeometry2D :: GLES
             => [Triangle (Vec2, Vec2)] -- ^ (Position, Texture UV coordinates)
             -> Geometry Geometry2D
mkGeometry2D = mkGeometry . map (fmap $ \(v, u) -> vertex2D v u)

-- | Create an extended 2D 'Geometry'.
mkGeometry2D' :: (GLES, Attributes (Append is Geometry2D))
              => [Triangle (Vertex is, Vec2, Vec2)]
              -> Geometry (Append is Geometry2D)
mkGeometry2D' = mkGeometry . map (fmap $ \(e, v, u) -> extend e $ vertex2D v u)

-- | Create a 3D 'Geometry' using a list of indices.
mkGeometry3DInd :: GLES
                => [(Vec3, Vec2, Vec3)]
                -> [Triangle Word16]
                -> Geometry Geometry3D
mkGeometry3DInd = mkGeometryInd . map (\(v, u, n) -> vertex3D v u n)

-- | Create an extended 3D 'Geometry' using a list of indices.
mkGeometry3DInd' :: (GLES, Attributes (Append is Geometry3D))
                 => [(Vertex is, Vec3, Vec2, Vec3)]
                 -> [Triangle Word16]
                 -> Geometry (Append is Geometry3D)
mkGeometry3DInd' = mkGeometryInd .
                   map (\(e, v, u, n) -> extend e $ vertex3D v u n)

-- | Create a 2D 'Geometry' using a list of indices.
mkGeometry2DInd :: GLES
                => [(Vec2, Vec2)]
                -> [Triangle Word16]
                -> Geometry Geometry2D
mkGeometry2DInd = mkGeometryInd . map (\(v, u) -> vertex2D v u)

-- | Create an extended 2D 'Geometry' using a list of indices.
mkGeometry2DInd' :: (GLES, Attributes (Append is Geometry2D))
                 => [(Vertex is, Vec2, Vec2)]
                 -> [Triangle Word16]
                 -> Geometry (Append is Geometry2D)
mkGeometry2DInd' = mkGeometryInd .  map (\(e, v, u) -> extend e $ vertex2D v u)

vertex3D :: GLES => Vec3 -> Vec2 -> Vec3 -> Vertex Geometry3D
vertex3D p u n = Attr D3.Position3 p :~ Attr D3.UV u :~ Attr D3.Normal3 n

vertex2D :: GLES => Vec2 -> Vec2 -> Vertex Geometry2D
vertex2D p u = Attr D2.Position2 p :~ Attr D2.UV u

extend :: Vertex is -> Vertex is' -> Vertex (Append is is')
extend (Attr c x) v = Attr c x :~ v
extend (Attr c x :~ v') v = Attr c x :~ extend v' v

-- | Remove the 'UV' and 'Normal3' attributes from a 3D Geometry.
positionOnly :: Geometry Geometry3D -> Geometry '[Position3]
positionOnly (Geometry (AttrListCons pd _) es _) =
        geometry (AttrListCons pd AttrListNil) es

instance Hashable (Vertex is) where
        hashWithSalt s (Attr _ a) = hashWithSalt s a
        hashWithSalt s (x :~ y) = hashWithSalt (hashWithSalt s x) y

instance Functor Triangle where
        fmap f (Triangle x y z) = Triangle (f x) (f y) (f z)
