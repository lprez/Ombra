{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
             PolyKinds, TypeFamilies, RankNTypes, ConstraintKinds,
             UndecidableInstances #-}

-- |
-- Module:      Graphics.Rendering.Ombra.Geometry
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: GHC only

module Graphics.Rendering.Ombra.Geometry (
        Geometry,
        Triangle(..),
        mkGeometry,
        mapVertices,
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
        -- *
        GeometryVertex(..)
) where

import Control.Monad.Trans.State
import Data.Foldable (foldlM)
import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as H
import Data.List (foldl')
import Data.Proxy
import Data.Word (Word16)

import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Internal.TList (Remove, Append)
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Vector

rehashGeometry :: Geometry g -> Geometry g
rehashGeometry g = let Triangles elemsHash _ = elements g
                   in g { geometryHash = H.hashWithSalt (topHash g) elemsHash }

emptyGeometry :: GeometryVertex g => Geometry g
emptyGeometry = rehashGeometry $ Geometry 0 0 emptyAttrCol (Triangles 0 []) (-1)

foldVertices :: NotTop p
             => (AttrVertex is -> b -> b)
             -> b
             -> AttrTable p is
             -> (Int, b)
foldVertices f acc AttrEnd = (-1, acc)
foldVertices f acc cell@(AttrCell _ _ down) =
        let (didx, acc') = foldVertices f acc down
            idx = didx + 1
            widx = fromIntegral idx
        in (idx, f (AttrVertex widx cell) acc')

addVertex :: GeometryVertex g
          => VertexAttributes (AttributeTypes g)
          -> Geometry g
          -> (AttrVertex (AttributeTypes g), Geometry g)
addVertex v g =
        let top' = addTop v $ top g
            topHash = H.hash top'
            idx = lastIndex g + 1
            av = case top' of
                      AttrTop _ _ c -> AttrVertex (fromIntegral idx) c
        in ( av
           , rehashGeometry $ g { topHash = topHash
                                , top = top'
                                , lastIndex = idx
                                }
           )

addTriangle :: GeometryVertex g
            => Triangle (AttrVertex (AttributeTypes g))
            -> Geometry g
            -> Geometry g
addTriangle t g = let Triangles h ts = elements g
                      elements' = Triangles (H.hashWithSalt (H.hash t) h)
                                            (t : ts)
                  in rehashGeometry $ g { elements = elements' }

-- | Create a new vertex that can be used in 'addTriangle'.
vertex :: (Monad m, GeometryVertex g)
       => Vertex g
       -> GeometryBuilderT g m (AttrVertex (AttributeTypes g))
vertex = GeometryBuilderT . state . addVertex . toVertexAttributes

-- | Add a triangle to the current geometry.
triangle :: (Monad m, GeometryVertex g)
         => AttrVertex (AttributeTypes g)
         -> AttrVertex (AttributeTypes g)
         -> AttrVertex (AttributeTypes g)
         -> GeometryBuilderT g m ()
triangle x y z = GeometryBuilderT . state $ \g -> ((), addTriangle t g)
        where t = Triangle x y z

-- | Create a 'Geometry' using the 'GeometryBuilder' monad. This is more
-- efficient than 'mkGeometry'.
buildGeometry :: GeometryVertex g => GeometryBuilder g () -> Geometry g
buildGeometry (GeometryBuilderT m) = execState m emptyGeometry

buildGeometryT :: (Monad m, GeometryVertex g)
               => GeometryBuilderT g m ()
               -> m (Geometry g)
buildGeometryT (GeometryBuilderT m) = execStateT m emptyGeometry

-- | Create a 'Geometry' using a list of triangles.
mkGeometry :: (GLES, GeometryVertex g)
           => [Triangle (Vertex g)]
           -> Geometry g
mkGeometry t = buildGeometry (foldlM add H.empty t >> return ())
        where add verts (Triangle v1 v2 v3) =
                do (verts1, av1) <- mvertex verts $ toVertexAttributes v1
                   (verts2, av2) <- mvertex verts1 $ toVertexAttributes v2
                   (verts3, av3) <- mvertex verts2 $ toVertexAttributes v3
                   triangle av1 av2 av3
                   return verts3
              mvertex vertices v =
                case H.lookup v vertices of
                     Just av -> return (vertices, av)
                     Nothing -> do av <- vertex $ fromVertexAttributes v
                                   return (H.insert v av vertices, av)

attrVertexToVertex :: Attributes is => AttrVertex is -> VertexAttributes is
attrVertexToVertex (AttrVertex _ tab) = rowToVertexAttributes tab

-- | Convert a 'Geometry' back to a list of triangles.
decompose :: GeometryVertex g => Geometry g -> [Triangle (Vertex g)] 
decompose g@(Geometry _ _ _ (Triangles _ triangles) _) =
        flip map triangles $ fmap (fromVertexAttributes . attrVertexToVertex)

type AttrVertexMap is v = H.HashMap (AttrVertex is) v

-- | Transform each vertex of a geometry. You can create a value for each
-- triangle so that the transforming function will receive a list of the values
-- of the triangles the vertex belongs to.
mapVertices :: forall a g g'. (GLES, GeometryVertex g, GeometryVertex g')
            => (Triangle (Vertex g) -> a)
            -> ([a] -> Vertex g -> Vertex g')
            -> Geometry g
            -> Geometry g'
mapVertices getValue (transVert :: [a] -> Vertex is -> Vertex is')
            (Geometry _ _ (AttrTop _ _ row0) (Triangles thash triangles) _) =
        let accTriangle vertMap tri@(Triangle v1 v2 v3) (values, triangles) =
                    let value = getValue $ fmap ( fromVertexAttributes
                                                . attrVertexToVertex
                                                ) tri
                        values' = foldr (flip (H.insertWith (++)) [value])
                                        values
                                        [v1, v2, v3]
                        tri' = fmap (vertMap H.!) tri
                    in (values', tri' : triangles)

            accVertex :: H.HashMap (AttrVertex (AttributeTypes g)) [a]
                      -> AttrVertex (AttributeTypes g)
                      -> ( H.HashMap (AttrVertex (AttributeTypes g))
                                     (AttrVertex (AttributeTypes g'))
                         , Geometry g'
                         )
                      -> ( H.HashMap (AttrVertex (AttributeTypes g))
                                     (AttrVertex (AttributeTypes g'))
                         , Geometry g'
                         )
            accVertex valueMap avert (vertMap, geom) =
                    let value = valueMap H.! avert
                        vert = fromVertexAttributes $ attrVertexToVertex avert
                        vert' = toVertexAttributes $ transVert value vert
                        (avert', geom') = addVertex vert' geom
                        vertMap' = H.insert avert avert' vertMap
                    in (vertMap', geom')

            (valueMap, triangles') = foldr (accTriangle vertMap)
                                           (H.empty, [])
                                           triangles
            (_, (vertMap, Geometry tophash' _ top' _ lidx)) =
                    foldVertices (accVertex valueMap)
                                 (H.empty, emptyGeometry)
                                 row0
            geom' = Geometry tophash' 0 top' (Triangles thash triangles') lidx
        in rehashGeometry geom'
