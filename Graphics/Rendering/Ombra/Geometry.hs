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
        Point(..),
        Line(..),
        Triangle(..),
        mkGeometry,
        mapVertices,
        foldGeometry,
        decompose,
        -- * Geometry builder
        Attributes,
        AttrVertex,
        GeometryBuilder,
        GeometryBuilderT,
        vertex,
        point,
        line,
        triangle,
        buildGeometry,
        buildGeometryT,
        -- *
        GeometryVertex(..),
        ElementType
) where

import Control.Monad.Trans.State
import Data.Foldable (toList, foldlM, foldrM)
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

rehashGeometry :: Geometry e g -> Geometry e g
rehashGeometry g = let Elements elemsHash _ = elements g
                   in g { geometryHash = H.hashWithSalt (topHash g) elemsHash }

emptyGeometry :: GeometryVertex g => Geometry e g
emptyGeometry = rehashGeometry $ Geometry 0 0 emptyAttrCol (Elements 0 []) (-1)

foldAttrVertices :: NotTop p
                 => (AttrVertex is -> b -> b)
                 -> b
                 -> AttrTable p is
                 -> (Int, b)
foldAttrVertices f acc AttrEnd = (-1, acc)
foldAttrVertices f acc cell@(AttrCell _ _ down) =
        let (didx, acc') = foldAttrVertices f acc down
            idx = didx + 1
            widx = fromIntegral idx
        in (idx, f (AttrVertex widx cell) acc')

addVertex :: GeometryVertex g
          => VertexAttributes (AttributeTypes g)
          -> Geometry e g
          -> (AttrVertex (AttributeTypes g), Geometry e g)
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

addElement :: (GeometryVertex g, H.Hashable (e (AttrVertex (AttributeTypes g))))
           => e (AttrVertex (AttributeTypes g))
           -> Geometry e g
           -> Geometry e g
addElement t g = let Elements h ts = elements g
                     elements' = Elements (H.hashWithSalt (H.hash t) h)
                                          (t : ts)
                  in rehashGeometry $ g { elements = elements' }

-- | Create a new vertex that can be used in 'triangle', 'line' and 'point'.
vertex :: (Monad m, GeometryVertex g)
       => Vertex g
       -> GeometryBuilderT e g m (AttrVertex (AttributeTypes g))
vertex = GeometryBuilderT . state . addVertex . toVertexAttributes

-- | Add a point to the current geometry.
point :: (Monad m, GeometryVertex g)
      => AttrVertex (AttributeTypes g)
      -> GeometryBuilderT Point g m ()
point x = GeometryBuilderT . state $ \g -> ((), addElement (Point x) g)

-- | Add a line to the current geometry.
line :: (Monad m, GeometryVertex g)
     => AttrVertex (AttributeTypes g)
     -> AttrVertex (AttributeTypes g)
     -> GeometryBuilderT Line g m ()
line x y = GeometryBuilderT . state $ \g -> ((), addElement t g)
        where t = Line x y

-- | Add a triangle to the current geometry.
triangle :: (Monad m, GeometryVertex g)
         => AttrVertex (AttributeTypes g)
         -> AttrVertex (AttributeTypes g)
         -> AttrVertex (AttributeTypes g)
         -> GeometryBuilderT Triangle g m ()
triangle x y z = GeometryBuilderT . state $ \g -> ((), addElement t g)
        where t = Triangle x y z

-- | Create a 'Geometry' using the 'GeometryBuilder' monad. This is more
-- efficient than 'mkGeometry'.
buildGeometry :: GeometryVertex g => GeometryBuilder e g () -> Geometry e g
buildGeometry (GeometryBuilderT m) = execState m emptyGeometry

buildGeometryT :: (Monad m, GeometryVertex g)
               => GeometryBuilderT e g m ()
               -> m (Geometry e g)
buildGeometryT (GeometryBuilderT m) = execStateT m emptyGeometry

-- | Create a 'Geometry' using a list of points, lines or triangles.
mkGeometry :: ( GLES
              , GeometryVertex g
              , ElementType e
              , H.Hashable (e (AttrVertex (AttributeTypes g)))
              )
           => [e (Vertex g)]    -- ^ List of elements.
           -> Geometry e g
mkGeometry t = buildGeometry (foldlM add H.empty t >> return ())
        where add verts e =
                do vsavs <- foldrM (\v (verts, avs) ->
                                        do let attrs = toVertexAttributes v
                                           (verts', av) <- mvertex verts attrs
                                           return (verts', av : avs))
                                   (verts, [])
                                   e
                   let ae = elementFromList $ snd vsavs
                   GeometryBuilderT . state $ \g -> ((), addElement ae g)
                   return $ fst vsavs
              mvertex vertices v =
                case H.lookup v vertices of
                     Just av -> return (vertices, av)
                     Nothing -> do av <- vertex $ fromVertexAttributes v
                                   return (H.insert v av vertices, av)

attrVertexToVertex :: Attributes is => AttrVertex is -> VertexAttributes is
attrVertexToVertex (AttrVertex _ tab) = rowToVertexAttributes tab

-- | Convert a 'Geometry' back to a list of elements.
decompose :: (GeometryVertex g, Functor e) => Geometry e g -> [e (Vertex g)] 
decompose g@(Geometry _ _ _ (Elements _ elems) _) =
        flip map elems $ fmap (fromVertexAttributes . attrVertexToVertex)

type AttrVertexMap is v = H.HashMap (AttrVertex is) v

-- | Transform each vertex of a geometry.
mapVertices :: (GLES, GeometryVertex g, GeometryVertex g', ElementType e)
            => (e (Vertex g) -> a)            -- ^ Value to associate to each
                                              -- element.
            -> ([a] -> Vertex g -> Vertex g') -- ^ The first argument is the
                                              -- list of values associated with
                                              -- the elements the vertex belongs
                                              -- to.
            -> Geometry e g
            -> Geometry e g'
mapVertices valf f = 
        let addValue elem valMap = let val = valf $ fmap ( fromVertexAttributes
                                                         . attrVertexToVertex
                                                         ) elem
                                   in foldr (flip (H.insertWith (++)) [val])
                                            valMap
                                            (toList elem)
            mapVertex valMap avert _ = let attrs = attrVertexToVertex avert
                                           vert = fromVertexAttributes attrs
                                           vert' = f (valMap H.! avert) vert
                                           attrs' = toVertexAttributes vert'
                                       in ((), attrs')
        in snd . modifyVertices addValue mapVertex H.empty ()

-- | Fold elements and then vertices.
foldGeometry :: forall g e vacc eacc. (GLES, GeometryVertex g, ElementType e)
             => (e (Vertex g) -> eacc -> eacc)
             -> (eacc -> Vertex g -> vacc -> vacc)
             -> eacc
             -> vacc
             -> Geometry e g
             -> (eacc, vacc)
foldGeometry ef vf eacc vacc g =
        let accElems e = ef $ fmap (fromVertexAttributes . attrVertexToVertex) e
            accVerts eacc av vacc = let v = attrVertexToVertex av
                                        vacc' = vf eacc
                                                   (fromVertexAttributes v)
                                                   vacc
                                    in (vacc', v)
            (accs', _) = modifyVertices accElems accVerts eacc vacc g
                                :: ((eacc, vacc), Geometry e g)
        in accs'

-- | Fold triangles, then map and fold vertices using the previously accumulated
-- value.
modifyVertices :: forall e eacc vacc g g'.
                  (GLES, GeometryVertex g, GeometryVertex g', ElementType e)
               => (e (AttrVertex (AttributeTypes g)) -> eacc -> eacc)
               -> (   eacc
                   -> AttrVertex (AttributeTypes g)
                   -> vacc
                   -> (vacc, VertexAttributes (AttributeTypes g'))
                  )
               -> eacc
               -> vacc
               -> Geometry e g
               -> ((eacc, vacc), Geometry e g')
modifyVertices ef vf eacc vacc
               (Geometry _ _ (AttrTop _ _ row0) (Elements thash elems) _) =
        let accElem vertMap elem (eacc, elems) =
                    (ef elem eacc, fmap (vertMap H.!) elem : elems)
            accVertex eacc avert (vertMap, vacc, (geom :: Geometry e g')) =
                    let (vacc', attrs') = vf eacc avert vacc
                        (avert', geom') = addVertex attrs' geom
                        vertMap' = H.insert avert avert' vertMap
                    in (vertMap', vacc', geom')

            (eacc', elems') = foldr (accElem vertMap) (eacc, []) elems
            (_, (vertMap, vacc', Geometry tophash' _ top' _ lidx)) =
                    foldAttrVertices (accVertex eacc')
                                     (H.empty, vacc, emptyGeometry)
                                     row0
            geom' = Geometry tophash' 0 top' (Elements thash elems') lidx
        in ((eacc', vacc'), rehashGeometry geom')
