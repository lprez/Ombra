{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
             PolyKinds, TypeFamilies, RankNTypes, ConstraintKinds,
             UndecidableInstances #-}

module Graphics.Rendering.Ombra.Geometry.Internal (
        MonadGeometry(..),
        LoadedBuffer,
        LoadedAttribute,
        LoadedGeometry(..),
        vertex,
        triangle,
        mkGeometry,
        buildGeometry,
        buildGeometryT,
        decompose,
        mapVertices,
        drawGeometry
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Foldable (foldlM)
import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as H
import Data.List (foldl')
import Data.Proxy
import Data.Word (Word16)

import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Internal.TList (Remove, Append)
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language.Types (ShaderType(size))
import Graphics.Rendering.Ombra.Vector

class (GLES, MonadGL m) => MonadGeometry m where
        getAttribute :: BaseAttribute i
                     => AttrCol (i ': is)
                     -> m (Either String LoadedAttribute)
        getElementBuffer :: Elements is -> m (Either String LoadedBuffer)
        getGeometry :: GVertex g
                    => Geometry g
                    -> m (Either String LoadedGeometry)


data LoadedGeometry = LoadedGeometry {
        -- elementType :: GLEnum,
        elementCount :: Int,
        vao :: VertexArrayObject
}

newtype LoadedBuffer = LoadedBuffer Buffer

data LoadedAttribute = LoadedAttribute GLUInt [(Buffer, GLUInt -> GL ())]

rehashGeometry :: Geometry g -> Geometry g
rehashGeometry g = let Triangles elemsHash _ = elements g
                   in g { geometryHash = H.hashWithSalt (topHash g) elemsHash }

emptyGeometry :: GVertex g => Geometry g
emptyGeometry = rehashGeometry $ Geometry 0 0 emptyAttrCol (Triangles 0 []) (-1)

downList :: NotTop p => AttrTable p (i ': is) -> [CPUBase i] -> [CPUBase i]
downList AttrEnd xs = xs
downList (AttrCell x _ down) xs = downList down $ x : xs

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

addVertex :: GVertex g
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

addTriangle :: GVertex g
            => Triangle (AttrVertex (AttributeTypes g))
            -> Geometry g
            -> Geometry g
addTriangle t g = let Triangles h ts = elements g
                      elements' = Triangles (H.hashWithSalt (H.hash t) h)
                                            (t : ts)
                  in rehashGeometry $ g { elements = elements' }

-- | Create a new vertex that can be used in 'addTriangle'.
vertex :: (Monad m, GVertex g)
       => Vertex g
       -> GeometryBuilderT g m (AttrVertex (AttributeTypes g))
vertex = GeometryBuilderT . state . addVertex . toVertexAttributes

-- | Add a triangle to the current geometry.
triangle :: (Monad m, GVertex g)
         => AttrVertex (AttributeTypes g)
         -> AttrVertex (AttributeTypes g)
         -> AttrVertex (AttributeTypes g)
         -> GeometryBuilderT g m ()
triangle x y z = GeometryBuilderT . state $ \g -> ((), addTriangle t g)
        where t = Triangle x y z

-- | Create a 'Geometry' using the 'GeometryBuilder' monad. This is more
-- efficient than 'mkGeometry'.
buildGeometry :: GVertex g => GeometryBuilder g () -> Geometry g
buildGeometry (GeometryBuilderT m) = execState m emptyGeometry

buildGeometryT :: (Monad m, GVertex g)
               => GeometryBuilderT g m ()
               -> m (Geometry g)
buildGeometryT (GeometryBuilderT m) = execStateT m emptyGeometry

-- | Create a 'Geometry' using a list of triangles.
mkGeometry :: (GLES, GVertex g)
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
decompose :: GVertex g => Geometry g -> [Triangle (Vertex g)] 
decompose g@(Geometry _ _ _ (Triangles _ triangles) _) =
        flip map triangles $ fmap (fromVertexAttributes . attrVertexToVertex)

type AttrVertexMap is v = H.HashMap (AttrVertex is) v

-- | Transform each vertex of a geometry. You can create a value for each
-- triangle so that the transforming function will receive a list of the values
-- of the triangles the vertex belongs to.
mapVertices :: forall a g g'. (GLES, GVertex g, GVertex g')
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

{-
-- | Remove an attribute from a geometry.
removeAttribute :: ( RemoveAttr i is
                   , Attributes is
                   , Attributes (Remove i is)
                   , GLES
                   )
                => (a -> i)      -- ^ Attribute constructor (or any other
                                 -- function with that type).
                -> Geometry is
                -> Geometry (Remove i is)
removeAttribute g = mapVertices (const ()) (const $ removeAttr g)

class RemoveAttr i is where
        removeAttr :: (a -> i) -> Vertex is -> Vertex (Remove i is)

instance {-# OVERLAPPING #-} (Remove i '[i', i] ~ '[i']) =>
        RemoveAttr i '[i', i] where
        removeAttr g (Attr g' x :~ _) = Attr g' x

instance {-# OVERLAPPING #-} RemoveAttr i is' => RemoveAttr i (i ': is') where
        removeAttr g (Attr _ _ :~ v) = removeAttr g v

instance {-# OVERLAPPABLE #-} ( RemoveAttr i is'
                              , Remove i (i' ': is') ~ (i' ': Remove i is')
                              ) => RemoveAttr i (i' ': is') where
        removeAttr g (Attr g' x) = Attr g' x
        removeAttr g (Attr g' x :~ v) = Attr g' x :~ removeAttr g v
-}

instance GLES => Resource (AttrCol (i ': is)) LoadedAttribute GL where
        loadResource (AttrTop _ _ down :: AttrCol (i ': is)) =
                fmap (Right . uncurry LoadedAttribute) .
                flip execStateT (0, []) $
                        do (i, as) <- get
                           arr <- lift $ encodeAttribute (Proxy :: Proxy i) vs
                           buf <- lift $ loadBuffer gl_ARRAY_BUFFER arr
                           let sz = fromIntegral . size $ (undefined :: i)
                               set = setAttribute (Proxy :: Proxy i) . (+ i)
                           put (i + sz, (buf, set) : as)
                where vs = downList down []
        unloadResource _ (LoadedAttribute _ as) =
                mapM_ (\(buf, _) -> deleteBuffer buf) as

instance GLES => Resource (Elements is) LoadedBuffer GL where
        loadResource (Triangles _ ts) =
                liftIO (encodeUInt16s elems) >>=
                        fmap (Right . LoadedBuffer) .
                        loadBuffer gl_ELEMENT_ARRAY_BUFFER
                        . fromUInt16Array
                where elems = ts >>= ids
                      ids (Triangle (AttrVertex x _)
                                    (AttrVertex y _)
                                    (AttrVertex z _)) = [x, y, z]
        unloadResource _ (LoadedBuffer buf) = deleteBuffer buf

instance (GLES, MonadGeometry m, EmbedIO m, GVertex g) =>
        Resource (Geometry g) LoadedGeometry m where
        loadResource = loadGeometry
        unloadResource _ = gl . deleteGeometry

loadGeometry :: (GLES, MonadGeometry m, GVertex g)
             => Geometry g
             -> m (Either String LoadedGeometry)
loadGeometry geometry@(Geometry _ _ _ _ _) = runExceptT $
        do vao <- lift $ gl createVertexArray
           lift . gl $ bindVertexArray vao

           ExceptT . setAttrTop (0 :: GLUInt) $ top geometry
           LoadedBuffer eb <- ExceptT . getElementBuffer $ elements geometry

           lift . gl $ do bindBuffer gl_ELEMENT_ARRAY_BUFFER eb
                          bindVertexArray noVAO
                          bindBuffer gl_ELEMENT_ARRAY_BUFFER noBuffer
                          bindBuffer gl_ARRAY_BUFFER noBuffer

           return $ LoadedGeometry (elementCount $ elements geometry) vao
        where elementCount (Triangles _ ts) = 3 * length ts

setAttrTop :: (GLES, MonadGeometry m, Attributes is)
           => GLUInt
           -> AttrCol is
           -> m (Either String ())
setAttrTop i0 col0 = runExceptT . (>> return ()) $
        foldTop (\geti col@(AttrTop _ _ _) ->
                        do i <- geti
                           LoadedAttribute sz as <- ExceptT $ getAttribute col
                           lift . gl $
                                mapM_ (\(buf, set) ->
                                        do bindBuffer gl_ARRAY_BUFFER buf
                                           enableVertexAttribArray i
                                           set i
                                      ) as
                           return $ i + sz
                ) (return i0) col0

deleteGeometry :: GLES => LoadedGeometry -> GL ()
deleteGeometry (LoadedGeometry _ vao) = deleteVertexArray vao

loadBuffer :: GLES => GLEnum -> AnyArray -> GL Buffer
loadBuffer ty bufData =
        do buffer <- createBuffer
           bindBuffer ty buffer
           bufferData ty bufData gl_STATIC_DRAW
           bindBuffer ty noBuffer
           return buffer

drawGeometry :: (MonadGeometry m, GVertex g) => Geometry g -> m ()
drawGeometry g = getGeometry g >>= \eg ->
        case eg of
             Left _ -> return ()
             Right (LoadedGeometry ec vao) ->
                     gl $ do bindVertexArray vao
                             drawElements gl_TRIANGLES
                                          (fromIntegral ec)
                                          gl_UNSIGNED_SHORT
                                          nullGLPtr
                             bindVertexArray noVAO
