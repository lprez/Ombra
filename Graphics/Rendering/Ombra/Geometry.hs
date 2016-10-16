{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
             PolyKinds #-}

module Graphics.Rendering.Ombra.Geometry (
        AttrList(..),
        Geometry(..),
        Geometry2D,
        Geometry3D,
        GPUBufferGeometry(..),
        GPUVAOGeometry(..),
        extend,
        remove,
        positionOnly,
        withGPUBufferGeometry,
        mkGeometry,
        mkGeometry2D,
        mkGeometry3D,
        castGeometry
) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as H
import Data.Typeable
import qualified Data.Vector.Storable as V
import Data.Vect.Float hiding (Normal3)
import Data.Word (Word16)
import Unsafe.Coerce

import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Default2D (Position2)
import Graphics.Rendering.Ombra.Shader.Default3D (Position3, Normal3)
import qualified Graphics.Rendering.Ombra.Shader.Default2D as D2
import qualified Graphics.Rendering.Ombra.Shader.Default3D as D3
import Graphics.Rendering.Ombra.Shader.Language.Types (ShaderType(size))
import Graphics.Rendering.Ombra.Transformation

-- | A heterogeneous list of attributes.
data AttrList (is :: [*]) where
        AttrListNil :: AttrList '[]
        AttrListCons :: (H.Hashable (CPU S i), Attribute S i)
                     => (a -> i)
                     -> [CPU S i]
                     -> AttrList is
                     -> AttrList (i ': is)

-- | A set of attributes and indices.
data Geometry (is :: [*]) = Geometry (AttrList is) [Word16] Int

data GPUBufferGeometry = GPUBufferGeometry {
        attributeBuffers :: [(Buffer, GLUInt, GLUInt -> GL ())],
        elementBuffer :: Buffer,
        elementCount :: Int,
        geometryHash :: Int
}

data GPUVAOGeometry = GPUVAOGeometry {
        vaoBoundBuffers :: [Buffer],
        vaoElementCount :: Int,
        vao :: VertexArrayObject
}

-- | A 3D geometry.
type Geometry3D = '[Position3, D3.UV, Normal3]

-- | A 2D geometry.
type Geometry2D = '[Position2, D2.UV]

instance H.Hashable (AttrList is) where
        hashWithSalt salt AttrListNil = salt
        hashWithSalt salt (AttrListCons _ i is) = H.hashWithSalt salt (i, is)

instance H.Hashable (Geometry is) where
        hashWithSalt salt (Geometry _ _ h) = H.hashWithSalt salt h

instance Eq (Geometry is) where
        (Geometry _ _ h) == (Geometry _ _ h') = h == h'

instance H.Hashable GPUBufferGeometry where
        hashWithSalt salt = H.hashWithSalt salt . geometryHash

instance Eq GPUBufferGeometry where
        g == g' = geometryHash g == geometryHash g'

-- | Create a 3D 'Geometry'. The first three lists should have the same length.
mkGeometry3D :: GLES
            => [Vec3]   -- ^ List of vertices.
            -> [Vec2]   -- ^ List of UV coordinates.
            -> [Vec3]   -- ^ List of normals.
            -> [Word16] -- ^ Triangles expressed as triples of indices to the
                        --   three lists above.
            -> Geometry Geometry3D
mkGeometry3D v u n = mkGeometry (AttrListCons D3.Position3 v $
                                AttrListCons D3.UV u $
                                AttrListCons D3.Normal3 n
                                AttrListNil)

-- | Create a 2D 'Geometry'. The first two lists should have the same length.
mkGeometry2D :: GLES
            => [Vec2]     -- ^ List of vertices.
            -> [Vec2]     -- ^ List of UV coordinates.
            -> [Word16] -- ^ Triangles expressed as triples of indices to the
                        --   two lists above.
            -> Geometry Geometry2D
mkGeometry2D v u = mkGeometry (AttrListCons D2.Position2 v $
                               AttrListCons D2.UV u
                               AttrListNil)


-- | Add an attribute to a geometry.
extend :: (Attribute 'S i, H.Hashable (CPU 'S i), ShaderType i, GLES)
       => (a -> i)              -- ^ Attribute constructor (or any other
                                -- function with that type).
       -> [CPU 'S i]            -- ^ List of values
       -> Geometry is
       -> Geometry (i ': is)
extend g c (Geometry al es _) = mkGeometry (AttrListCons g c al) es

-- | Remove an attribute from a geometry.
remove :: (RemoveAttr i is is', GLES)
       => (a -> i)      -- ^ Attribute constructor (or any other function with
                        -- that type).
       -> Geometry is -> Geometry is'
remove g (Geometry al es _) = mkGeometry (removeAttr g al) es

-- Remove the 'UV' and 'Normal3' attributes from a 3D Geometry.
positionOnly :: Geometry Geometry3D -> Geometry '[Position3]
positionOnly (Geometry (AttrListCons pg pc _) es h) =
        Geometry (AttrListCons pg pc AttrListNil) es h

class RemoveAttr i is is' where
        removeAttr :: (a -> i) -> AttrList is -> AttrList is'

instance RemoveAttr i (i ': is) is where
        removeAttr _ (AttrListCons _ _ al) = al

instance RemoveAttr i is is' =>
         RemoveAttr i (i1 ': is) (i1 ': is') where
        removeAttr g (AttrListCons g' c al) =
                AttrListCons g' c $ removeAttr g al

-- | Create a custom 'Geometry'.
mkGeometry :: GLES => AttrList is -> [Word16] -> Geometry is
mkGeometry al e = Geometry al e $ H.hash (al, e)

castGeometry :: Geometry is -> Geometry is'
castGeometry = unsafeCoerce

instance GLES => Resource (Geometry i) GPUBufferGeometry GL where
        -- TODO: err check
        loadResource i = Right <$> loadGeometry i
        unloadResource _ = deleteGPUBufferGeometry

instance GLES => Resource GPUBufferGeometry GPUVAOGeometry GL where
        -- TODO: err check
        loadResource i = Right <$> loadGPUVAOGeometry i
        unloadResource _ = deleteGPUVAOGeometry

loadGPUVAOGeometry :: GLES
                   => GPUBufferGeometry
                   -> GL GPUVAOGeometry
loadGPUVAOGeometry g =
        do vao <- createVertexArray
           bindVertexArray vao
           (ec, bufs) <- withGPUBufferGeometry g $
                   \ec bufs -> bindVertexArray noVAO >> return (ec, bufs)
           return $ GPUVAOGeometry bufs ec vao

loadGeometry :: GLES => Geometry i -> GL GPUBufferGeometry
loadGeometry (Geometry al es h) =
        GPUBufferGeometry <$> loadAttrList al
                          <*> (liftIO (encodeUShorts es) >>=
                                  loadBuffer gl_ELEMENT_ARRAY_BUFFER .
                                  fromUInt16Array)
                          <*> pure (length es)
                          <*> pure h

loadAttrList :: GLES => AttrList is -> GL [(Buffer, GLUInt, GLUInt -> GL ())]
loadAttrList = loadFrom 0
        where loadFrom :: GLUInt -> AttrList is
                       -> GL [(Buffer, GLUInt, GLUInt -> GL ())]
              loadFrom _ AttrListNil = return []
              loadFrom idx (AttrListCons g c al) =
                      do (newIdx, attrInfo) <- loadAttribute idx (g undefined) c
                         (attrInfo ++) <$> loadFrom newIdx al
         
              loadAttribute :: Attribute 'S g => GLUInt -> g -> [CPU 'S g]
                            -> GL (GLUInt, [(Buffer, GLUInt, GLUInt -> GL ())])
              loadAttribute ii g c = flip execStateT (ii, []) $
                withAttributes (Proxy :: Proxy 'S) g c $ \_ (g :: Proxy g) c ->
                        do (i, infos) <- get
                           arr <- lift $ encodeAttribute g c
                           buf <- lift $ loadBuffer gl_ARRAY_BUFFER arr
                           put ( i + fromIntegral (size (undefined :: g))
                               , (buf, i, setAttribute g) : infos )

withGPUBufferGeometry :: GLES
                      => GPUBufferGeometry -> (Int -> [Buffer] -> GL a) -> GL a
withGPUBufferGeometry (GPUBufferGeometry abs eb ec _) f =
        do bindBuffer gl_ARRAY_BUFFER noBuffer
           (locs, bufs) <- unzip <$>
                   mapM (\(buf, loc, setAttr) ->
                                     do bindBuffer gl_ARRAY_BUFFER buf
                                        enableVertexAttribArray loc
                                        setAttr loc
                                        return (loc, buf)
                        ) abs

           bindBuffer gl_ELEMENT_ARRAY_BUFFER eb
           r <- f ec $ eb : bufs
           bindBuffer gl_ELEMENT_ARRAY_BUFFER noBuffer
           bindBuffer gl_ARRAY_BUFFER noBuffer
           -- mapM_ (disableVertexAttribArray . fromIntegral) locs
           return r

deleteGPUVAOGeometry :: GLES => GPUVAOGeometry -> GL ()
deleteGPUVAOGeometry (GPUVAOGeometry bufs _ vao) =
        do mapM_ deleteBuffer bufs
           deleteVertexArray vao


deleteGPUBufferGeometry :: GLES => GPUBufferGeometry -> GL ()
deleteGPUBufferGeometry (GPUBufferGeometry abs eb _ _) =
        mapM_ (\(buf, _, _) -> deleteBuffer buf) abs >> deleteBuffer eb

-- TODO: move
loadBuffer :: GLES => GLEnum -> AnyArray -> GL Buffer
loadBuffer ty bufData =
        do buffer <- createBuffer
           bindBuffer ty buffer
           bufferData ty bufData gl_STATIC_DRAW
           bindBuffer ty noBuffer
           return buffer

instance H.Hashable Vec2 where
        hashWithSalt s (Vec2 x y) = H.hashWithSalt s (x, y)

instance H.Hashable Vec3 where
        hashWithSalt s (Vec3 x y z) = H.hashWithSalt s (x, y, z)

instance H.Hashable Vec4 where
        hashWithSalt s (Vec4 x y z w) = H.hashWithSalt s (x, y, z, w)
