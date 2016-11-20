{-# LANGUAGE GADTs, TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
             PolyKinds, UndecidableInstances, RankNTypes #-}

module Graphics.Rendering.Ombra.Geometry.Internal (
        AttrList(..),
        AttrData(..),
        Geometry(..),
        ElemData(..),
        LoadedBuffer,
        LoadedAttribute,
        LoadedGeometry(..),
        Attributes(..),
        geometry,
        removeAttribute,
        loadGeometry,
        deleteGeometry
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Hashable as H
import Data.Typeable
import Data.Vect.Float hiding (Normal3)
import Data.Word (Word16)
import Unsafe.Coerce

import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Default2D (Position2)
import Graphics.Rendering.Ombra.Shader.Default3D (Position3, Normal3)
import qualified Graphics.Rendering.Ombra.Shader.Default2D as D2
import qualified Graphics.Rendering.Ombra.Shader.Default3D as D3
import Graphics.Rendering.Ombra.Shader.Language.Types (ShaderType(size))

-- | A heterogeneous list of attributes.
data AttrList (is :: [*]) where
        AttrListNil :: AttrList '[]
        AttrListCons :: (H.Hashable (CPU S i), Attribute S i)
                     => AttrData i
                     -> AttrList is
                     -> AttrList (i ': is)

data AttrData i = AttrData [CPU S i] Int

-- | A set of attributes and indices.
data Geometry (is :: [*]) = Geometry (AttrList is) ElemData Int

data LoadedGeometry = LoadedGeometry {
        elementCount :: Int,
        vao :: VertexArrayObject
}

newtype LoadedBuffer = LoadedBuffer Buffer

data ElemData = ElemData [Word16] Int

data LoadedAttribute = LoadedAttribute GLUInt [(Buffer, GLUInt -> GL ())]

instance Eq (Geometry is) where
        (Geometry _ _ h) == (Geometry _ _ h') = h == h'

instance H.Hashable (Geometry is) where
        hashWithSalt salt (Geometry _ _ h) = H.hashWithSalt salt h

instance H.Hashable ElemData where
        hashWithSalt salt (ElemData _ h) = H.hashWithSalt salt h

instance Eq ElemData where
        (ElemData _ h) == (ElemData _ h') = h == h'

instance H.Hashable (AttrData i) where
        hashWithSalt salt (AttrData _ h) = H.hashWithSalt salt h

instance Eq (AttrData i) where
        (AttrData _ h) == (AttrData _ h') = h == h'

instance H.Hashable (AttrList is) where
        hashWithSalt salt AttrListNil = salt
        hashWithSalt salt (AttrListCons (AttrData _ h) al) = H.hashWithSalt h al

class Attributes (is :: [*]) where
        emptyAttrList :: Proxy (is :: [*]) -> AttrList is
        
instance Attributes '[] where
        emptyAttrList _ = AttrListNil

instance (H.Hashable (CPU S i), Attribute S i, Attributes is) =>
        Attributes (i ': is) where
        emptyAttrList (_ :: Proxy (i ': is)) =
                AttrListCons (AttrData [] 0 :: AttrData i) $
                        emptyAttrList (Proxy :: Proxy is)

geometry :: AttrList is -> ElemData -> Geometry is
geometry al es = Geometry al es $ H.hashWithSalt (H.hash al) es

-- | Remove an attribute from a geometry.
removeAttribute :: (RemoveAttr i is is', GLES)
                => (a -> i)      -- ^ Attribute constructor (or any other
                                 -- function with that type).
                -> Geometry is -> Geometry is'
removeAttribute g (Geometry al es _) = geometry (removeAttr g al) es

class RemoveAttr i is is' where
        removeAttr :: (a -> i) -> AttrList is -> AttrList is'

instance RemoveAttr i (i ': is) is where
        removeAttr _ (AttrListCons _ al) = al

instance RemoveAttr i is is' =>
         RemoveAttr i (i1 ': is) (i1 ': is') where
        removeAttr g (AttrListCons c al) =
                AttrListCons c $ removeAttr g al

instance (GLES, Attribute 'S i) => Resource (AttrData i) LoadedAttribute GL where
        loadResource (AttrData vs _ :: AttrData i) =
                fmap (Right . uncurry LoadedAttribute) .
                flip execStateT (0, []) $
                        withAttributes (Proxy :: Proxy 'S) (undefined :: i) vs $
                                \_ (g :: Proxy g) c ->
                                        do (i, as) <- get
                                           arr <- lift $ encodeAttribute g c
                                           buf <- lift $
                                                   loadBuffer gl_ARRAY_BUFFER
                                                              arr
                                           let sz = fromIntegral . size $
                                                        (undefined :: g)
                                               set = setAttribute g . (+ i)
                                           put (i + sz, (buf, set) : as)
        unloadResource _ (LoadedAttribute _ as) =
                mapM_ (\(buf, _) -> deleteBuffer buf) as

instance GLES => Resource ElemData LoadedBuffer GL where
        loadResource (ElemData elems _) =
                liftIO (encodeUShorts elems) >>=
                        fmap (Right . LoadedBuffer) .
                        loadBuffer gl_ELEMENT_ARRAY_BUFFER
                        . fromUInt16Array
        unloadResource _ (LoadedBuffer buf) = deleteBuffer buf

loadGeometry :: (GLES, Monad m)
             => (forall i. Attribute 'S i => AttrData i -> m LoadedAttribute)
             -> (ElemData -> m LoadedBuffer)
             -> (forall a. GL a -> m a)
             -> Geometry is
             -> m LoadedGeometry
loadGeometry getAttr getElems gl (Geometry attrList elems@(ElemData es _) _) =
        do vao <- gl createVertexArray
           gl $ bindVertexArray vao

           setAttrList getAttr gl (0 :: GLUInt) attrList
           LoadedBuffer eb <- getElems elems

           gl $ do bindBuffer gl_ELEMENT_ARRAY_BUFFER eb
                   bindVertexArray noVAO
                   bindBuffer gl_ELEMENT_ARRAY_BUFFER noBuffer
                   bindBuffer gl_ARRAY_BUFFER noBuffer

           return $ LoadedGeometry (length es) vao

setAttrList :: (GLES, Monad m)
            => (forall i. Attribute 'S i => AttrData i -> m LoadedAttribute)
            -> (forall a. GL a -> m a)
            -> GLUInt
            -> AttrList is
            -> m ()
setAttrList getAttr gl i (AttrListCons attrData rest) =
        do (LoadedAttribute sz as) <- getAttr attrData
           gl $ mapM_ (\(buf, set) -> do bindBuffer gl_ARRAY_BUFFER buf
                                         enableVertexAttribArray i
                                         set i
                      ) as
           setAttrList getAttr gl (i + sz) rest
setAttrList _ _ _ AttrListNil = return ()

deleteGeometry :: GLES => LoadedGeometry -> GL ()
deleteGeometry (LoadedGeometry _ vao) = deleteVertexArray vao

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
