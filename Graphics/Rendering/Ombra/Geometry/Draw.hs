{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             FlexibleInstances, ConstraintKinds, ScopedTypeVariables,
             MultiParamTypeClasses, UndecidableInstances, GADTs #-}

module Graphics.Rendering.Ombra.Geometry.Draw (
        LoadedBuffer,
        LoadedAttribute,
        LoadedGeometry(..),
        AttributeData,
        drawGeometry,
        setVAO
) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Hashable (Hashable, hashWithSalt)
import Data.Foldable (toList)
import Data.Proxy
import Data.Type.Equality

import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language.Types (ShaderType(size))

data AttributeData = forall i is. BaseAttribute i =>
                        AttributeData (AttrCol (i ': is))

instance Eq AttributeData where
        AttributeData (AttrTop h _ _) == AttributeData (AttrTop h' _ _) =
                h == h'

instance Hashable AttributeData where
        hashWithSalt s (AttributeData a) = hashWithSalt s a

data LoadedGeometry = LoadedGeometry {
        elementCount :: Int,
        vao :: VertexArrayObject
}

newtype LoadedBuffer = LoadedBuffer Buffer

data LoadedAttribute = LoadedAttribute GLUInt [(Buffer, GLUInt -> GL ())]

instance (GLES, MonadGL m) => MonadLoad AttributeData LoadedAttribute m where
        loadResource (AttributeData (AttrTop _ _ down :: AttrCol (i ': is))) =
                gl .
                fmap (Right . uncurry LoadedAttribute) .
                flip execStateT (0, []) $
                        do (i, as) <- get
                           arr <- lift $ encodeAttribute (Proxy :: Proxy i) vs
                           buf <- lift $ loadBuffer gl_ARRAY_BUFFER arr
                           let sz = fromIntegral . size $ (undefined :: i)
                               set = setBaseAttribute (Proxy :: Proxy i) . (+ i)
                           put (i + sz, (buf, set) : as)
                where vs = downList down []
        unloadResource _ (LoadedAttribute _ as) =
                gl $ mapM_ (\(buf, _) -> deleteBuffer buf) as

instance (GLES, ElementType e, MonadGL m) =>
        MonadLoad (Elements e is) LoadedBuffer m where
        loadResource (Elements _ es) =
                liftBase (encodeUInt16s $ es >>= map idx . toList) >>=
                        fmap (Right . LoadedBuffer) .  gl .
                        loadBuffer gl_ELEMENT_ARRAY_BUFFER .
                        fromUInt16Array
                where idx (AttrVertex i _) = i
        unloadResource _ (LoadedBuffer buf) = gl $ deleteBuffer buf

instance ( MonadResource (Elements e (AttributeTypes g)) LoadedBuffer m
         , MonadResource AttributeData LoadedAttribute m
         , MonadGL m
         , MonadBaseControl IO m
         , GeometryVertex g
         , ElementType e
         , GLES
         ) =>
        MonadLoad (Geometry e g) LoadedGeometry m where
        loadResource = loadGeometry
        unloadResource _ = gl . deleteGeometry

downList :: NotTop p => AttrTable p (i ': is) -> [CPUBase i] -> [CPUBase i]
downList AttrEnd xs = xs
downList (AttrCell x _ down) xs = downList down $ x : xs

loadGeometry :: ( MonadResource (Elements e (AttributeTypes g)) LoadedBuffer m
                , MonadResource AttributeData LoadedAttribute m
                , MonadGL m
                , GeometryVertex g
                , ElementType e
                , GLES
                )
             => Geometry e g
             -> m (Either String LoadedGeometry)
loadGeometry geometry@(Geometry _ _ _ _ _) = runExceptT $
        do vao <- lift $ gl createVertexArray
           lift . gl $ bindVertexArray vao

           ExceptT . setAttrTop (0 :: GLUInt) $ top geometry
           LoadedBuffer eb <- ExceptT . getResource $ elements geometry

           lift . gl $ do bindBuffer gl_ELEMENT_ARRAY_BUFFER eb
                          bindVertexArray noVAO
                          bindBuffer gl_ELEMENT_ARRAY_BUFFER noBuffer
                          bindBuffer gl_ARRAY_BUFFER noBuffer

           return $ LoadedGeometry (elementCount $ elements geometry) vao
        where elementCount (Elements _ (t : ts)) = length t * (1 + length ts)
              elementCount _ = 0

setAttrTop :: ( GLES
              , MonadResource AttributeData LoadedAttribute m
              , MonadGL m
              , Attributes is
              )
           => GLUInt
           -> AttrCol is
           -> m (Either String ())
setAttrTop i0 col0 = runExceptT . (>> return ()) $
        foldTop (\geti col@(AttrTop _ _ _) ->
                        do i <- geti
                           LoadedAttribute sz as <-
                                ExceptT $ getResource' (Just col)
                                                       (AttributeData col)
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

setVAO :: forall e m g.
          ( MonadResource (Elements e (AttributeTypes g))
                          LoadedBuffer m
          , MonadResource AttributeData LoadedAttribute m
          , MonadResource (Geometry e g) LoadedGeometry m
          , MonadGL m
          , GeometryVertex g
          , ElementType e
          , GLES
          )
       => Maybe (Geometry e g)
       -> m ()
setVAO Nothing = gl $ bindVertexArray noVAO
setVAO (Just g) = getResource g >>= \eg ->
        case eg of
             Left _ -> return ()
             Right (LoadedGeometry ec vao) -> gl $ bindVertexArray vao

drawGeometry :: forall e m g.
                ( MonadResource (Elements e (AttributeTypes g))
                                LoadedBuffer m
                , MonadResource AttributeData LoadedAttribute m
                , MonadResource (Geometry e g) LoadedGeometry m
                , MonadGL m
                , GeometryVertex g
                , ElementType e
                , GLES
                )
             => Bool
             -> Geometry e g
             -> m ()
drawGeometry changed g = getResource g >>= \eg ->
        case eg of
             Left _ -> return ()
             Right (LoadedGeometry ec vao) ->
                     gl $ do when changed $ bindVertexArray vao
                             drawElements (elementType (Proxy :: Proxy e))
                                          (fromIntegral ec)
                                          gl_UNSIGNED_SHORT
                                          nullGLPtr
