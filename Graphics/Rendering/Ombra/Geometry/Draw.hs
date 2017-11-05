{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, FlexibleContexts,
             FlexibleInstances, ConstraintKinds, ScopedTypeVariables,
             MultiParamTypeClasses, GADTs #-}

module Graphics.Rendering.Ombra.Geometry.Draw (
        MonadGeometry(..),
        LoadedBuffer,
        LoadedAttribute,
        LoadedGeometry(..),
        defaultDrawGeometry
) where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Proxy

import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Language.Types (ShaderType(size))

class (GLES, Monad m) => MonadGeometry m where
        getAttribute :: BaseAttribute i
                     => AttrCol (i ': is)
                     -> m (Either String LoadedAttribute)
        getElementBuffer :: Elements is -> m (Either String LoadedBuffer)
        getGeometry :: GeometryVertex g
                    => Geometry g
                    -> m (Either String LoadedGeometry)

data LoadedGeometry = LoadedGeometry {
        -- elementType :: GLEnum,
        elementCount :: Int,
        vao :: VertexArrayObject
}

newtype LoadedBuffer = LoadedBuffer Buffer

data LoadedAttribute = LoadedAttribute GLUInt [(Buffer, GLUInt -> GL ())]

instance (GLES, BaseAttribute i) =>
        Resource (AttrCol (i ': is)) LoadedAttribute GL where
        loadResource (AttrTop _ _ down :: AttrCol (i ': is)) =
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

instance (MonadGeometry m, MonadGL m, MonadBaseControl IO m, GeometryVertex g) =>
        Resource (Geometry g) LoadedGeometry m where
        loadResource = loadGeometry
        unloadResource _ = gl . deleteGeometry

downList :: NotTop p => AttrTable p (i ': is) -> [CPUBase i] -> [CPUBase i]
downList AttrEnd xs = xs
downList (AttrCell x _ down) xs = downList down $ x : xs

loadGeometry :: (MonadGeometry m, MonadGL m, GeometryVertex g)
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

setAttrTop :: (GLES, MonadGeometry m, MonadGL m, Attributes is)
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

defaultDrawGeometry :: (MonadGeometry m, MonadGL m, GeometryVertex g)
                    => Geometry g
                    -> m ()
defaultDrawGeometry g = getGeometry g >>= \eg ->
        case eg of
             Left _ -> return ()
             Right (LoadedGeometry ec vao) ->
                     gl $ do bindVertexArray vao
                             drawElements gl_TRIANGLES
                                          (fromIntegral ec)
                                          gl_UNSIGNED_SHORT
                                          nullGLPtr
                             bindVertexArray noVAO
