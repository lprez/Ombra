{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Texture.Internal (
        MonadTexture(..),
        withActiveTexture,
        textureSize,
        emptyTexture
) where

import Control.Monad (when)
import Data.Hashable
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Color
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture)
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Texture.Types

class (MonadGL m, GLES) => MonadTexture m where
        getTexture :: Texture -> m (Either String LoadedTexture)
        getActiveTexturesCount :: m Int
        setActiveTexturesCount :: Int -> m ()
        newTexture :: Int
                   -> Int
                   -> (Filter, Maybe Filter)
                   -> Filter
                   -> Int
                   -> (GL.Texture -> GL ())
                   -> m LoadedTexture
        unusedTextures :: [LoadedTexture] -> m ()

instance GLES => Resource TextureImage LoadedTexture GL where
        loadResource i = Right <$> loadTextureImage i
        unloadResource _ (LoadedTexture _ _ _ t) = deleteTexture t

makeActive :: MonadTexture m => (ActiveTexture -> m a) -> m a
makeActive f = do atn <- getActiveTexturesCount
                  setActiveTexturesCount $ atn + 1
                  gl . activeTexture $ gl_TEXTURE0 + fromIntegral atn
                  ret <- f . ActiveTexture . fromIntegral $ atn
                  setActiveTexturesCount $ atn
                  return ret

withActiveTexture :: MonadTexture m
                  => Texture
                  -> a
                  -> (ActiveTexture -> m a)
                  -> m a
withActiveTexture tex fail f = getTexture tex >>= \etex ->
        case etex of
             Left _ -> return fail
             Right (LoadedTexture _ _ _ wtex) -> makeActive $
                        \at -> do gl $ bindTexture gl_TEXTURE_2D wtex
                                  f at

-- | Get the dimensions of a 'Texture'.
textureSize :: (MonadTexture m, Num a) => Texture -> m (a, a)
textureSize tex = do etex <- getTexture tex
                     case etex of
                          Left _ -> return (0, 0)
                          Right (LoadedTexture w h _ _) ->
                                  return (fromIntegral w, fromIntegral h)

loadTextureImage :: GLES => TextureImage -> GL LoadedTexture
loadTextureImage (TexturePixels g pss min mag w h hash) =
        do arr <- mapM (\ps -> liftIO . encodeUInt8s .
                               take (fromIntegral $ w * h * 4) $
                               ps >>= \(Color r g b a) -> [r, g, b, a]) pss
           loadTextureImage $ TextureRaw g arr min mag w h hash
loadTextureImage (TextureRaw g arrs min mag w h _) =
        do t <- emptyTexture min mag
           mapM_ (\(arr, l) -> texImage2DUInt gl_TEXTURE_2D l
                                              (fromIntegral gl_RGBA)
                                              w h 0
                                              gl_RGBA
                                              gl_UNSIGNED_BYTE
                                              arr
                 )
                 (zip arrs [0 ..])
           when g $ generateMipmap gl_TEXTURE_2D
           return $ LoadedTexture (fromIntegral w)
                                  (fromIntegral h)
                                  0
                                  t
loadTextureImage (TextureFloat ps min mag w h hash) =
        do arr <- liftIO . encodeFloats . take (fromIntegral $ w * h * 4) $ ps
           t <- emptyTexture min mag
           texImage2DFloat gl_TEXTURE_2D 0
                           (fromIntegral gl_RGBA32F)
                           w h 0
                           gl_RGBA
                           gl_FLOAT
                           arr
           return $ LoadedTexture (fromIntegral w)
                                  (fromIntegral h)
                                  0
                                  t

emptyTexture :: GLES => (Filter, Maybe Filter) -> Filter -> GL GL.Texture
emptyTexture minf magf = do t <- createTexture
                            bindTexture gl_TEXTURE_2D t
                            param gl_TEXTURE_MIN_FILTER $ mf minf
                            param gl_TEXTURE_MAG_FILTER $ f magf
                            param gl_TEXTURE_WRAP_S gl_REPEAT
                            param gl_TEXTURE_WRAP_T gl_REPEAT
                            return t
        where f Linear = gl_LINEAR
              f Nearest = gl_NEAREST
              mf (Linear, Nothing) = gl_LINEAR
              mf (Linear, Just Nearest) = gl_LINEAR_MIPMAP_NEAREST
              mf (Linear, Just Linear) = gl_LINEAR_MIPMAP_LINEAR
              mf (Nearest, Nothing) = gl_NEAREST
              mf (Nearest, Just Nearest) = gl_NEAREST_MIPMAP_NEAREST
              mf (Nearest, Just Linear) = gl_NEAREST_MIPMAP_LINEAR

              param :: GLES => GLEnum -> GLEnum -> GL ()
              param p v = texParameteri gl_TEXTURE_2D p $ fromIntegral v
