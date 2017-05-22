{-# LANGUAGE GADTs, RankNTypes, DataKinds, KindSignatures, FlexibleInstances #-}

module Graphics.Rendering.Ombra.Layer.Internal where

import Data.Functor ((<$>))
import Data.Word (Word8, Word16)
import Control.Monad (when, replicateM)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL hiding (Buffer, Texture, clear)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Layer.Types
import Graphics.Rendering.Ombra.Object.Internal
import Graphics.Rendering.Ombra.Object.Types
import Graphics.Rendering.Ombra.Screen
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Texture.Internal
import Graphics.Rendering.Ombra.Texture.Types
import Graphics.Rendering.Ombra.Vector

-- TODO: document buffers.
-- | Layer that clear some buffers. For instance, @clear ['ColorBuffer']@ fills
-- the screen with a black rectangle, without affecting the depth buffer.
clear :: [Buffer] -> Layer' s t ()
clear = Clear

-- | Free the 'TTexture's resources associated with a NonDrawable layer, before
-- drawing it.
drawable :: (forall t. Layer' NonDrawable t a) -> Layer' s t a
drawable = Free

-- | Create a 'TTexture'.
newTTexture :: Int                      -- ^ Width.
            -> Int                      -- ^ Height.
            -> Filter                   -- ^ Minification filter.
            -> Filter                   -- ^ Magnification filter.
            -> TTextureType bufs        -- ^ Texture type.
            -> Layer' NonDrawable t (TTexture bufs t)
newTTexture = NewTTexture

-- | Make the type of a drawable 'Layer'' more generic.
castLayer :: Layer' Drawable t a -> Layer' s t' a
castLayer = Cast

-- | Make a 'TTexture' permanent. Its lifetime is still bound to the 'Texture'
-- returned.
permanent :: TTexture bufs t -> Layer' NonDrawable t Texture
permanent = Permanent

-- | Draw a 'Layer' parameterized by a 'TTexture'. You can use this to pass
-- TTextures as uniforms.
withTTexture :: TTexture bufs t -> (Texture -> Layer) -> Layer' NonDrawable t ()
withTTexture pt f = withTTextures [pt] $ \[t] -> f t

-- | Draw a 'Layer' parameterized by a list of 'TTexture's.
withTTextures :: [TTexture bufs t]
              -> ([Texture] -> Layer)
              -> Layer' NonDrawable t ()
withTTextures pts f = WithTTextures pts $ \ts -> castLayer $ f ts

-- | Draw a 'Layer'' parameterized by a list of 'TTexture's.
withTTextures' :: [TTexture bufs t]
               -> ([Texture] -> Layer' s t ())
               -> Layer' NonDrawable t ()
withTTextures' = WithTTextures

castTTexture :: TTexture bufs t -> TTexture bufs t'
castTTexture (TTexture ty lt) = TTexture ty lt

-- | Draw a 'Layer' to a depth 'Texture'.
depthToTexture :: Int           -- ^ Textures width.
               -> Int           -- ^ Textures height.
               -> Layer' s t a  -- ^ Layer to draw.
               -> Layer' NonDrawable t (a, TTexture '[DepthBuffer] t)
depthToTexture w h l =
        do dt <- newTTexture w h Nearest Nearest ShortTTexture
           x <- layerToTexture w h [] (Left dt) $ clear [DepthBuffer] >> l
           return (x, dt)

-- | Draw a 'Layer' to a color 'Texture' and a depth 'Texture'.
colorDepthToTexture :: Int              -- ^ Textures width.
                    -> Int              -- ^ Textures height.
                    -> Layer' s t a     -- ^ Layer to draw.
                    -> Layer' NonDrawable t ( a
                                            , TTexture '[ColorBuffer] t
                                            , TTexture '[DepthBuffer] t
                                            )
colorDepthToTexture w h l =
        do ct <- newTTexture w h Nearest Nearest RGBAByteTTexture
           dt <- newTTexture w h Nearest Nearest ShortTTexture
           x <- layerToTexture w h [ct] (Left dt) $
                        clear [ColorBuffer, DepthBuffer] >> l
           return (x, ct, dt)

-- | Draw a 'Layer' to a color 'Texture' with an additional stencil buffer.
colorStencilToTexture :: Int -- ^ Texture width.
                      -> Int -- ^ Texture height.
                      -> Layer' s t a
                      -> Layer' NonDrawable t ( a
                                              , TTexture '[ColorBuffer] t
                                              , TTexture '[ DepthBuffer
                                                          , StencilBuffer
                                                          ] t
                                              )
colorStencilToTexture w h l =
        do ct <- newTTexture w h Nearest Nearest RGBAByteTTexture
           dt <- newTTexture w h Nearest Nearest IntTTexture
           x <- layerToTexture w h [ct] (Right dt) $
                        clear [ColorBuffer, DepthBuffer, StencilBuffer] >> l
           return (x, ct, dt)

-- | Draw a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) to some 'Texture's and to a depth
-- Texture.
buffersDepthToTexture :: Int            -- ^ Texture width.
                      -> Int            -- ^ Texture height.
                      -> Int            -- ^ Number of colors.
                      -> Layer' s t a   -- ^ Layer to draw.
                      -> Layer' NonDrawable t ( a
                                              , [TTexture '[ColorBuffer] t]
                                              , TTexture '[DepthBuffer] t
                                              )
buffersDepthToTexture w h n l =
        do cts <- replicateM n $
                        newTTexture w h Nearest Nearest RGBAFloatTTexture
           dt <- newTTexture w h Nearest Nearest ShortTTexture
           x <- layerToTexture w h cts (Left dt) $
                        clear [ColorBuffer, DepthBuffer] >> l
           return (x, cts, dt)

-- | Draw a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) to some 'Texture's with an additional
-- stencil buffer.
buffersStencilToTexture :: Int          -- ^ Texture width.
                        -> Int          -- ^ Texture height.
                        -> Int          -- ^ Number of colors.
                        -> Layer' s t a -- ^ Layer to draw.
                        -> Layer' NonDrawable t ( a
                                                , [TTexture '[ColorBuffer] t]
                                                , TTexture '[ DepthBuffer
                                                            , StencilBuffer
                                                            ] t
                                                )
buffersStencilToTexture w h n l =
        do cts <- replicateM n $
                        newTTexture w h Nearest Nearest RGBAFloatTTexture
           dt <- newTTexture w h Nearest Nearest IntTTexture
           x <- layerToTexture w h cts (Right dt) $
                        clear [ColorBuffer, DepthBuffer, StencilBuffer] >> l
           return (x, cts, dt)

-- | Draw a 'Layer' to some textures. This is the most generic variant of the
-- *ToTexture functions, and you have to create the 'TTexture's manually with
-- 'newTTexture' to use it (or reuse the ones created from functions like
-- 'depthToTexture').
--
-- __IMPORTANT:__ The layer must clear all the buffers it uses before
-- drawing on them, but that's not necessary for the ones that have TTextures
-- attached that were already drawn to with other *ToTexture invocations.
layerToTexture :: Int                           -- ^ Viewport width.
               -> Int                           -- ^ Viewport height.
               -> [TTexture '[ColorBuffer] t] -- ^ Color attachments.
               -> Either (TTexture '[DepthBuffer] t)
                         (TTexture '[DepthBuffer, StencilBuffer] t)
                                                -- ^ Depth/stencil attachment.
               -> Layer' s t a                  -- ^ Layer to draw.
               -> Layer' NonDrawable t a
layerToTexture = TextureLayer

-- | Read a rectangle of pixel colors from the screen (or texture).
readColor :: (Int, Int, Int, Int) -> Layer' s t [Color]
readColor = ReadColor

-- | 'readColor' variant that read color vectors.
readColorFloat :: (Int, Int, Int, Int) -> Layer' s t [Vec4]
readColorFloat = ReadColorFloat

-- | Read a rectangle of pixel depths from the screen (or texture).
-- Not supported on WebGL!
readDepth :: (Int, Int, Int, Int) -> Layer' s t [Word16]
readDepth = ReadDepth

-- | 'readDepth' variants that read floats. Not supported on WebGL as well.
readDepthFloat :: (Int, Int, Int, Int) -> Layer' s t [Float]
readDepthFloat = ReadDepthFloat

-- | Read a rectangle of stencil values from the screen (or texture).
-- Not supported on WebGL!
readStencil :: (Int, Int, Int, Int) -> Layer' s t [Word8]
readStencil = ReadStencil

clearBuffers :: (GLES, MonadGL m) => [Buffer] -> m ()
clearBuffers = mapM_ $ gl . GL.clear . buffer
        where buffer ColorBuffer = gl_COLOR_BUFFER_BIT
              buffer DepthBuffer = gl_DEPTH_BUFFER_BIT
              buffer StencilBuffer = gl_STENCIL_BUFFER_BIT

-- | Draw a 'Layer'.
drawLayer :: MonadObject m => Layer' Drawable t a -> m a
drawLayer = fmap fst . flip drawLayer' []

drawLayer' :: MonadObject m
           => Layer' s t a
           -> [LoadedTexture]
           -> m (a, [LoadedTexture])
drawLayer' (Layer prg grp) ts = do setProgram prg
                                   drawObject grp
                                   return ((), ts)
drawLayer' (NewTTexture w h min mag ty) tts =
        (\tt@(TTexture _ l) -> (tt, l : tts)) <$> createTTexture w h min mag ty
drawLayer' (Permanent (TTexture _ lt)) tts = 
        do let t = TextureLoaded lt
           gl $ unloader t (Nothing :: Maybe TextureImage) lt
           return (t, filter (/= lt) tts)
drawLayer' (WithTTextures ets f) tts = fmap (const ((), tts)) $
           drawLayer' (f $ map (\(TTexture _ lt) -> TextureLoaded lt) ets) tts
drawLayer' (TextureLayer w h colAtts edsAtt layer) tts =
        drawLayerToTexture w h colAtts edsAtt layer tts
drawLayer' (ReadColor r) tts = flip (,) tts <$> readPixels r gl_RGBA
drawLayer' (ReadColorFloat r) tts = flip (,) tts <$> readPixels r gl_RGBA
drawLayer' (ReadDepth r) tts = flip (,) tts <$> readPixels r gl_DEPTH_COMPONENT
drawLayer' (ReadDepthFloat r) tts =
        flip (,) tts <$> readPixels r gl_DEPTH_COMPONENT
drawLayer' (ReadStencil r) tts = flip (,) tts <$> readPixels r gl_STENCIL_INDEX
drawLayer' (Free layer) tts =
        do (x, tts') <- drawLayer' layer []
           mapM_ unusedTexture tts'
           return (x, tts)
drawLayer' (Clear bufs) tts = const ((), tts) <$> clearBuffers bufs
drawLayer' (Cast layer) tts = drawLayer' layer tts
drawLayer' (Bind lx f) tts0 = drawLayer' lx tts0 >>=
                                \(x, tts1) -> drawLayer' (f x) tts1
drawLayer' (Return x) tts = return (x, tts)

createTTexture :: (GLES, MonadTexture m)
               => Int
               -> Int
               -> Filter
               -> Filter
               -> TTextureType bufs
               -> m (TTexture bufs t)
createTTexture w h min mag ty = 
        do lt@(LoadedTexture _ _ t) <- newTexture w h (min, Nothing) mag
           gl $ bindTexture gl_TEXTURE_2D t
           if pixelType == gl_FLOAT
           then liftIO noFloat32Array >>=
                        gl . texImage2DFloat gl_TEXTURE_2D 0
                                             internalFormat w' h'
                                             0 format pixelType
           else liftIO noUInt8Array >>=
                        gl . texImage2DUInt gl_TEXTURE_2D 0
                                            internalFormat w' h'
                                            0 format pixelType
           gl $ bindTexture gl_TEXTURE_2D noTexture
           return $ TTexture ty lt
        where (w', h') = (fromIntegral w, fromIntegral h)
              (internalFormat, format, pixelType) = case ty of
                        RGBAByteTTexture ->  ( fromIntegral gl_RGBA
                                             , gl_RGBA
                                             , gl_UNSIGNED_BYTE
                                             )
                        RGBAFloatTTexture -> ( fromIntegral gl_RGBA32F
                                             , gl_RGBA
                                             , gl_FLOAT
                                             )
                        ShortTTexture ->     ( fromIntegral gl_DEPTH_COMPONENT
                                             , gl_DEPTH_COMPONENT
                                             , gl_UNSIGNED_SHORT
                                             )
                        IntTTexture ->       ( fromIntegral gl_DEPTH_STENCIL
                                             , gl_DEPTH_STENCIL
                                             , gl_UNSIGNED_INT_24_8
                                             )


drawLayerToTexture :: (GLES, MonadObject m)
                   => Int
                   -> Int
                   -> [TTexture '[ColorBuffer] t]
                   -> Either (TTexture '[DepthBuffer] t)
                             (TTexture '[DepthBuffer, StencilBuffer] t)
                   -> Layer' s t a
                   -> [LoadedTexture]
                   -> m (a, [LoadedTexture])
drawLayerToTexture w h colorAtts edsAtt layer tts =
        drawToTexture drawBufs (dsAtt' : colorAtts') w h $ drawLayer' layer tts

        where drawBufs | (_ : _ : _) <- colorAtts = True
                       | otherwise = False

              colorAtts' = zipWith attachment colorAtts [0 ..]
              dsAtt' = case edsAtt of
                            Left att -> attachment att 0
                            Right att -> attachment att 0

              attachment (TTexture ty (LoadedTexture _ _ t)) n =
                      (t, attachmentType ty $ fromIntegral n)

              attachmentType :: TTextureType bufs -> GLEnum -> GLEnum
              attachmentType RGBAFloatTTexture n = gl_COLOR_ATTACHMENT0 + n
              attachmentType RGBAByteTTexture n = gl_COLOR_ATTACHMENT0 + n
              attachmentType ShortTTexture _ = gl_DEPTH_ATTACHMENT
              attachmentType IntTTexture _ = gl_DEPTH_STENCIL_ATTACHMENT

drawToTexture :: (GLES, MonadObject m)
              => Bool
              -> [(GL.Texture, GLEnum)]
              -> Int
              -> Int
              -> m a
              -> m a
drawToTexture useDrawBuffers atts w h act =
        do fb <- gl createFramebuffer 
           -- TODO: nested framebuffers
           gl $ bindFramebuffer gl_FRAMEBUFFER fb

           buffersToDraw <- fmap concat . flip mapM atts $
                   \(t, attach) ->
                           do let drawAttachment =
                                   [ fromIntegral attach
                                   | attach /= gl_DEPTH_ATTACHMENT
                                   , attach /= gl_DEPTH_STENCIL_ATTACHMENT
                                   ]
                                        
                              gl $ framebufferTexture2D gl_FRAMEBUFFER attach
                                                        gl_TEXTURE_2D t 0
                              return drawAttachment

           when useDrawBuffers $
                   liftIO (encodeInts buffersToDraw) >>= gl . drawBuffers

           (sw, sh) <- currentViewport
           resizeViewport (fromIntegral w) (fromIntegral h)

           ret <- act

           resizeViewport sw sh
           gl $ do deleteFramebuffer fb
                   bindFramebuffer gl_FRAMEBUFFER noFramebuffer

           return ret

class ReadPixels r where
        readPixels :: MonadGL m => (Int, Int, Int, Int) -> GLEnum -> m r

instance GLES => ReadPixels [Color] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newUInt8Array $
                                        fromIntegral rw * fromIntegral rh * 4
                           gl $ readPixelsUInt8 (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_UNSIGNED_BYTE arr
                           liftIO $ fmap wordsToColors (decodeUInt8s arr)
                where wordsToColors (r : g : b : a : xs) =
                                Color r g b a : wordsToColors xs
                      wordsToColors _ = []

instance GLES => ReadPixels [Vec4] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newFloat32Array $
                                        fromIntegral rw * fromIntegral rh * 4
                           gl $ readPixelsFloat (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_FLOAT arr
                           liftIO $ fmap floatsToVecs (decodeFloat32s arr)
                where floatsToVecs (r : g : b : a : xs) =
                                Vec4 r g b a : floatsToVecs xs
                      floatsToVecs _ = []

instance GLES => ReadPixels [Word8] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newUInt8Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsUInt8 (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_UNSIGNED_BYTE arr
                           liftIO $ decodeUInt8s arr

instance GLES => ReadPixels [Word16] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newUInt16Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsUInt16 (fromIntegral x)
                                                 (fromIntegral y)
                                                 (fromIntegral rw)
                                                 (fromIntegral rh)
                                                 format gl_UNSIGNED_SHORT arr
                           liftIO $ decodeUInt16s arr

instance GLES => ReadPixels [Float] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newFloat32Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsFloat (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_FLOAT arr
                           liftIO $ decodeFloat32s arr
