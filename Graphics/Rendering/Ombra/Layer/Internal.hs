{-# LANGUAGE GADTs, RankNTypes, DataKinds, KindSignatures #-}

module Graphics.Rendering.Ombra.Layer.Internal where

import Data.Word (Word8)
import Control.Monad (when)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL hiding (Buffer, Texture)
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

-- TODO: document buffers.
-- | Layer that clear some buffers. For instance, @clear ['ColorBuffer']@ fills
-- the screen with a black rectangle, without affecting the depth buffer.
clear :: [Buffer] -> Layer' s t ()
clear = Clear

-- | Free the temporary resources associated with a NonDrawable layer, before
-- drawing it.
drawable :: (forall t. Layer' NonDrawable t a) -> Layer' s t a
drawable = Free

castDrawable :: Layer' Drawable t a -> Layer' Drawable t' a
castDrawable = Cast

-- | Make the type of a simple 'Layer' more generic.
castLayer :: Layer -> Layer' Drawable t ()
castLayer = castDrawable

-- | Make a 'TTexture' permanent. Its lifetime is still bound to the 'Texture'
-- returned.
permanent :: TTexture t -> Layer' NonDrawable t Texture
permanent = Permanent

-- | Draw a Layer using a temporary texture.
withTTexture :: TTexture t -> (Texture -> Layer) -> Layer' NonDrawable t ()
withTTexture pt f = WithTTextures [pt] $ \[t] -> f t

-- | Draw a Layer using a list of temporary textures.
withTTextures :: [TTexture t] -> ([Texture] -> Layer) -> Layer' NonDrawable t ()
withTTextures = WithTTextures

castTTexture :: TTexture t -> TTexture t'
castTTexture (TTexture lt) = TTexture lt

-- | Draw a 'Layer' to a depth 'Texture'.
depthToTexture :: Int           -- ^ Textures width.
               -> Int           -- ^ Textures height.
               -> Layer' s t a  -- ^ Layer to draw.
               -> Layer' NonDrawable t (a, TTexture t)
depthToTexture w h l =
        fmap (\(x, [t], _, _) -> (x, t)) $
                TextureLayer False [DepthLayer] (w, h) (0, 0, 0, 0)
                             False False l

-- | Draw a 'Layer' to a color 'Texture' and a depth 'Texture'.
colorDepthToTexture :: Int              -- ^ Textures width.
                    -> Int              -- ^ Textures height.
                    -> Layer' s t a     -- ^ Layer to draw.
                    -> Layer' NonDrawable t (a, TTexture t, TTexture t)
colorDepthToTexture w h l =
        fmap (\(x, [ct, dt], _, _) -> (x, ct, dt)) $
                TextureLayer False [ColorLayer, DepthLayer] (w, h) (0, 0, 0, 0)
                             False False l

-- | Draw a 'Layer' to a color 'Texture' with an additional stencil buffer.
colorStencilToTexture :: Int -- ^ Texture width.
                      -> Int -- ^ Texture height.
                      -> Layer' s t a
                      -> Layer' NonDrawable t (a, TTexture t)
colorStencilToTexture w h l =
        fmap (\(x, [ct, _], _, _) -> (x, ct)) $
                TextureLayer False [ColorLayer, DepthStencilLayer] (w, h)
                             (0, 0, 0, 0) False False l

-- | Draw a 'Layer' to a 'Texture', reading the content of the texture.
colorToTexture' :: Int          -- ^ Texture width.
                -> Int          -- ^ Texture height.
                -> Int          -- ^ First pixel to read X.
                -> Int          -- ^ First pixel to read Y.
                -> Int          -- ^ Width of the rectangle to read.
                -> Int          -- ^ Height of the rectangle to read.
                -> Layer' s t a -- ^ Layer to draw.
                -> Layer' NonDrawable t (a, TTexture t, [Color])
colorToTexture' w h rx ry rw rh l =
        fmap (\(x, [t, _], Just c, _) -> (x, t, c)) $
                TextureLayer False [ColorLayer, DepthLayer] (w, h)
                             (rx, ry, rw, rh) True False l

-- | Draw a 'Layer' to a depth 'Texture', reading the content of the texture.
-- Not supported on WebGL.
depthToTexture' :: Int          -- ^ Texture width.
                -> Int          -- ^ Texture height.
                -> Int          -- ^ First pixel to read X.
                -> Int          -- ^ First pixel to read Y.
                -> Int          -- ^ Width of the rectangle to read.
                -> Int          -- ^ Height of the rectangle to read.
                -> Layer' s t a -- ^ Layer to draw.
                -> Layer' NonDrawable t (a, TTexture t, [Word8])
depthToTexture' w h rx ry rw rh l =
        fmap (\(x, [t], _, Just d) -> (x, t, d)) $
                TextureLayer False [DepthLayer] (w, h) (rx, ry, rw, rh)
                             False True l

-- | Combination of 'colorToTexture'' and 'depthToTexture''. Not supported
-- on WebGL.
colorDepthToTexture' :: Int             -- ^ Texture width.
                     -> Int             -- ^ Texture height.
                     -> Int             -- ^ First pixel to read X.
                     -> Int             -- ^ First pixel to read Y.
                     -> Int             -- ^ Width of the rectangle to read.
                     -> Int             -- ^ Height of the rectangle to read.
                     -> Layer' s t a    -- ^ Layer to draw.
                     -> Layer' NonDrawable t
                               (a, TTexture t, TTexture t, [Color], [Word8])
colorDepthToTexture' w h rx ry rw rh l =
        fmap (\(x, [ct, dt], Just c, Just d) -> (x, ct, dt, c, d)) $
                TextureLayer False [ColorLayer, DepthLayer] (w, h)
                             (rx, ry, rw, rh) True True l

-- | 'colorToTexture'' with an additional stencil buffer.
colorStencilToTexture' :: Int           -- ^ Texture width.
                       -> Int           -- ^ Texture height.
                       -> Int           -- ^ First pixel to read X.
                       -> Int           -- ^ First pixel to read Y.
                       -> Int           -- ^ Width of the rectangle to read.
                       -> Int           -- ^ Height of the rectangle to read.
                       -> Layer' s t a  -- ^ Layer to draw.
                       -> Layer' NonDrawable t (a, TTexture t, [Color])
colorStencilToTexture' w h rx ry rw rh l =
        fmap (\(x, [t, _], Just c, _) -> (x, t, c)) $
                TextureLayer False [ColorLayer, DepthStencilLayer] (w, h)
                             (rx, ry, rw, rh) True False l

-- | Draw a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) to some 'Texture's and to a depth
-- Texture.
buffersDepthToTexture :: Int            -- ^ Texture width.
                      -> Int            -- ^ Texture height.
                      -> Int            -- ^ Number of colors.
                      -> Layer' s t a   -- ^ Layer to draw.
                      -> Layer' NonDrawable t (a, [TTexture t], TTexture t)
buffersDepthToTexture w h n l =
        fmap (\(x, dt : ts, _, _) -> (x, ts, dt)) $
                TextureLayer True (DepthLayer : map BufferLayer [0 .. n - 1])
                             (w, h) (0, 0, 0, 0) False False l

-- | Draw a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) to some 'Texture's with an additional
-- stencil buffer.
buffersStencilToTexture :: Int          -- ^ Texture width.
                        -> Int          -- ^ Texture height.
                        -> Int          -- ^ Number of colors.
                        -> Layer' s t a -- ^ Layer to draw.
                        -> Layer' NonDrawable t (a, [TTexture t])
buffersStencilToTexture w h n l =
        fmap (\(x, _ : ts, _, _) -> (x, ts)) $
                TextureLayer True
                             (DepthStencilLayer : map BufferLayer [0 .. n - 1])
                             (w, h) (0, 0, 0, 0) False False l

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
           -> [TTexture t]
           -> m (a, [TTexture t])
drawLayer' (Layer prg grp) ts = do setProgram prg
                                   drawObject grp
                                   return ((), ts)
drawLayer' (TextureLayer drawBufs stypes (w, h) (rx, ry, rw, rh)
                         inspCol inspDepth layer) tts0 =
        do (x, tts1, ts, mcol, mdepth) <-
                layerToTexture drawBufs stypes w h layer
                               (mayInspect inspCol) (mayInspect inspDepth) tts0
           let tts2 = map (TTexture . LoadedTexture gw gh) ts
           return ((x, tts2, mcol, mdepth), tts1 ++ tts2)
        where (gw, gh) = (fromIntegral w, fromIntegral h)
        
              mayInspect :: Monad m
                         => Bool
                         -> Either (Maybe [r])
                                   ([r] -> m (Maybe [r]), Int, Int, Int, Int)
              mayInspect True = Right (return . Just, rx, ry, rw, rh)
              mayInspect False = Left Nothing
drawLayer' (Permanent tt@(TTexture lt)) tts = 
        do let t = TextureLoaded lt
           gl $ unloader t (Nothing :: Maybe TextureImage) lt
           return (t, filter (/= tt) tts)
drawLayer' (WithTTextures ets f) tts =
        do drawLayer . f $ map (\(TTexture lt) -> TextureLoaded lt) ets
           return ((), tts)
drawLayer' (Free layer) tts =
        do (x, tts') <- drawLayer' layer []
           mapM_ (\(TTexture lt) -> unusedTexture lt) tts'
           return (x, tts)
drawLayer' (Clear bufs) tts = clearBuffers bufs >> return ((), tts)
drawLayer' (Cast layer) tts =
        do (x, tts') <- drawLayer' layer $ map castTTexture tts
           return (x, map castTTexture tts')
drawLayer' (Bind lx f) tts0 = drawLayer' lx tts0 >>=
                                \(x, tts1) -> drawLayer' (f x) tts1
drawLayer' (Return x) tts = return (x, tts)

-- | Draw a 'Layer' on some textures.
layerToTexture :: (GLES, Integral a, MonadObject m)
               => Bool                                  -- ^ Draw buffers
               -> [LayerType]                           -- ^ Textures contents
               -> a                                     -- ^ Width
               -> a                                     -- ^ Height
               -> Layer' s t x                          -- ^ Layer to draw
               -> Either b ( [Color] -> m b
                           , Int, Int, Int, Int)        -- ^ Color inspecting
                                                        -- function, start x,
                                                        -- start y, width,
                                                        -- height
               -> Either c ( [Word8] -> m c
                           , Int, Int, Int, Int)        -- ^ Depth inspecting,
                                                        -- function, etc.
               -> [TTexture t]
               -> m (x, [TTexture t], [GL.Texture], b ,c)
layerToTexture drawBufs stypes wp hp layer einspc einspd tts = do
        (ts, (x, tts', colRes, depthRes)) <-
                renderToTexture drawBufs (map arguments stypes) w h $
                        do (x, tts') <- drawLayer' layer tts
                           colRes <- inspect einspc gl_RGBA wordsToColors 4
                           depthRes <- inspect einspd gl_DEPTH_COMPONENT id 1
                           return (x, tts', colRes, depthRes)

        return (x, tts', ts, colRes, depthRes)

        where (w, h) = (fromIntegral wp, fromIntegral hp)
              arguments stype =
                        case stype of
                              ColorLayer -> ( fromIntegral gl_RGBA
                                            , gl_RGBA
                                            , gl_UNSIGNED_BYTE
                                            , gl_COLOR_ATTACHMENT0
                                            , [ColorBuffer] )
                              DepthLayer -> ( fromIntegral gl_DEPTH_COMPONENT
                                            , gl_DEPTH_COMPONENT
                                            , gl_UNSIGNED_SHORT
                                            , gl_DEPTH_ATTACHMENT
                                            , [DepthBuffer] )
                              DepthStencilLayer -> ( fromIntegral
                                                        gl_DEPTH_STENCIL
                                                   , gl_DEPTH_STENCIL
                                                   , gl_UNSIGNED_INT_24_8
                                                   , gl_DEPTH_STENCIL_ATTACHMENT
                                                   , [ DepthBuffer
                                                     , StencilBuffer]
                                                   )
                              BufferLayer n -> ( fromIntegral gl_RGBA32F
                                               , gl_RGBA
                                               , gl_FLOAT
                                               , gl_COLOR_ATTACHMENT0 + 
                                                 fromIntegral n
                                               , [] )

              inspect (Left r) _ _ _ = return r
              inspect (Right (insp, x, y, rw, rh)) format trans s =
                        do arr <- liftIO . newByteArray $
                                        fromIntegral rw * fromIntegral rh * s
                           gl $ readPixels (fromIntegral x)
                                           (fromIntegral y)
                                           (fromIntegral rw)
                                           (fromIntegral rh)
                                           format gl_UNSIGNED_BYTE arr
                           liftIO (decodeBytes arr) >>= insp . trans
              wordsToColors (r : g : b : a : xs) = Color r g b a :
                                                   wordsToColors xs
              wordsToColors _ = []

renderToTexture :: (GLES, MonadObject m)
                => Bool
                -> [(GLInt, GLEnum, GLEnum, GLEnum, [Buffer])]
                -> GLSize
                -> GLSize
                -> m a
                -> m ([GL.Texture], a)
renderToTexture drawBufs infos w h act = do
        fb <- gl createFramebuffer 
        gl $ bindFramebuffer gl_FRAMEBUFFER fb

        (ts, attchs, buffersToClear) <- fmap unzip3 . flip mapM infos $
                \(internalFormat, format, pixelType, attachment, buffer) ->
                        do LoadedTexture _ _ t <- newTexture (fromIntegral w)
                                                             (fromIntegral h)
                                                             (Nearest, Nothing)
                                                             Nearest
                           gl $ bindTexture gl_TEXTURE_2D t
                           if pixelType == gl_FLOAT
                           then liftIO noFloat32Array >>=
                                        gl . texImage2DFloat gl_TEXTURE_2D 0
                                                             internalFormat w h
                                                             0 format pixelType
                           else liftIO noUInt8Array >>=
                                        gl . texImage2DUInt gl_TEXTURE_2D 0
                                                            internalFormat w h
                                                            0 format pixelType
                           gl $ framebufferTexture2D gl_FRAMEBUFFER attachment
                                                     gl_TEXTURE_2D t 0
                           return (t, fromIntegral attachment, buffer)

        let buffersToDraw = filter (/= fromIntegral gl_DEPTH_ATTACHMENT) attchs
        when drawBufs $ liftIO (encodeInts buffersToDraw) >>= gl . drawBuffers

        (sw, sh) <- currentViewport
        resizeViewport (fromIntegral w) (fromIntegral h)

        clearBuffers $ concat buffersToClear
        ret <- act

        resizeViewport sw sh
        gl $ deleteFramebuffer fb

        return (ts, ret)
