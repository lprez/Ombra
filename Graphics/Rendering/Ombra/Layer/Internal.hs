{-# LANGUAGE GADTs, RankNTypes, DataKinds, KindSignatures #-}

module Graphics.Rendering.Ombra.Layer.Internal where

import Data.Word (Word8)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Object.Internal
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Texture.Internal

-- | An 'Object' associated with a program.
type Layer = Layer' Drawable () ()

-- | A layer with a return value. It may also be 'NonDrawable', this means that
-- there are some protected temporary resources and you have to call 'drawable'
-- to turn it into a normal layer. The second parameter prevents the 'TTexture's
-- from being returned by a @NonDrawable@ layer in a @drawable@ operation.
--
-- Note that layers are monads: @flip ('>>')@ is equivalent to 'over' for
-- Drawable layers, while ('>>=') can be used to achieve the same effect of
-- the subLayer functions.
data Layer' (s :: LayerStatus) t a where
        Layer :: (Subset pi oi, Subset pg og)
              => Program pg pi
              -> Object og oi
              -> Layer' s t ()
        TextureLayer :: Bool                    -- Use drawBuffers
                     -> [LayerType]             -- Attachments
                     -> (Int, Int)              -- Width, height
                     -> (Int, Int, Int, Int)    -- Inspect rectangle
                     -> Bool                    -- Inspect color
                     -> Bool                    -- Inspect depth
                     -> Layer' s t a            -- Layer to draw
                     -> Layer' NonDrawable t
                               (a, [TTexture t], Maybe [Color], Maybe [Word8])
        Permanent :: TTexture t -> Layer' NonDrawable t Texture
        WithTTextures :: [TTexture t]
                      -> ([Texture] -> Layer)
                      -> Layer' NonDrawable t ()
        Free :: (forall t. Layer' NonDrawable t a) -> Layer' s t a
        Clear :: [Buffer] -> Layer' s t ()
        Cast :: Layer' Drawable t a -> Layer' Drawable t' a
        Bind :: Layer' s t a -> (a -> Layer' s t b) -> Layer' s t b
        Return :: a -> Layer' s t a

-- | Temporary texture.
newtype TTexture t = TTexture LoadedTexture deriving Eq

data LayerStatus = Drawable | NonDrawable

data Buffer = ColorBuffer | DepthBuffer | StencilBuffer

data LayerType = ColorLayer
               | DepthLayer
               | DepthStencilLayer
               | BufferLayer Int deriving Eq

instance Functor (Layer' s t) where
        fmap f = flip Bind $ Return . f

instance Applicative (Layer' s t) where
        lf <*> lx = Bind lf $ \f -> Bind lx $ \x -> Return $ f x
        pure = Return

instance Monad (Layer' s t) where
        (>>=) = Bind
        return = Return

-- TODO: document buffers.
-- | Clear some buffers before drawing a Layer.
clear :: [Buffer] -> Layer' s t a -> Layer' s t a
clear bs = (Clear bs >>)

-- | Free the temporary resources associated with a NonDrawable layer, before
-- drawing it.
drawable :: (forall t. Layer' NonDrawable t a) -> Layer' s t a
drawable = Free

castDrawable :: Layer' Drawable t a -> Layer' Drawable t' a
castDrawable = Cast

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
