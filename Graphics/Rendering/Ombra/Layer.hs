module Graphics.Rendering.Ombra.Layer (
        Buffer(..),
        Layer,
        layer,
        over,
        clear,
        -- * Programs
        Compatible,
        Program,
        program,
        -- * Sublayers
        subLayer,
        colorSubLayer,
        depthSubLayer,
        colorDepthSubLayer,
        colorStencilSubLayer,
        colorSubLayer',
        depthSubLayer',
        colorDepthSubLayer',
        colorStencilSubLayer',
        buffersSubLayer,
        buffersDepthSubLayer,
        buffersStencilSubLayer,
        -- * Layers with return values
        Layer',
        drawable,
        depthToTexture,
        colorDepthToTexture,
        colorStencilToTexture,
        colorToTexture',
        depthToTexture',
        colorDepthToTexture',
        colorStencilToTexture',
        buffersDepthToTexture,
        buffersStencilToTexture,
        -- ** Temporary textures
        TTexture,
        withTTexture,
        permanent
) where

import Data.Word (Word8)
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Layer.Internal
import Graphics.Rendering.Ombra.Object
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Texture

-- | Create a simple Layer from a Program and an Object.
layer :: (Subset progAttr grpAttr, Subset progUni grpUni)
      => Program progUni progAttr -> Object grpUni grpAttr -> Layer' s t ()
layer = Layer

infixl 1 `over`
-- | Draw the first Layer over the second one. This means that the first Layer
-- will use the same buffers (color, depth, stencil) of the second, but
-- the visibility of the objects still depends on their depth.
over :: Layer -> Layer -> Layer
over = flip (>>)

-- | Alias for 'colorSubLayer'.
subLayer :: Int -> Int -> Layer -> (Texture -> Layer) -> Layer
subLayer = colorSubLayer

-- | Use a 'Layer' as a 'Texture' on another.
colorSubLayer :: Int                    -- ^ Texture width.
              -> Int                    -- ^ Texture height.
              -> Layer                  -- ^ Layer to draw on a 'Texture'.
              -> (Texture -> Layer)     -- ^ Layers using the texture.
              -> Layer
colorSubLayer w h l = colorDepthSubLayer w h l . flip . const

-- | Use a 'Layer' as a depth 'Texture' on another.
depthSubLayer :: Int                         -- ^ Texture width.
              -> Int                         -- ^ Texture height.
              -> Layer                       -- ^ Layer to draw on a
                                             -- depth 'Texture'.
              -> (Texture -> Layer)          -- ^ Layers using the texture.
              -> Layer
depthSubLayer w h l f = drawable $
        depthToTexture w h (castDrawable l) >>= \(_, t) -> withTTexture t f

-- | Combination of 'colorSubLayer' and 'depthSubLayer'.
colorDepthSubLayer :: Int                               -- ^ Texture width.
                   -> Int                               -- ^ Texture height.
                   -> Layer                             -- ^ Layer to draw on the
                                                        -- 'Texture's.
                   -> (Texture -> Texture -> Layer)     -- ^ Color, depth.
                   -> Layer
colorDepthSubLayer w h l f = drawable $
        colorDepthToTexture w h (castDrawable l) >>=
                \(_, ct, dt) -> withTTextures [ct, dt] $
                        \[ct', dt'] -> f ct' dt'

-- | 'colorSubLayer' with a stencil buffer.
colorStencilSubLayer :: Int                     -- ^ Texture width.
                     -> Int                     -- ^ Texture height.
                     -> Layer                   -- ^ Layer to draw on a 'Texture'
                     -> (Texture -> Layer)      -- ^ Color.
                     -> Layer
colorStencilSubLayer w h l f = drawable $
        colorStencilToTexture w h (castDrawable l) >>= \(_, t) -> withTTexture t f

-- | Extended version of 'colorSubLayer' that reads and converts the Texture
-- pixels.
colorSubLayer'
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> Layer                        -- ^ Layer to draw on a 'Texture'.
        -> (Texture -> [Color] -> Layer) -- ^ Function using the texture.
        -> Layer
colorSubLayer' w h rx ry rw rh l f = drawable $
        colorToTexture' w h rx ry rw rh (castDrawable l) >>=
                \(_, t, c) -> withTTexture t $ flip f c

-- | Extended version of 'depthSubLayer'. Not supported on WebGL.
depthSubLayer'
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> Layer                        -- ^ Layer to draw on a depth 'Texture'.
        -> (Texture -> [Word8] -> Layer) -- ^ Layers using the texture.
        -> Layer
depthSubLayer' w h rx ry rw rh l f = drawable $
        depthToTexture' w h rx ry rw rh (castDrawable l) >>=
                \(_, t, d) -> withTTexture t $ flip f d

-- | Extended version of 'colorDepthSubLayer'. Not supported on WebGL.
colorDepthSubLayer'
        :: Int         -- ^ Texture width.
        -> Int         -- ^ Texture height.
        -> Int         -- ^ First pixel to read X
        -> Int         -- ^ First pixel to read Y
        -> Int         -- ^ Width of the rectangle to read
        -> Int         -- ^ Height of the rectangle to read
        -> Layer       -- ^ Layer to draw on a 'Texture'
        -> (Texture -> Texture -> [Color] -> [Word8] -> Layer) -- ^ Layers using
                                                               -- the texture.
        -> Layer
colorDepthSubLayer' w h rx ry rw rh l f = drawable $
        colorDepthToTexture' w h rx ry rw rh (castDrawable l) >>=
                \(_, ct, dt, c, d) -> withTTextures [ct, dt] $
                        \[ct', dt'] -> f ct' dt' c d

-- | 'colorSubLayer'' with an additional stencil buffer.
colorStencilSubLayer'
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> Layer                        -- ^ Layer to draw on a 'Texture'.
        -> (Texture -> [Color] -> Layer) -- ^ Function using the texture.
        -> Layer
colorStencilSubLayer' w h rx ry rw rh l f = drawable $
        colorStencilToTexture' w h rx ry rw rh (castDrawable l) >>=
                \(_, t, c) -> withTTexture t $ flip f c

-- | Draw a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) to some 'Texture's and use them to
-- create another Layer.
buffersSubLayer :: Int                          -- ^ Textures width.
                -> Int                          -- ^ Textures height.
                -> Int                          -- ^ Number of colors.
                -> Layer                        -- ^ Layer to draw.
                -> ([Texture] -> Layer)         -- ^ Function using the textures.
                -> Layer
buffersSubLayer w h n l = buffersDepthSubLayer w h n l . flip . const

-- | Combination of 'buffersSubLayer' and 'depthSubLayer'.
buffersDepthSubLayer :: Int                             -- ^ Textures width.
                   -> Int                               -- ^ Textures height.
                   -> Int                               -- ^ Number of colors.
                   -> Layer                             -- ^ Layer to draw.
                   -> ([Texture] -> Texture -> Layer)   -- ^ Function using the
                                                        -- buffers textures and
                                                        -- the depth texture.
                   -> Layer
buffersDepthSubLayer w h n l f = drawable $
        buffersDepthToTexture w h n (castDrawable l) >>=
                \(_, bts, dt) -> withTTextures (dt : bts) $
                        \(dt' : bts') -> f bts' dt'

-- | 'buffersSubLayer' with an additional stencil buffer.
buffersStencilSubLayer :: Int                   -- ^ Textures width.
                       -> Int                   -- ^ Textures height.
                       -> Int                   -- ^ Number of colors.
                       -> Layer                 -- ^ Layer to draw.
                       -> ([Texture] -> Layer)  -- ^ Function using the texture.
                       -> Layer
buffersStencilSubLayer w h n l f = drawable $
        buffersStencilToTexture w h n (castDrawable l) >>=
                \(_, bts) -> withTTextures bts f
