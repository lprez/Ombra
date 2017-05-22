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
        buffersSubLayer,
        buffersDepthSubLayer,
        buffersStencilSubLayer,
        -- * Layers with return values
        Layer',
        LayerStatus(..),
        drawable,
        castLayer,
        -- ** Temporary textures
        TTexture,
        TTextureType(..),
        newTTexture,
        withTTexture,
        withTTextures,
        withTTextures',
        permanent,
        -- ** Drawing to textures
        depthToTexture,
        colorDepthToTexture,
        colorStencilToTexture,
        buffersDepthToTexture,
        buffersStencilToTexture,
        layerToTexture,
        -- * Reading screen pixels
        readColor,
        readColorFloat,
        readDepth,
        readDepthFloat,
        readStencil
) where

import Data.Word (Word8)
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Layer.Internal
import Graphics.Rendering.Ombra.Layer.Types
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
        depthToTexture w h (castLayer l) >>=
                \(_, t) -> withTTexture t f

-- | Combination of 'colorSubLayer' and 'depthSubLayer'.
colorDepthSubLayer :: Int                               -- ^ Texture width.
                   -> Int                               -- ^ Texture height.
                   -> Layer                             -- ^ Layer to draw on the
                                                        -- 'Texture's.
                   -> (Texture -> Texture -> Layer)     -- ^ Color, depth.
                   -> Layer
colorDepthSubLayer w h l f = drawable $
        colorDepthToTexture w h (castLayer l) >>=
                \(_, ct, dt) -> withTTextures' [ct] $
                        \[ct'] -> withTTextures' [dt] $
                                \[dt'] -> castLayer $ f ct' dt'

-- | 'colorSubLayer' with a stencil buffer.
colorStencilSubLayer :: Int                     -- ^ Texture width.
                     -> Int                     -- ^ Texture height.
                     -> Layer                   -- ^ Layer to draw on a 'Texture'
                     -> (Texture -> Layer)      -- ^ Color.
                     -> Layer
colorStencilSubLayer w h l f = drawable $
        colorStencilToTexture w h (castLayer l) >>=
                \(_, t, _) -> withTTexture t f

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
                     -> Int                             -- ^ Textures height.
                     -> Int                             -- ^ Number of colors.
                     -> Layer                           -- ^ Layer to draw.
                     -> ([Texture] -> Texture -> Layer) -- ^ Function using the
                                                        -- buffers textures and
                                                        -- the depth texture.
                     -> Layer
buffersDepthSubLayer w h n l f = drawable $
        buffersDepthToTexture w h n (castLayer l) >>=
                \(_, bts, dt) -> withTTextures' [dt] $
                        \[dt'] -> withTTextures' bts $
                                \bts' -> castLayer $ f bts' dt'

-- | 'buffersSubLayer' with an additional stencil buffer.
buffersStencilSubLayer :: Int                   -- ^ Textures width.
                       -> Int                   -- ^ Textures height.
                       -> Int                   -- ^ Number of colors.
                       -> Layer                 -- ^ Layer to draw.
                       -> ([Texture] -> Layer)  -- ^ Function using the texture.
                       -> Layer
buffersStencilSubLayer w h n l f = drawable $
        buffersStencilToTexture w h n (castLayer l) >>=
                \(_, bts, _) -> withTTextures bts f
