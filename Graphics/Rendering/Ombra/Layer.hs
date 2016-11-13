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
      => Program progUni progAttr -> Object grpUni grpAttr -> Layer
layer = Layer

infixl 1 `over`
-- | Draw the first Layer over the second one. This means that the first Layer
-- will use the same buffers (color, depth, stencil) of the second, but
-- the visibility of the objects still depends on their depth.
over :: Layer -> Layer -> Layer
over = OverLayer

-- TODO: document buffers.
-- | Clear some buffers before drawing a Layer.
clear :: [Buffer] -> Layer -> Layer
clear = ClearLayer

-- | Generate a 1x1 texture.
colorTex :: GLES => Color -> Texture
colorTex c = mkTexture 1 1 [ c ]

-- | Alias for 'colorSubLayer'.
subLayer :: Int -> Int -> Layer -> (Texture -> Layer) -> Layer
subLayer = colorSubLayer

-- | Use a 'Layer' as a 'Texture' on another.
colorSubLayer :: Int                    -- ^ Texture width.
              -> Int                    -- ^ Texture height.
              -> Layer                  -- ^ Layer to draw on a 'Texture'.
              -> (Texture -> Layer)     -- ^ Layers using the texture.
              -> Layer
colorSubLayer w h l = subRenderLayer . renderColor w h l

-- | Use a 'Layer' as a depth 'Texture' on another.
depthSubLayer :: Int                         -- ^ Texture width.
              -> Int                         -- ^ Texture height.
              -> Layer                       -- ^ Layer to draw on a
                                             -- depth 'Texture'.
              -> (Texture -> Layer)          -- ^ Layers using the texture.
              -> Layer
depthSubLayer w h l = subRenderLayer . renderDepth w h l

-- | Combination of 'colorSubLayer' and 'depthSubLayer'.
colorDepthSubLayer :: Int                               -- ^ Texture width.
                   -> Int                               -- ^ Texture height.
                   -> Layer                             -- ^ Layer to draw on the
                                                        -- 'Texture's.
                   -> (Texture -> Texture -> Layer)     -- ^ Color, depth.
                   -> Layer
colorDepthSubLayer w h l = subRenderLayer . renderColorDepth w h l

-- | 'colorSubLayer' with a stencil buffer.
colorStencilSubLayer :: Int                     -- ^ Texture width.
                     -> Int                     -- ^ Texture height.
                     -> Layer                   -- ^ Layer to draw on a 'Texture'
                     -> (Texture -> Layer)      -- ^ Color.
                     -> Layer
colorStencilSubLayer w h l = subRenderLayer . renderColorStencil w h l

-- | Extended version of 'colorSubLayer' that reads and converts the Texture
-- pixels.
colorSubLayer'
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Color] -> Layer) -- ^ Function using the texture.
        -> Layer
colorSubLayer' w h l rx ry rw rh =
        subRenderLayer . renderColorInspect w h l rx ry rw rh

-- | Extended version of 'depthSubLayer'. Not supported on WebGL.
depthSubLayer'
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a depth 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Word8] -> Layer) -- ^ Layers using the texture.
        -> Layer
depthSubLayer' w h l rx ry rw rh =
        subRenderLayer . renderDepthInspect w h l rx ry rw rh

-- | Extended version of 'colorDepthSubLayer'. Not supported on WebGL.
colorDepthSubLayer'
        :: Int         -- ^ Texture width.
        -> Int         -- ^ Texture height.
        -> Layer       -- ^ Layer to draw on a 'Texture'
        -> Int         -- ^ First pixel to read X
        -> Int         -- ^ First pixel to read Y
        -> Int         -- ^ Width of the rectangle to read
        -> Int         -- ^ Height of the rectangle to read
        -> (Texture -> Texture -> [Color] -> [Word8] -> Layer) -- ^ Layers using
                                                               -- the texture.
        -> Layer
colorDepthSubLayer' w h l rx ry rw rh =
        subRenderLayer . renderColorDepthInspect w h l rx ry rw rh

-- | 'colorSubLayer'' with an additional stencil buffer.
colorStencilSubLayer'
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Color] -> Layer) -- ^ Function using the texture.
        -> Layer
colorStencilSubLayer' w h l rx ry rw rh =
        subRenderLayer . renderColorStencilInspect w h l rx ry rw rh

-- | Render a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) in some 'Texture's and use them to
-- create another Layer.
buffersSubLayer :: Int                          -- ^ Textures width.
                -> Int                          -- ^ Textures height.
                -> Int                          -- ^ Number of colors.
                -> Layer                        -- ^ Layer to draw.
                -> ([Texture] -> Layer)         -- ^ Function using the textures.
                -> Layer
buffersSubLayer w h n l = subRenderLayer . renderBuffers w h n l

-- | Combination of 'buffersSubLayer' and 'depthSubLayer'.
buffersDepthSubLayer :: Int                             -- ^ Textures width.
                   -> Int                               -- ^ Textures height.
                   -> Int                               -- ^ Number of colors.
                   -> Layer                             -- ^ Layer to draw.
                   -> ([Texture] -> Texture -> Layer)   -- ^ Function using the
                                                        -- buffers textures and
                                                        -- the depth texture.
                   -> Layer
buffersDepthSubLayer w h n l = subRenderLayer . renderBuffersDepth w h n l

-- | 'buffersSubLayer' with an additional stencil buffer.
buffersStencilSubLayer :: Int                   -- ^ Textures width.
                       -> Int                   -- ^ Textures height.
                       -> Int                   -- ^ Number of colors.
                       -> Layer                 -- ^ Layer to draw.
                       -> ([Texture] -> Layer)  -- ^ Function using the texture.
                       -> Layer
buffersStencilSubLayer w h n l = subRenderLayer . renderBuffersStencil w h n l

subRenderLayer :: RenderLayer Layer -> Layer
subRenderLayer = SubLayer

-- | Render a 'Layer' in a 'Texture'.
renderColor :: Int -> Int -> Layer -> (Texture -> a) -> RenderLayer a
renderColor w h l f = RenderLayer False [ColorLayer, DepthLayer] w h 0 0 0 0
                                  False False l $ \[t, _] _ _ -> f t

-- | Render a 'Layer' in a depth 'Texture'.
renderDepth :: Int -> Int -> Layer -> (Texture -> a) -> RenderLayer a
renderDepth w h l f =
        RenderLayer False [DepthLayer] w h 0 0 0 0 False False l $
                \[t] _ _ -> f t

-- | Combination of 'renderColor' and 'renderDepth'.
renderColorDepth :: Int
                 -> Int
                 -> Layer
                 -> (Texture -> Texture -> a)
                 -> RenderLayer a
renderColorDepth w h l f =
        RenderLayer False [ColorLayer, DepthLayer] w h 0 0 0 0 False False l $
                    \[ct, dt] _ _ -> f ct dt

-- | 'renderColor' with an additional stencil buffer.
renderColorStencil :: Int
                   -> Int
                   -> Layer
                   -> (Texture -> a)
                   -> RenderLayer a
renderColorStencil w h l f =
        RenderLayer False [ColorLayer, DepthStencilLayer] w h 0 0 0 0
                    False False l $
                    \[ct, _] _ _ -> f ct

-- | Render a 'Layer' in a 'Texture', reading the content of the texture.
renderColorInspect :: Int
                   -> Int
                   -> Layer
                   -> Int
                   -> Int
                   -> Int
                   -> Int
                   -> (Texture -> [Color] -> a)
                   -> RenderLayer a
renderColorInspect w h l rx ry rw rh f =
        RenderLayer False [ColorLayer, DepthLayer] w h rx ry
                    rw rh True False l $
                    \[t, _] (Just c) _ -> f t c

-- | Render a 'Layer' in a depth 'Texture', reading the content of the texture.
-- Not supported on WebGL.
renderDepthInspect :: Int
                   -> Int
                   -> Layer
                   -> Int
                   -> Int
                   -> Int
                   -> Int
                   -> (Texture -> [Word8] -> a)
                   -> RenderLayer a
renderDepthInspect w h l rx ry rw rh f =
        RenderLayer False [DepthLayer] w h rx ry rw rh False True l $
                    \[t] _ (Just d) -> f t d

-- | Combination of 'renderColorInspect' and 'renderDepthInspect'. Not supported
-- on WebGL.
renderColorDepthInspect :: Int
                        -> Int
                        -> Layer
                        -> Int
                        -> Int
                        -> Int
                        -> Int
                        -> (Texture -> Texture -> [Color] -> [Word8] -> a)

                        -> RenderLayer a
renderColorDepthInspect w h l rx ry rw rh f =
        RenderLayer False [ColorLayer, DepthLayer] w h rx ry rw rh True True l $
                    \[ct, dt] (Just c) (Just d) -> f ct dt c d

-- | 'renderColorInspect' with an additional stencil buffer.
renderColorStencilInspect :: Int
                          -> Int
                          -> Layer
                          -> Int
                          -> Int
                          -> Int
                          -> Int
                          -> (Texture -> [Color] -> a)
                          -> RenderLayer a
renderColorStencilInspect w h l rx ry rw rh f =
        RenderLayer False [ColorLayer, DepthStencilLayer] w h rx ry
                    rw rh True False l $
                    \[t, _] (Just c) _ -> f t c

-- | Render a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) in some 'Texture's.
renderBuffers :: Int -> Int -> Int -> Layer -> ([Texture] -> a) -> RenderLayer a
renderBuffers w h n l f =
        RenderLayer True (DepthLayer : map BufferLayer [0 .. n - 1]) w h
                    0 0 0 0 False False l $ \(_ : ts) _ _ -> f ts

-- | Combination of 'renderBuffers' and 'renderDepth'.
renderBuffersDepth :: Int
                   -> Int
                   -> Int
                   -> Layer
                   -> ([Texture] -> Texture -> a)
                   -> RenderLayer a
renderBuffersDepth w h n l f =
        RenderLayer True (DepthLayer : map BufferLayer [0 .. n - 1]) w h
                    0 0 0 0 False False l $ \(dt : ts) _ _ -> f ts dt

-- | 'renderBuffers' with an additional stencil buffer.
renderBuffersStencil :: Int
                     -> Int
                     -> Int
                     -> Layer
                     -> ([Texture] -> a)
                     -> RenderLayer a
renderBuffersStencil w h n l f =
        RenderLayer True (DepthStencilLayer : map BufferLayer [0 .. n - 1]) w h
                    0 0 0 0 False False l $ \(_ : ts) _ _ -> f ts
