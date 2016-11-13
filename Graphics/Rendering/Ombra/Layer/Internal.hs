{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Rendering.Ombra.Layer.Internal where

import Data.Word (Word8)
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Object.Internal
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Texture

-- | An 'Object' associated with a program.
data Layer = forall oi pi og pg. (Subset pi oi, Subset pg og)
                              => Layer (Program pg pi) (Object og oi)
           | SubLayer (RenderLayer Layer)
           | OverLayer Layer Layer
           | ClearLayer [Buffer] Layer

data Buffer = ColorBuffer | DepthBuffer | StencilBuffer

-- | Represents a 'Layer' drawn on a 'Texture'.
data RenderLayer a = RenderLayer Bool                   -- Use drawBuffers
                                 [LayerType]            -- Attachments
                                 Int Int                -- Width, height
                                 Int Int Int Int        -- Inspect rectangle
                                 Bool Bool              -- Inspect color, depth
                                 Layer                  -- Layer to draw
                                 ([Texture] -> Maybe [Color] ->
                                  Maybe [Word8] -> a)   -- Accepting function

data LayerType = ColorLayer
               | DepthLayer
               | DepthStencilLayer
               | BufferLayer Int deriving Eq

