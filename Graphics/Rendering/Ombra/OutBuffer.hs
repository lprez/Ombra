{-# LANGUAGE DataKinds #-}

module Graphics.Rendering.Ombra.OutBuffer (
        OutStatus(..),
        GBuffer,
        DepthBuffer,
        UsedGBuffer,
        UsedDepthBuffer,
        GBufferSampler,
        DepthBufferSampler,
        sampleGBuffer,
        sampleDepthBuffer,
        emptyFloatGBuffer,
        emptyByteGBuffer,
        emptyDepthBuffer,
        emptyDepthStencilBuffer
) where

import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Texture.Types

sampleGBuffer :: GBufferSampler t o -> GVec2 -> o
sampleGBuffer (GBufferSampler samplers) st =
        fromGVec4s $ map (flip sample st) samplers

sampleDepthBuffer :: DepthBufferSampler t -> GVec2 -> GFloat
sampleDepthBuffer (DepthBufferSampler sampler) st =
        let GVec4 x _ _ _ = sample sampler st
        in x

emptyFloatGBuffer :: FragmentShaderOutput o
                  => Filter
                  -> Filter
                  -> GBuffer t OutEmpty o
emptyFloatGBuffer = EmptyFloatGBuffer

emptyByteGBuffer :: FragmentShaderOutput o
                 => Filter
                 -> Filter
                 -> GBuffer t OutEmpty o
emptyByteGBuffer = EmptyByteGBuffer

emptyDepthBuffer :: Filter -> Filter -> DepthBuffer t OutEmpty
emptyDepthBuffer = EmptyDepthBuffer

emptyDepthStencilBuffer :: Filter -> Filter -> DepthBuffer t OutEmpty
emptyDepthStencilBuffer = EmptyDepthStencilBuffer
