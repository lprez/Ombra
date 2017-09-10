{-# LANGUAGE DataKinds #-}

module Graphics.Rendering.Ombra.OutBuffer (
        GBuffer,
        DepthBuffer,
        GBufferInfo,
        DepthBufferInfo,
        GBufferSampler,
        DepthBufferSampler,
        sampleGBuffer,
        sampleDepthBuffer,
        floatGBuffer,
        byteGBuffer,
        depthBuffer,
        depthStencilBuffer,
        -- * Conversion between buffers and textures
        toTextureSampler,
        fromTextureSampler
) where

import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Texture.Types

-- | Sample a value from a 'GBufferSampler'.
sampleGBuffer :: GBufferSampler t o -> GVec2 -> o
sampleGBuffer (GBufferSampler samplers) st =
        fromGVec4s $ map (flip sample st) samplers

-- | Sample a value from a 'DepthBufferSampler'.
sampleDepthBuffer :: DepthBufferSampler t -> GVec2 -> GFloat
sampleDepthBuffer (DepthBufferSampler sampler) st =
        let GVec4 x _ _ _ = sample sampler st in x

fromTextureSampler :: TextureSampler -> GBufferSampler t GVec4
fromTextureSampler sampler = GBufferSampler [sampler]

toTextureSampler :: GBufferSampler t GVec4 -> TextureSampler
toTextureSampler (GBufferSampler (sampler : _)) = sampler

floatGBuffer :: FragmentShaderOutput o => TextureParameters -> GBufferInfo o
floatGBuffer = EmptyFloatGBuffer

byteGBuffer :: FragmentShaderOutput o => TextureParameters -> GBufferInfo o
byteGBuffer = EmptyByteGBuffer

depthBuffer :: TextureParameters -> DepthBufferInfo
depthBuffer = EmptyDepthBuffer

depthStencilBuffer :: TextureParameters -> DepthBufferInfo
depthStencilBuffer = EmptyDepthStencilBuffer
