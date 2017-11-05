{-# LANGUAGE DataKinds #-}

module Graphics.Rendering.Ombra.OutBuffer (
        GBuffer,
        DepthBuffer,
        BufferPair,
        GBufferInfo,
        DepthBufferInfo,
        GBufferSampler,
        DepthBufferSampler,
        sampleGBuffer,
        sampleDepthBuffer,
        floatGBufferInfo,
        byteGBufferInfo,
        depthBufferInfo,
        depthStencilBufferInfo,
        bufferPair,
        gBuffer,
        depthBuffer,
        bufferSize,
        -- * Conversion between buffers and textures
        toTextureSampler,
        fromTextureSampler
) where

import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Texture.Types

bufferPair :: GBuffer o -> DepthBuffer -> Maybe (BufferPair o)
bufferPair g d | bufferSize g == bufferSize d = Just $ BufferPair g d

gBuffer :: BufferPair o -> GBuffer o
gBuffer (BufferPair buf _) = buf

depthBuffer :: BufferPair o -> DepthBuffer
depthBuffer (BufferPair _ buf) = buf

-- | Sample a value from a 'GBufferSampler'.
sampleGBuffer :: GBufferSampler o -> GVec2 -> o
sampleGBuffer (GBufferSampler samplers) st =
        fromGVec4s $ map (flip sample st) samplers

-- | Sample a value from a 'DepthBufferSampler'.
sampleDepthBuffer :: DepthBufferSampler -> GVec2 -> GFloat
sampleDepthBuffer (DepthBufferSampler sampler) st =
        let GVec4 x _ _ _ = sample sampler st in x

fromTextureSampler :: TextureSampler -> GBufferSampler GVec4
fromTextureSampler sampler = GBufferSampler [sampler]

toTextureSampler :: GBufferSampler GVec4 -> TextureSampler
toTextureSampler (GBufferSampler (sampler : _)) = sampler

floatGBufferInfo :: FragmentShaderOutput o => TextureParameters -> GBufferInfo o
floatGBufferInfo = EmptyFloatGBuffer

byteGBufferInfo :: FragmentShaderOutput o => TextureParameters -> GBufferInfo o
byteGBufferInfo = EmptyByteGBuffer

depthBufferInfo :: TextureParameters -> DepthBufferInfo
depthBufferInfo = EmptyDepthBuffer

depthStencilBufferInfo :: TextureParameters -> DepthBufferInfo
depthStencilBufferInfo = EmptyDepthStencilBuffer
