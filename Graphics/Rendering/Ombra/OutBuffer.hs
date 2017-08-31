{-# LANGUAGE DataKinds #-}

module Graphics.Rendering.Ombra.OutBuffer (
        OutStatus(..),
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
        toGSampler2D,
        fromGSampler2D
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
        let GVec4 x _ _ _ = sample sampler st
        in x

fromGSampler2D :: GSampler2D -> GBufferSampler t GVec4
fromGSampler2D sampler = GBufferSampler [sampler]

toGSampler2D :: GBufferSampler t GVec4 -> GSampler2D
toGSampler2D (GBufferSampler (sampler : _)) = sampler

floatGBuffer :: FragmentShaderOutput o
             => Filter
             -> Filter
             -> GBufferInfo o
floatGBuffer = EmptyFloatGBuffer

byteGBuffer :: FragmentShaderOutput o
            => Filter
            -> Filter
            -> GBufferInfo o
byteGBuffer = EmptyByteGBuffer

depthBuffer :: Filter -> Filter -> DepthBufferInfo
depthBuffer = EmptyDepthBuffer

depthStencilBuffer :: Filter -> Filter -> DepthBufferInfo
depthStencilBuffer = EmptyDepthStencilBuffer
