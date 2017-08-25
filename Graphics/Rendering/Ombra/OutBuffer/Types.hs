{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies,
             ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.OutBuffer.Types where

import Data.Proxy
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Texture.Types

-- | A GPU object that can be used to retrieve data from a 'GBuffer'.
data GBufferSampler t o = FragmentShaderOutput o => GBufferSampler [GSampler2D]

-- | A GPU object that can be used to retrieve data from a 'DepthBuffer'.
newtype DepthBufferSampler t = DepthBufferSampler GSampler2D

data OutDepthBuffer
data OutStatus = OutEmpty | OutUsed

data OutBuffer t (s :: OutStatus) o where
        EmptyFloatGBuffer               :: FragmentShaderOutput o
                                        => Filter
                                        -> Filter
                                        -> OutBuffer t OutEmpty o

        EmptyByteGBuffer                :: FragmentShaderOutput o
                                        => Filter
                                        -> Filter
                                        -> OutBuffer t OutEmpty o

        EmptyDepthBuffer                :: Filter
                                        -> Filter
                                        -> OutBuffer t OutEmpty OutDepthBuffer

        EmptyDepthStencilBuffer         :: Filter
                                        -> Filter
                                        -> OutBuffer t OutEmpty OutDepthBuffer

        TextureFloatGBuffer             :: FragmentShaderOutput o
                                        => [LoadedTexture]
                                        -> OutBuffer t OutUsed o

        TextureByteGBuffer              :: FragmentShaderOutput o
                                        => [LoadedTexture]
                                        -> OutBuffer t OutUsed o

        TextureDepthBuffer              :: LoadedTexture
                                        -> OutBuffer t OutUsed OutDepthBuffer

        TextureDepthStencilBuffer       :: LoadedTexture
                                        -> OutBuffer t OutUsed OutDepthBuffer

instance FragmentShaderOutput o => MultiShaderType (GBufferSampler t o) where
        type ExprMST (GBufferSampler t o) = [ExprMST GSampler2D]
        mapMST f (GBufferSampler x) = GBufferSampler $ map f x
        foldrMST f s (GBufferSampler x) = foldr f s x
        toExprMST (GBufferSampler x) = map toExprMST x
        fromExprMST = GBufferSampler . map fromExprMST

instance FragmentShaderOutput o => ShaderInput (GBufferSampler t o) where
        buildMST f i = (GBufferSampler $ take n infiniteSamplers, i + n)
                where n = textureCount (Proxy :: Proxy o)
                      infiniteSamplers = map f [i ..]

instance FragmentShaderOutput o => Uniform (GBufferSampler t o) where
        type CPUUniform (GBufferSampler t o) = UsedGBuffer t o
        foldrUniform _ f s buf =
                foldr (\u s -> f (UniformTexture $ TextureLoaded u) s)
                      s (textures buf)

instance MultiShaderType (DepthBufferSampler t) where
        type ExprMST (DepthBufferSampler t) = ExprMST GSampler2D
        mapMST f (DepthBufferSampler x) = DepthBufferSampler $ f x
        foldrMST f s (DepthBufferSampler x) = foldrMST f s x
        toExprMST (DepthBufferSampler x) = toExprMST x
        fromExprMST = DepthBufferSampler . fromExprMST

instance ShaderInput (DepthBufferSampler t) where
        buildMST f i = (DepthBufferSampler $ f i, i + 1)

instance Uniform (DepthBufferSampler t) where
        type CPUUniform (DepthBufferSampler t) = UsedDepthBuffer t
        foldrUniform _ f s buf =
                f (UniformTexture . TextureLoaded . head . textures $ buf) s

-- | A container that can be used to store the output of some drawing operation.
type GBuffer = OutBuffer
-- | A container for depth/stencil values.
type DepthBuffer t s = OutBuffer t s OutDepthBuffer

-- | A 'GBuffer' that was previously drawn to.
type UsedGBuffer t o = GBuffer t OutUsed o
-- | A 'DepthBuffer' that was previously drawn to.
type UsedDepthBuffer t = DepthBuffer t OutUsed

textures :: OutBuffer t OutUsed o -> [LoadedTexture]
textures (TextureFloatGBuffer ts) = ts
textures (TextureByteGBuffer ts) = ts
textures (TextureDepthBuffer t) = [t]
textures (TextureDepthStencilBuffer t) = [t]

castUsedBuffer :: OutBuffer t OutUsed o -> OutBuffer t' OutUsed o
castUsedBuffer (TextureFloatGBuffer ts) = TextureFloatGBuffer ts
castUsedBuffer (TextureByteGBuffer ts) = TextureByteGBuffer ts
castUsedBuffer (TextureDepthBuffer t) = TextureDepthBuffer t
castUsedBuffer (TextureDepthStencilBuffer t) = TextureDepthStencilBuffer t
