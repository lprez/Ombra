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

data OutBufferInfo o where
        EmptyFloatGBuffer               :: FragmentShaderOutput o
                                        => TextureParameters
                                        -> OutBufferInfo o

        EmptyByteGBuffer                :: FragmentShaderOutput o
                                        => TextureParameters
                                        -> OutBufferInfo o

        EmptyDepthBuffer                :: TextureParameters
                                        -> OutBufferInfo OutDepthBuffer

        EmptyDepthStencilBuffer         :: TextureParameters
                                        -> OutBufferInfo OutDepthBuffer

data OutBuffer t o where
        TextureFloatGBuffer             :: FragmentShaderOutput o
                                        => [LoadedTexture]
                                        -> OutBuffer t o

        TextureByteGBuffer              :: FragmentShaderOutput o
                                        => [LoadedTexture]
                                        -> OutBuffer t o

        TextureDepthBuffer              :: LoadedTexture
                                        -> OutBuffer t OutDepthBuffer

        TextureDepthStencilBuffer       :: LoadedTexture
                                        -> OutBuffer t OutDepthBuffer

instance FragmentShaderOutput o => MultiShaderType (GBufferSampler t o) where
        type ExprMST (GBufferSampler t o) = [ExprMST GSampler2D]
        mapMST f (GBufferSampler x) = GBufferSampler $ map f x
        toExprMST (GBufferSampler x) = map toExprMST x
        fromExprMST = GBufferSampler . map fromExprMST

instance FragmentShaderOutput o => ShaderInput (GBufferSampler t o) where
        buildMST f i = (GBufferSampler $ take n infiniteSamplers, i + n)
                where n = textureCount (Proxy :: Proxy o)
                      infiniteSamplers = map f [i ..]
        foldrMST f s (GBufferSampler x) = foldr f s x

instance FragmentShaderOutput o => Uniform (GBufferSampler t o) where
        type CPUUniform (GBufferSampler t o) = GBuffer t o
        foldrUniform _ f s buf =
                foldr (\u s -> f (UniformTexture $ TextureLoaded u) s)
                      s (textures buf)

instance MultiShaderType (DepthBufferSampler t) where
        type ExprMST (DepthBufferSampler t) = ExprMST GSampler2D
        mapMST f (DepthBufferSampler x) = DepthBufferSampler $ f x
        toExprMST (DepthBufferSampler x) = toExprMST x
        fromExprMST = DepthBufferSampler . fromExprMST

instance ShaderInput (DepthBufferSampler t) where
        buildMST f i = (DepthBufferSampler $ f i, i + 1)
        foldrMST f s (DepthBufferSampler x) = foldrMST f s x

instance Uniform (DepthBufferSampler t) where
        type CPUUniform (DepthBufferSampler t) = DepthBuffer t
        foldrUniform _ f s buf =
                f (UniformTexture . TextureLoaded . head . textures $ buf) s

-- | A container that can be used to store the output of some drawing operation.
type GBuffer = OutBuffer
-- | A container for depth/stencil values.
type DepthBuffer t = OutBuffer t OutDepthBuffer

type GBufferInfo = OutBufferInfo
type DepthBufferInfo = OutBufferInfo OutDepthBuffer

textures :: OutBuffer t o -> [LoadedTexture]
textures (TextureFloatGBuffer ts) = ts
textures (TextureByteGBuffer ts) = ts
textures (TextureDepthBuffer t) = [t]
textures (TextureDepthStencilBuffer t) = [t]

castBuffer :: OutBuffer t o -> OutBuffer t' o
castBuffer (TextureFloatGBuffer ts) = TextureFloatGBuffer ts
castBuffer (TextureByteGBuffer ts) = TextureByteGBuffer ts
castBuffer (TextureDepthBuffer t) = TextureDepthBuffer t
castBuffer (TextureDepthStencilBuffer t) = TextureDepthStencilBuffer t
