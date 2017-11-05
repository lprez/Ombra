{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies,
             ScopedTypeVariables, UndecidableInstances #-}

module Graphics.Rendering.Ombra.OutBuffer.Types where

import Data.Proxy
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Texture.Types

-- | A GPU object that can be used to retrieve data from a 'GBuffer'.
data GBufferSampler o = FragmentShaderOutput o => GBufferSampler [GSampler2D]

-- | A GPU object that can be used to retrieve data from a 'DepthBuffer'.
newtype DepthBufferSampler = DepthBufferSampler GSampler2D

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

data OutBuffer o where
        TextureFloatGBuffer             :: FragmentShaderOutput o
                                        => Int
                                        -> Int
                                        -> [LoadedTexture]
                                        -> OutBuffer o

        TextureByteGBuffer              :: FragmentShaderOutput o
                                        => Int
                                        -> Int
                                        -> [LoadedTexture]
                                        -> OutBuffer o

        TextureDepthBuffer              :: Int
                                        -> Int
                                        -> LoadedTexture
                                        -> OutBuffer OutDepthBuffer

        TextureDepthStencilBuffer       :: Int
                                        -> Int
                                        -> LoadedTexture
                                        -> OutBuffer OutDepthBuffer

-- | A 'GBuffer' and a 'DepthBuffer' of the same size.
data BufferPair o = BufferPair (GBuffer o) DepthBuffer

instance FragmentShaderOutput o => MultiShaderType (GBufferSampler o) where
        type ExprMST (GBufferSampler o) = [ExprMST GSampler2D]
        mapMST f (GBufferSampler x) = GBufferSampler $ map f x
        toExprMST (GBufferSampler x) = map toExprMST x
        fromExprMST = GBufferSampler . map fromExprMST

instance FragmentShaderOutput o => ShaderInput (GBufferSampler o) where
        buildMST f i = (GBufferSampler $ take n infiniteSamplers, i + n)
                where n = textureCount (Proxy :: Proxy o)
                      infiniteSamplers = map f [i ..]
        foldrMST f s (GBufferSampler x) = foldr f s x

instance FragmentShaderOutput o => Uniform (GBufferSampler o) where
        type CPUUniform (GBufferSampler o) = GBuffer o
        foldrUniform _ f s buf =
                foldr (\u s -> f (UniformTexture $ TextureLoaded u) s)
                      s (textures buf)

instance MultiShaderType DepthBufferSampler where
        type ExprMST DepthBufferSampler = ExprMST GSampler2D
        mapMST f (DepthBufferSampler x) = DepthBufferSampler $ f x
        toExprMST (DepthBufferSampler x) = toExprMST x
        fromExprMST = DepthBufferSampler . fromExprMST

instance ShaderInput DepthBufferSampler where
        buildMST f i = (DepthBufferSampler $ f i, i + 1)
        foldrMST f s (DepthBufferSampler x) = foldrMST f s x

instance Uniform DepthBufferSampler where
        type CPUUniform DepthBufferSampler = DepthBuffer
        foldrUniform _ f s buf =
                f (UniformTexture . TextureLoaded . head . textures $ buf) s

-- | A container that can be used to store the output of some drawing operation.
type GBuffer = OutBuffer
-- | A container for depth/stencil values.
type DepthBuffer = OutBuffer OutDepthBuffer

type GBufferInfo = OutBufferInfo
type DepthBufferInfo = OutBufferInfo OutDepthBuffer

textures :: OutBuffer o -> [LoadedTexture]
textures (TextureFloatGBuffer _ _ ts) = ts
textures (TextureByteGBuffer _ _ ts) = ts
textures (TextureDepthBuffer _ _ t) = [t]
textures (TextureDepthStencilBuffer _ _ t) = [t]

bufferSize :: OutBuffer o -> (Int, Int)
bufferSize (TextureFloatGBuffer w h _) = (w, h)
bufferSize (TextureByteGBuffer w h _) = (w, h)
bufferSize (TextureDepthBuffer w h _) = (w, h)
bufferSize (TextureDepthStencilBuffer w h _) = (w, h)
