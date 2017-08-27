{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Graphics.Rendering.Ombra.Backend.WebGL.Types (
        Ctx,
        Program,
        Shader,
        Buffer,
        FrameBuffer,
        RenderBuffer,
        VertexArrayObject,
        Texture,
        UniformLocation,
        ActiveInfo,
        ShaderPrecisionFormat,
        getCtx,
        noBuffer,
        noFramebuffer,
        noTexture,
        noVAO,
) where

import Data.Int (Int32)
import Data.Word (Word8, Word16)
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Types
import JavaScript.Array
import JavaScript.TypedArray

type Ctx = JSVal

type Program = JSVal

type Shader = JSVal

type Buffer = JSVal

type FrameBuffer = JSVal

type RenderBuffer = JSVal

type VertexArrayObject = JSVal

type Texture = JSVal

{-
instance Eq Texture where
        (==) = eqRef
-}

type UniformLocation = JSVal

type ActiveInfo = JSVal

type ShaderPrecisionFormat = JSVal

-- type ArrayBufferView = JSVal

noBuffer :: Buffer
noBuffer = jsNull

noFramebuffer :: FrameBuffer
noFramebuffer = jsNull

noTexture :: Texture
noTexture = jsNull

noVAO :: VertexArrayObject
noVAO = jsNull

foreign import javascript unsafe "$r = $1.getContext(\"webgl\");"
        getCtx :: JSRef a -> IO Ctx
