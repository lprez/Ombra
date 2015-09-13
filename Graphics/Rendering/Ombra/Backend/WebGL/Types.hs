{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Graphics.Rendering.Ombra.Backend.WebGL.Types (
        Ctx,
        Float32Array,
        Int32Array,
        Uint16Array,
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
        ArrayBufferView,
        getCtx,
        float32Array,
        int32Array,
        uint16Array,
        uint8Array,
        uint8ArraySize,
        float32View,
        int32View,
        uint16View,
        uint8View,
        toJSArray,
        listToJSArray,
        noBuffer,
        noTexture,
        noVAO,
        noArray
) where

import Data.Int (Int32)
import Data.Word (Word8, Word16)
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Types

data Ctx_
type Ctx = JSRef Ctx_

data Float32Array_
type Float32Array = JSRef Float32Array_

data Int32Array_
type Int32Array = JSRef Int32Array_

data Uint16Array_
type Uint16Array = JSRef Uint16Array_

data Uint8Array_
type Uint8Array = JSRef Uint8Array_

data Program_
type Program = JSRef Program_

data Shader_
type Shader = JSRef Shader_

data Buffer_
type Buffer = JSRef Buffer_

data FrameBuffer_
type FrameBuffer = JSRef FrameBuffer_

data RenderBuffer_
type RenderBuffer = JSRef RenderBuffer_

data VertexArrayObject_
type VertexArrayObject = JSRef VertexArrayObject_

data Texture_
type Texture = JSRef Texture_

instance Eq Texture where
        (==) = eqRef

data UniformLocation_
type UniformLocation = JSRef UniformLocation_

data ActiveInfo_
type ActiveInfo = JSRef ActiveInfo_

data ShaderPrecisionFormat_
type ShaderPrecisionFormat = JSRef ShaderPrecisionFormat_

data ArrayBufferView_
type ArrayBufferView = JSRef ArrayBufferView_

noBuffer :: Buffer
noBuffer = jsNull

noTexture :: Texture
noTexture = jsNull

noVAO :: VertexArrayObject
noVAO = jsNull

noArray :: IO ArrayBufferView
noArray = return jsNull

float32View :: JSArray Float -> IO ArrayBufferView
float32View = fmap castRef . float32Array

int32View :: JSArray Int32 -> IO ArrayBufferView
int32View = fmap castRef . int32Array

uint16View :: JSArray Word16 -> IO ArrayBufferView
uint16View = fmap castRef . uint16Array

uint8View :: JSArray Word8 -> IO ArrayBufferView
uint8View = fmap castRef . uint8Array

toJSArray :: ToJSRef a => (v -> Maybe (a, v)) -> v -> IO (JSArray a)
toJSArray next iv = newArray >>= iterPush iv
        where iterPush v arr = case next v of
                                        Just (x, v') -> do xRef <- toJSRef x
                                                           pushArray xRef arr
                                                           iterPush v' arr
                                        Nothing -> return arr

listToJSArray :: ToJSRef a => [a] -> IO (JSArray a)
listToJSArray = toJSArray deconstr
        where deconstr (x : xs) = Just (x, xs)
              deconstr [] = Nothing

foreign import javascript unsafe "$r = new Float32Array($1);"
        float32Array :: JSArray Float -> IO Float32Array

foreign import javascript unsafe "$r = new Int32Array($1);"
        int32Array :: JSArray Int32 -> IO Int32Array

foreign import javascript unsafe "$r = new Uint16Array($1);"
        uint16Array :: JSArray Word16 -> IO Uint16Array

foreign import javascript unsafe "$r = new Uint8Array($1);"
        uint8Array :: JSArray Word8 -> IO Uint8Array

foreign import javascript unsafe "$r = new Uint8Array($1);"
        uint8ArraySize :: Int -> IO Uint8Array

foreign import javascript unsafe "$r = $1.getContext(\"webgl\");"
        getCtx :: JSRef a -> IO Ctx
