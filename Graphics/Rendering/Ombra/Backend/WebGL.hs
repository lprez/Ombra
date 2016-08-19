{-# LANGUAGE MultiParamTypeClasses, TypeFamilies,
             UndecidableInstances, OverloadedStrings #-}

{-| The GHCJS/WebGL backend. This just exports the instance for 'GLES'. -}
module Graphics.Rendering.Ombra.Backend.WebGL (
        makeContext
) where

import Control.Applicative
import Control.Concurrent
import Data.Coerce
import Data.Maybe
import qualified Data.HashMap.Strict as H
import Data.Int (Int32)
import Data.IORef
import Data.List (unfoldr)
import Data.JSString (JSString, pack)
import Data.Vect.Float
import Data.Word
import Graphics.Rendering.Ombra.Backend
import qualified Graphics.Rendering.Ombra.Backend.WebGL.Const as JS
import qualified Graphics.Rendering.Ombra.Backend.WebGL.Raw as JS
import qualified Graphics.Rendering.Ombra.Backend.WebGL.Types as JS
import Graphics.Rendering.Ombra.Color
import GHCJS.Foreign hiding (Object)
import GHCJS.Types
import GHCJS.Marshal
import qualified JavaScript.Array as JSArray
import JavaScript.Object hiding (create)
import JavaScript.Object.Internal (Object(..))
import JavaScript.TypedArray hiding (Float32Array, Int32Array)
import qualified JavaScript.TypedArray as JS
import qualified JavaScript.TypedArray.ArrayBuffer as JS
import qualified JavaScript.TypedArray.Internal as JS
import qualified JavaScript.TypedArray.DataView as JSDataView

-- TODO: ??? 
foreign import javascript unsafe "eval('null')" nullUInt8Array :: IO UInt8Array

data TagTex = TagTex Int JS.Texture

instance Eq TagTex where
        TagTex t _ == TagTex t' _ = t == t'

makeContext :: JSVal -- ^ Canvas element.
            -> IO Ctx
makeContext element = do ctx <- JS.getCtx element
                         counter <- newIORef 0
                         JS.getExtension ctx "WEBGL_depth_texture"
                         JS.getExtension ctx "WEBGL_color_buffer_float"
                         vaoExt <- JS.getExtension ctx "OES_vertex_array_object"
                         drawBufsExt <- JS.getExtension ctx "WEBGL_draw_buffers"
                         setProp "vaoExt" vaoExt $ Object ctx
                         setProp "drawBufs" drawBufsExt $ Object ctx
                         return (counter, ctx)

toJSArray :: ToJSVal a => (v -> Maybe (a, v)) -> v -> IO JSArray.JSArray
toJSArray next iv = JSArray.fromList <$> mapM toJSVal list
        where list = unfoldr next iv
        {- 
        JSArray.create >>= iterPush iv
        where iterPush v arr = case next v of
                                        Just (x, v') -> do xRef <- toJSVal x
                                                           JSArray.push xRef arr
                                                           iterPush v' arr
                                        Nothing -> return arr
        -}

instance GLES where
        type Ctx = (IORef Int, JS.Ctx)
        type GLEnum = Word
        type GLUInt = Word
        type GLInt = Int32
        type GLPtr = Word
        type GLPtrDiff = Word
        type GLSize = Int32
        type GLString = JSString
        type GLBool = Bool
        type Buffer = JS.Buffer
        type UniformLocation = JS.UniformLocation
        type Texture = TagTex
        type Shader = JS.Shader
        type Program = JS.Program
        type FrameBuffer = JS.FrameBuffer
        type RenderBuffer = JS.RenderBuffer
        type VertexArrayObject = JS.VertexArrayObject
        -- type ActiveInfo = JS.ActiveInfo
        -- type ShaderPrecisionFormat = JS.ShaderPrecisionFormat
        type AnyArray = JS.ArrayBuffer
        type Float32Array = JS.Float32Array
        type Int32Array = JS.Int32Array
        type UInt8Array = JS.Uint8Array
        type UInt16Array = JS.Uint16Array

        true = True
        false = False
        nullGLPtr = 0
        toGLString = pack
        noBuffer = JS.noBuffer
        noTexture = TagTex (-1) JS.noTexture
        noUInt8Array = nullUInt8Array
        noVAO = JS.noVAO

        encodeMat2 (Mat2 (Vec2 a1 a2) (Vec2 b1 b2)) =
                JSArray.fromList <$> mapM toJSVal [a1, a2, b1, b2]
                >>= JS.float32ArrayFrom

        encodeMat3 (Mat3 (Vec3 a1 a2 a3)
                         (Vec3 b1 b2 b3)
                         (Vec3 c1 c2 c3)) = JSArray.fromList <$> mapM toJSVal
                                                [ a1, a2, a3
                                                , b1, b2, b3
                                                , c1, c2, c3 ]
                                                >>= JS.float32ArrayFrom
        encodeMat4 (Mat4 (Vec4 a1 a2 a3 a4)
                         (Vec4 b1 b2 b3 b4)
                         (Vec4 c1 c2 c3 c4)
                         (Vec4 d1 d2 d3 d4) ) =
                        JSArray.fromList <$> mapM toJSVal [ a1, a2, a3, a4
                                                          , b1, b2, b3, b4
                                                          , c1, c2, c3, c4
                                                          , d1, d2, d3, d4 ]
                                                        >>= JS.float32ArrayFrom
        encodeFloats v = JSArray.fromList <$> mapM toJSVal v
                         >>= JS.float32ArrayFrom
        encodeInts v = JSArray.fromList <$> mapM toJSVal v
                       >>= JS.int32ArrayFrom

        -- TODO: decent implementation
        encodeVec2s v = toJSArray next (False, v) >>= JS.float32ArrayFrom
                where next (False, xs@(Vec2 x _ : _)) = Just (x, (True, xs))
                      next (True, Vec2 _ y : xs) = Just (y, (False, xs))
                      next (_, []) = Nothing

        encodeVec3s v = toJSArray next (0, v) >>= JS.float32ArrayFrom
                where next (0, xs@(Vec3 x _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(Vec3 _ y _ : _)) = Just (y, (2, xs))
                      next (2, Vec3 _ _ z : xs) = Just (z, (0, xs))
                      next (_, []) = Nothing

        encodeVec4s v = toJSArray next (0, v) >>= JS.float32ArrayFrom
                where next (0, xs@(Vec4 x _ _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(Vec4 _ y _ _ : _)) = Just (y, (2, xs))
                      next (2, xs@(Vec4 _ _ z _ : _)) = Just (z, (3, xs))
                      next (3, Vec4 _ _ _ w : xs) = Just (w, (0, xs))
                      next (_, []) = Nothing

        encodeIVec2s v = toJSArray next (False, v) >>= JS.int32ArrayFrom
                where next (False, xs@(IVec2 x _ : _)) = Just (x, (True, xs))
                      next (True, IVec2 _ y : xs) = Just (y, (False, xs))
                      next (_, []) = Nothing

        encodeIVec3s v = toJSArray next (0, v) >>= JS.int32ArrayFrom
                where next (0, xs@(IVec3 x _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(IVec3 _ y _ : _)) = Just (y, (2, xs))
                      next (2, IVec3 _ _ z : xs) = Just (z, (0, xs))
                      next (_, []) = Nothing

        encodeIVec4s v = toJSArray next (0, v) >>= JS.int32ArrayFrom
                where next (0, xs@(IVec4 x _ _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(IVec4 _ y _ _ : _)) = Just (y, (2, xs))
                      next (2, xs@(IVec4 _ _ z _ : _)) = Just (z, (3, xs))
                      next (3, IVec4 _ _ _ w : xs) = Just (w, (0, xs))
                      next (_, []) = Nothing

        encodeUShorts v = JSArray.fromList <$> mapM toJSVal v
                          >>= JS.uint16ArrayFrom

        encodeUInt8s v = JSArray.fromList <$> mapM toJSVal v
                         >>= JS.uint8ArrayFrom

{-
        encodeColors v = toJSArray next (0, v) >>= JS.uint8ArrayFrom
                where next (0, xs@(Color x _ _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(Color _ y _ _ : _)) = Just (y, (2, xs))
                      next (2, xs@(Color _ _ z _ : _)) = Just (z, (3, xs))
                      next (3, Color _ _ _ w : xs) = Just (w, (0, xs))
                      next (_, []) = Nothing
-}

        -- !
        newByteArray l = do arr <- JSArray.create
                            _ <- sequence . replicate l $
                                    toJSVal (0 :: Word) >>=
                                    flip JSArray.push arr
                            JSArray.unsafeFreeze arr >>= JS.uint8ArrayFrom
        fromFloat32Array = JS.buffer
        fromInt32Array = JS.buffer
        fromUInt8Array = JS.buffer
        fromUInt16Array = JS.buffer
        decodeBytes ar = let dw = JSDataView.dataView $ JS.buffer ar
                         in return $ map (flip JSDataView.getUint8 dw)
                                         [0 .. JS.length ar - 1]

        glActiveTexture = JS.glActiveTexture . snd
        glAttachShader = JS.glAttachShader . snd
        glBindAttribLocation = JS.glBindAttribLocation . snd
        glBindBuffer = JS.glBindBuffer . snd
        glBindFramebuffer = JS.glBindFramebuffer . snd
        glBindRenderbuffer = JS.glBindRenderbuffer . snd
        glBindTexture (_, c) e (TagTex _ t) = JS.glBindTexture c e t
        glBindVertexArray = JS.glBindVertexArrayOES . snd
        glBlendColor = JS.glBlendColor . snd
        glBlendEquation = JS.glBlendEquation . snd
        glBlendEquationSeparate = JS.glBlendEquationSeparate . snd
        glBlendFunc = JS.glBlendFunc . snd
        glBlendFuncSeparate = JS.glBlendFuncSeparate . snd
        glBufferData = JS.glBufferData . snd
        glBufferSubData = JS.glBufferSubData . snd
        glCheckFramebufferStatus = JS.glCheckFramebufferStatus . snd
        glClear = JS.glClear . snd
        glClearColor = JS.glClearColor . snd
        glClearDepth = JS.glClearDepth . snd
        glClearStencil = JS.glClearStencil . snd
        glColorMask = JS.glColorMask . snd
        glCompileShader = JS.glCompileShader . snd
        glCompressedTexImage2D = JS.glCompressedTexImage2D . snd
        glCompressedTexSubImage2D = JS.glCompressedTexSubImage2D . snd
        glCopyTexImage2D = JS.glCopyTexImage2D . snd
        glCopyTexSubImage2D = JS.glCopyTexSubImage2D . snd
        glCreateBuffer = JS.glCreateBuffer . snd
        glCreateFramebuffer = JS.glCreateFramebuffer . snd
        glCreateProgram = JS.glCreateProgram . snd
        glCreateRenderbuffer = JS.glCreateRenderbuffer . snd
        glCreateShader = JS.glCreateShader . snd
        glCreateTexture (r, c) = do t <- JS.glCreateTexture c
                                    n <- atomicModifyIORef' r $ \n -> (n + 1, n)
                                    return $ TagTex n t
        glCreateVertexArray = JS.glCreateVertexArrayOES . snd
        glCullFace = JS.glCullFace . snd
        glDeleteBuffer = JS.glDeleteBuffer . snd
        glDeleteFramebuffer = JS.glDeleteFramebuffer . snd
        glDeleteProgram = JS.glDeleteProgram . snd
        glDeleteRenderbuffer = JS.glDeleteRenderbuffer . snd
        glDeleteShader = JS.glDeleteShader . snd
        glDeleteTexture (_, c) (TagTex _ t) = JS.glDeleteTexture c t
        glDeleteVertexArray = JS.glDeleteVertexArrayOES . snd
        glDepthFunc = JS.glDepthFunc . snd
        glDepthMask = JS.glDepthMask . snd
        glDepthRange = JS.glDepthRange . snd
        glDetachShader = JS.glDetachShader . snd
        glDisable = JS.glDisable . snd
        glDisableVertexAttribArray = JS.glDisableVertexAttribArray . snd
        glDrawArrays = JS.glDrawArrays . snd
        glDrawBuffers = JS.glDrawBuffersWEBGL . snd
        glDrawElements = JS.glDrawElements . snd
        glEnable = JS.glEnable . snd
        glEnableVertexAttribArray = JS.glEnableVertexAttribArray . snd
        glFinish = JS.glFinish . snd
        glFlush = JS.glFlush . snd
        glFramebufferRenderbuffer = JS.glFramebufferRenderbuffer . snd
        glFramebufferTexture2D (_, ctx) a b c (TagTex _ t) d =
                JS.glFramebufferTexture2D ctx a b c t d
        glFrontFace = JS.glFrontFace . snd
        glGenerateMipmap = JS.glGenerateMipmap . snd
        -- glGetActiveAttrib = JS.glGetActiveAttrib . snd
        -- glGetActiveUniform = JS.glGetActiveUniform . snd
        glGetAttribLocation = JS.glGetAttribLocation . snd
        -- glGetBufferParameter = JS.glGetBufferParameter . snd
        -- glGetParameter = JS.glGetParameter . snd
        glGetError = JS.glGetError . snd
        -- glGetFramebufferAttachmentParameter = JS.glGetFramebufferAttachmentParameter . snd
        glGetProgramInfoLog = JS.glGetProgramInfoLog . snd
        -- glGetRenderbufferParameter = JS.glGetRenderbufferParameter . snd
        -- glGetShaderParameter = JS.glGetShaderParameter . snd
        -- glGetShaderPrecisionFormat = JS.glGetShaderPrecisionFormat . snd
        glGetShaderInfoLog = JS.glGetShaderInfoLog . snd
        glGetShaderSource = JS.glGetShaderSource . snd
        -- glGetTexParameter = JS.glGetTexParameter . snd
        -- glGetUniform = JS.glGetUniform . snd
        glGetUniformLocation = JS.glGetUniformLocation . snd
        -- glGetVertexAttrib = JS.glGetVertexAttrib . snd
        -- glGetVertexAttribOffset = JS.glGetVertexAttribOffset . snd
        glHint = JS.glHint . snd
        glIsBuffer = JS.glIsBuffer . snd
        glIsEnabled = JS.glIsEnabled . snd
        glIsFramebuffer = JS.glIsFramebuffer . snd
        glIsProgram = JS.glIsProgram . snd
        glIsRenderbuffer = JS.glIsRenderbuffer . snd
        glIsShader = JS.glIsShader . snd
        glIsTexture (_, c) (TagTex _ t) = JS.glIsTexture c t
        glIsVertexArray = JS.glIsVertexArrayOES . snd
        glLineWidth = JS.glLineWidth . snd
        glLinkProgram = JS.glLinkProgram . snd
        glPixelStorei = JS.glPixelStorei . snd
        glPolygonOffset = JS.glPolygonOffset . snd
        glReadPixels = JS.glReadPixels . snd
        glRenderbufferStorage = JS.glRenderbufferStorage . snd
        glSampleCoverage = JS.glSampleCoverage . snd
        glScissor = JS.glScissor . snd
        glShaderSource = JS.glShaderSource . snd
        glStencilFunc = JS.glStencilFunc . snd
        glStencilFuncSeparate = JS.glStencilFuncSeparate . snd
        glStencilMask = JS.glStencilMask . snd
        glStencilMaskSeparate = JS.glStencilMaskSeparate . snd
        glStencilOp = JS.glStencilOp . snd
        glStencilOpSeparate = JS.glStencilOpSeparate . snd
        glTexImage2D = JS.glTexImage2D . snd
        glTexParameterf = JS.glTexParameterf . snd
        glTexParameteri = JS.glTexParameteri . snd
        glTexSubImage2D = JS.glTexSubImage2D . snd
        glUniform1f = JS.glUniform1f . snd
        glUniform1fv = JS.glUniform1fv . snd
        glUniform1i = JS.glUniform1i . snd
        glUniform1iv = JS.glUniform1iv . snd
        glUniform2f = JS.glUniform2f . snd
        glUniform2fv = JS.glUniform2fv . snd
        glUniform2i = JS.glUniform2i . snd
        glUniform2iv = JS.glUniform2iv . snd
        glUniform3f = JS.glUniform3f . snd
        glUniform3fv = JS.glUniform3fv . snd
        glUniform3i = JS.glUniform3i . snd
        glUniform3iv = JS.glUniform3iv . snd
        glUniform4f = JS.glUniform4f . snd
        glUniform4fv = JS.glUniform4fv . snd
        glUniform4i = JS.glUniform4i . snd
        glUniform4iv = JS.glUniform4iv . snd
        -- XXX
        glUniformMatrix2fv (_, c) loc _ arr = JS.glUniformMatrix2fv c loc False arr
        glUniformMatrix3fv (_, c) loc _ arr = JS.glUniformMatrix3fv c loc False arr
        glUniformMatrix4fv (_, c) loc _ arr = JS.glUniformMatrix4fv c loc False arr
        glUseProgram = JS.glUseProgram . snd
        glValidateProgram = JS.glValidateProgram . snd
        glVertexAttrib1f = JS.glVertexAttrib1f . snd
        glVertexAttrib1fv = JS.glVertexAttrib1fv . snd
        glVertexAttrib2f = JS.glVertexAttrib2f . snd
        glVertexAttrib2fv = JS.glVertexAttrib2fv . snd
        glVertexAttrib3f = JS.glVertexAttrib3f . snd
        glVertexAttrib3fv = JS.glVertexAttrib3fv . snd
        glVertexAttrib4f = JS.glVertexAttrib4f . snd
        glVertexAttrib4fv = JS.glVertexAttrib4fv . snd
        glVertexAttribPointer = JS.glVertexAttribPointer . snd
        glViewport = JS.glViewport . snd

        gl_DEPTH_BUFFER_BIT = JS.gl_DEPTH_BUFFER_BIT
        gl_STENCIL_BUFFER_BIT = JS.gl_STENCIL_BUFFER_BIT
        gl_COLOR_BUFFER_BIT = JS.gl_COLOR_BUFFER_BIT
        gl_POINTS = JS.gl_POINTS
        gl_LINES = JS.gl_LINES
        gl_LINE_LOOP = JS.gl_LINE_LOOP
        gl_LINE_STRIP = JS.gl_LINE_STRIP
        gl_TRIANGLES = JS.gl_TRIANGLES
        gl_TRIANGLE_STRIP = JS.gl_TRIANGLE_STRIP
        gl_TRIANGLE_FAN = JS.gl_TRIANGLE_FAN
        gl_ZERO = JS.gl_ZERO
        gl_ONE = JS.gl_ONE
        gl_SRC_COLOR = JS.gl_SRC_COLOR
        gl_ONE_MINUS_SRC_COLOR = JS.gl_ONE_MINUS_SRC_COLOR
        gl_SRC_ALPHA = JS.gl_SRC_ALPHA
        gl_ONE_MINUS_SRC_ALPHA = JS.gl_ONE_MINUS_SRC_ALPHA
        gl_DST_ALPHA = JS.gl_DST_ALPHA
        gl_ONE_MINUS_DST_ALPHA = JS.gl_ONE_MINUS_DST_ALPHA
        gl_DST_COLOR = JS.gl_DST_COLOR
        gl_ONE_MINUS_DST_COLOR = JS.gl_ONE_MINUS_DST_COLOR
        gl_SRC_ALPHA_SATURATE = JS.gl_SRC_ALPHA_SATURATE
        gl_FUNC_ADD = JS.gl_FUNC_ADD
        gl_BLEND_EQUATION = JS.gl_BLEND_EQUATION
        gl_BLEND_EQUATION_RGB = JS.gl_BLEND_EQUATION_RGB
        gl_BLEND_EQUATION_ALPHA = JS.gl_BLEND_EQUATION_ALPHA
        gl_FUNC_SUBTRACT = JS.gl_FUNC_SUBTRACT
        gl_FUNC_REVERSE_SUBTRACT = JS.gl_FUNC_REVERSE_SUBTRACT
        gl_BLEND_DST_RGB = JS.gl_BLEND_DST_RGB
        gl_BLEND_SRC_RGB = JS.gl_BLEND_SRC_RGB
        gl_BLEND_DST_ALPHA = JS.gl_BLEND_DST_ALPHA
        gl_BLEND_SRC_ALPHA = JS.gl_BLEND_SRC_ALPHA
        gl_CONSTANT_COLOR = JS.gl_CONSTANT_COLOR
        gl_ONE_MINUS_CONSTANT_COLOR = JS.gl_ONE_MINUS_CONSTANT_COLOR
        gl_CONSTANT_ALPHA = JS.gl_CONSTANT_ALPHA
        gl_ONE_MINUS_CONSTANT_ALPHA = JS.gl_ONE_MINUS_CONSTANT_ALPHA
        gl_BLEND_COLOR = JS.gl_BLEND_COLOR
        gl_ARRAY_BUFFER = JS.gl_ARRAY_BUFFER
        gl_ELEMENT_ARRAY_BUFFER = JS.gl_ELEMENT_ARRAY_BUFFER
        gl_ARRAY_BUFFER_BINDING = JS.gl_ARRAY_BUFFER_BINDING
        gl_ELEMENT_ARRAY_BUFFER_BINDING = JS.gl_ELEMENT_ARRAY_BUFFER_BINDING
        gl_STREAM_DRAW = JS.gl_STREAM_DRAW
        gl_STATIC_DRAW = JS.gl_STATIC_DRAW
        gl_DYNAMIC_DRAW = JS.gl_DYNAMIC_DRAW
        gl_BUFFER_SIZE = JS.gl_BUFFER_SIZE
        gl_BUFFER_USAGE = JS.gl_BUFFER_USAGE
        gl_CURRENT_VERTEX_ATTRIB = JS.gl_CURRENT_VERTEX_ATTRIB
        gl_FRONT = JS.gl_FRONT
        gl_BACK = JS.gl_BACK
        gl_FRONT_AND_BACK = JS.gl_FRONT_AND_BACK
        gl_CULL_FACE = JS.gl_CULL_FACE
        gl_BLEND = JS.gl_BLEND
        gl_DITHER = JS.gl_DITHER
        gl_STENCIL_TEST = JS.gl_STENCIL_TEST
        gl_DEPTH_TEST = JS.gl_DEPTH_TEST
        gl_SCISSOR_TEST = JS.gl_SCISSOR_TEST
        gl_POLYGON_OFFSET_FILL = JS.gl_POLYGON_OFFSET_FILL
        gl_SAMPLE_ALPHA_TO_COVERAGE = JS.gl_SAMPLE_ALPHA_TO_COVERAGE
        gl_SAMPLE_COVERAGE = JS.gl_SAMPLE_COVERAGE
        gl_NO_ERROR = JS.gl_NO_ERROR
        gl_INVALID_ENUM = JS.gl_INVALID_ENUM
        gl_INVALID_VALUE = JS.gl_INVALID_VALUE
        gl_INVALID_OPERATION = JS.gl_INVALID_OPERATION
        gl_OUT_OF_MEMORY = JS.gl_OUT_OF_MEMORY
        gl_CW = JS.gl_CW
        gl_CCW = JS.gl_CCW
        gl_LINE_WIDTH = JS.gl_LINE_WIDTH
        gl_ALIASED_POINT_SIZE_RANGE = JS.gl_ALIASED_POINT_SIZE_RANGE
        gl_ALIASED_LINE_WIDTH_RANGE = JS.gl_ALIASED_LINE_WIDTH_RANGE
        gl_CULL_FACE_MODE = JS.gl_CULL_FACE_MODE
        gl_FRONT_FACE = JS.gl_FRONT_FACE
        gl_DEPTH_RANGE = JS.gl_DEPTH_RANGE
        gl_DEPTH_WRITEMASK = JS.gl_DEPTH_WRITEMASK
        gl_DEPTH_CLEAR_VALUE = JS.gl_DEPTH_CLEAR_VALUE
        gl_DEPTH_FUNC = JS.gl_DEPTH_FUNC
        gl_STENCIL_CLEAR_VALUE = JS.gl_STENCIL_CLEAR_VALUE
        gl_STENCIL_FUNC = JS.gl_STENCIL_FUNC
        gl_STENCIL_FAIL = JS.gl_STENCIL_FAIL
        gl_STENCIL_PASS_DEPTH_FAIL = JS.gl_STENCIL_PASS_DEPTH_FAIL
        gl_STENCIL_PASS_DEPTH_PASS = JS.gl_STENCIL_PASS_DEPTH_PASS
        gl_STENCIL_REF = JS.gl_STENCIL_REF
        gl_STENCIL_VALUE_MASK = JS.gl_STENCIL_VALUE_MASK
        gl_STENCIL_WRITEMASK = JS.gl_STENCIL_WRITEMASK
        gl_STENCIL_BACK_FUNC = JS.gl_STENCIL_BACK_FUNC
        gl_STENCIL_BACK_FAIL = JS.gl_STENCIL_BACK_FAIL
        gl_STENCIL_BACK_PASS_DEPTH_FAIL = JS.gl_STENCIL_BACK_PASS_DEPTH_FAIL
        gl_STENCIL_BACK_PASS_DEPTH_PASS = JS.gl_STENCIL_BACK_PASS_DEPTH_PASS
        gl_STENCIL_BACK_REF = JS.gl_STENCIL_BACK_REF
        gl_STENCIL_BACK_VALUE_MASK = JS.gl_STENCIL_BACK_VALUE_MASK
        gl_STENCIL_BACK_WRITEMASK = JS.gl_STENCIL_BACK_WRITEMASK
        gl_VIEWPORT = JS.gl_VIEWPORT
        gl_SCISSOR_BOX = JS.gl_SCISSOR_BOX
        gl_COLOR_CLEAR_VALUE = JS.gl_COLOR_CLEAR_VALUE
        gl_COLOR_WRITEMASK = JS.gl_COLOR_WRITEMASK
        gl_UNPACK_ALIGNMENT = JS.gl_UNPACK_ALIGNMENT
        gl_PACK_ALIGNMENT = JS.gl_PACK_ALIGNMENT
        gl_MAX_TEXTURE_SIZE = JS.gl_MAX_TEXTURE_SIZE
        gl_MAX_VIEWPORT_DIMS = JS.gl_MAX_VIEWPORT_DIMS
        gl_SUBPIXEL_BITS = JS.gl_SUBPIXEL_BITS
        gl_RED_BITS = JS.gl_RED_BITS
        gl_GREEN_BITS = JS.gl_GREEN_BITS
        gl_BLUE_BITS = JS.gl_BLUE_BITS
        gl_ALPHA_BITS = JS.gl_ALPHA_BITS
        gl_DEPTH_BITS = JS.gl_DEPTH_BITS
        gl_STENCIL_BITS = JS.gl_STENCIL_BITS
        gl_POLYGON_OFFSET_UNITS = JS.gl_POLYGON_OFFSET_UNITS
        gl_POLYGON_OFFSET_FACTOR = JS.gl_POLYGON_OFFSET_FACTOR
        gl_TEXTURE_BINDING_2D = JS.gl_TEXTURE_BINDING_2D
        gl_SAMPLE_BUFFERS = JS.gl_SAMPLE_BUFFERS
        gl_SAMPLES = JS.gl_SAMPLES
        gl_SAMPLE_COVERAGE_VALUE = JS.gl_SAMPLE_COVERAGE_VALUE
        gl_SAMPLE_COVERAGE_INVERT = JS.gl_SAMPLE_COVERAGE_INVERT
        gl_COMPRESSED_TEXTURE_FORMATS = JS.gl_COMPRESSED_TEXTURE_FORMATS
        gl_DONT_CARE = JS.gl_DONT_CARE
        gl_FASTEST = JS.gl_FASTEST
        gl_NICEST = JS.gl_NICEST
        gl_GENERATE_MIPMAP_HINT = JS.gl_GENERATE_MIPMAP_HINT
        gl_BYTE = JS.gl_BYTE
        gl_UNSIGNED_BYTE = JS.gl_UNSIGNED_BYTE
        gl_SHORT = JS.gl_SHORT
        gl_UNSIGNED_SHORT = JS.gl_UNSIGNED_SHORT
        gl_INT = JS.gl_INT
        gl_UNSIGNED_INT = JS.gl_UNSIGNED_INT
        gl_FLOAT = JS.gl_FLOAT
        gl_DEPTH_COMPONENT = JS.gl_DEPTH_COMPONENT
        gl_ALPHA = JS.gl_ALPHA
        gl_RGB = JS.gl_RGB
        gl_RGBA = JS.gl_RGBA
        gl_RGBA32F = JS.gl_RGBA32F_EXT
        gl_LUMINANCE = JS.gl_LUMINANCE
        gl_LUMINANCE_ALPHA = JS.gl_LUMINANCE_ALPHA
        gl_UNSIGNED_SHORT_4_4_4_4 = JS.gl_UNSIGNED_SHORT_4_4_4_4
        gl_UNSIGNED_SHORT_5_5_5_1 = JS.gl_UNSIGNED_SHORT_5_5_5_1
        gl_UNSIGNED_SHORT_5_6_5 = JS.gl_UNSIGNED_SHORT_5_6_5
        gl_FRAGMENT_SHADER = JS.gl_FRAGMENT_SHADER
        gl_VERTEX_SHADER = JS.gl_VERTEX_SHADER
        gl_MAX_VERTEX_ATTRIBS = JS.gl_MAX_VERTEX_ATTRIBS
        gl_MAX_VERTEX_UNIFORM_VECTORS = JS.gl_MAX_VERTEX_UNIFORM_VECTORS
        gl_MAX_VARYING_VECTORS = JS.gl_MAX_VARYING_VECTORS
        gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS = JS.gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
        gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS = JS.gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS
        gl_MAX_TEXTURE_IMAGE_UNITS = JS.gl_MAX_TEXTURE_IMAGE_UNITS
        gl_MAX_FRAGMENT_UNIFORM_VECTORS = JS.gl_MAX_FRAGMENT_UNIFORM_VECTORS
        gl_SHADER_TYPE = JS.gl_SHADER_TYPE
        gl_DELETE_STATUS = JS.gl_DELETE_STATUS
        gl_LINK_STATUS = JS.gl_LINK_STATUS
        gl_VALIDATE_STATUS = JS.gl_VALIDATE_STATUS
        gl_ATTACHED_SHADERS = JS.gl_ATTACHED_SHADERS
        gl_ACTIVE_UNIFORMS = JS.gl_ACTIVE_UNIFORMS
        gl_ACTIVE_ATTRIBUTES = JS.gl_ACTIVE_ATTRIBUTES
        gl_SHADING_LANGUAGE_VERSION = JS.gl_SHADING_LANGUAGE_VERSION
        gl_CURRENT_PROGRAM = JS.gl_CURRENT_PROGRAM
        gl_NEVER = JS.gl_NEVER
        gl_LESS = JS.gl_LESS
        gl_EQUAL = JS.gl_EQUAL
        gl_LEQUAL = JS.gl_LEQUAL
        gl_GREATER = JS.gl_GREATER
        gl_NOTEQUAL = JS.gl_NOTEQUAL
        gl_GEQUAL = JS.gl_GEQUAL
        gl_ALWAYS = JS.gl_ALWAYS
        gl_KEEP = JS.gl_KEEP
        gl_REPLACE = JS.gl_REPLACE
        gl_INCR = JS.gl_INCR
        gl_DECR = JS.gl_DECR
        gl_INVERT = JS.gl_INVERT
        gl_INCR_WRAP = JS.gl_INCR_WRAP
        gl_DECR_WRAP = JS.gl_DECR_WRAP
        gl_VENDOR = JS.gl_VENDOR
        gl_RENDERER = JS.gl_RENDERER
        gl_VERSION = JS.gl_VERSION
        gl_NEAREST = JS.gl_NEAREST
        gl_LINEAR = JS.gl_LINEAR
        gl_NEAREST_MIPMAP_NEAREST = JS.gl_NEAREST_MIPMAP_NEAREST
        gl_LINEAR_MIPMAP_NEAREST = JS.gl_LINEAR_MIPMAP_NEAREST
        gl_NEAREST_MIPMAP_LINEAR = JS.gl_NEAREST_MIPMAP_LINEAR
        gl_LINEAR_MIPMAP_LINEAR = JS.gl_LINEAR_MIPMAP_LINEAR
        gl_TEXTURE_MAG_FILTER = JS.gl_TEXTURE_MAG_FILTER
        gl_TEXTURE_MIN_FILTER = JS.gl_TEXTURE_MIN_FILTER
        gl_TEXTURE_WRAP_S = JS.gl_TEXTURE_WRAP_S
        gl_TEXTURE_WRAP_T = JS.gl_TEXTURE_WRAP_T
        gl_TEXTURE_2D = JS.gl_TEXTURE_2D
        gl_TEXTURE = JS.gl_TEXTURE
        gl_TEXTURE_CUBE_MAP = JS.gl_TEXTURE_CUBE_MAP
        gl_TEXTURE_BINDING_CUBE_MAP = JS.gl_TEXTURE_BINDING_CUBE_MAP
        gl_TEXTURE_CUBE_MAP_POSITIVE_X = JS.gl_TEXTURE_CUBE_MAP_POSITIVE_X
        gl_TEXTURE_CUBE_MAP_NEGATIVE_X = JS.gl_TEXTURE_CUBE_MAP_NEGATIVE_X
        gl_TEXTURE_CUBE_MAP_POSITIVE_Y = JS.gl_TEXTURE_CUBE_MAP_POSITIVE_Y
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Y = JS.gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
        gl_TEXTURE_CUBE_MAP_POSITIVE_Z = JS.gl_TEXTURE_CUBE_MAP_POSITIVE_Z
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Z = JS.gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
        gl_MAX_CUBE_MAP_TEXTURE_SIZE = JS.gl_MAX_CUBE_MAP_TEXTURE_SIZE
        gl_TEXTURE0 = JS.gl_TEXTURE0
        gl_TEXTURE1 = JS.gl_TEXTURE1
        gl_TEXTURE2 = JS.gl_TEXTURE2
        gl_TEXTURE3 = JS.gl_TEXTURE3
        gl_TEXTURE4 = JS.gl_TEXTURE4
        gl_TEXTURE5 = JS.gl_TEXTURE5
        gl_TEXTURE6 = JS.gl_TEXTURE6
        gl_TEXTURE7 = JS.gl_TEXTURE7
        gl_TEXTURE8 = JS.gl_TEXTURE8
        gl_TEXTURE9 = JS.gl_TEXTURE9
        gl_TEXTURE10 = JS.gl_TEXTURE10
        gl_TEXTURE11 = JS.gl_TEXTURE11
        gl_TEXTURE12 = JS.gl_TEXTURE12
        gl_TEXTURE13 = JS.gl_TEXTURE13
        gl_TEXTURE14 = JS.gl_TEXTURE14
        gl_TEXTURE15 = JS.gl_TEXTURE15
        gl_TEXTURE16 = JS.gl_TEXTURE16
        gl_TEXTURE17 = JS.gl_TEXTURE17
        gl_TEXTURE18 = JS.gl_TEXTURE18
        gl_TEXTURE19 = JS.gl_TEXTURE19
        gl_TEXTURE20 = JS.gl_TEXTURE20
        gl_TEXTURE21 = JS.gl_TEXTURE21
        gl_TEXTURE22 = JS.gl_TEXTURE22
        gl_TEXTURE23 = JS.gl_TEXTURE23
        gl_TEXTURE24 = JS.gl_TEXTURE24
        gl_TEXTURE25 = JS.gl_TEXTURE25
        gl_TEXTURE26 = JS.gl_TEXTURE26
        gl_TEXTURE27 = JS.gl_TEXTURE27
        gl_TEXTURE28 = JS.gl_TEXTURE28
        gl_TEXTURE29 = JS.gl_TEXTURE29
        gl_TEXTURE30 = JS.gl_TEXTURE30
        gl_TEXTURE31 = JS.gl_TEXTURE31
        gl_ACTIVE_TEXTURE = JS.gl_ACTIVE_TEXTURE
        gl_REPEAT = JS.gl_REPEAT
        gl_CLAMP_TO_EDGE = JS.gl_CLAMP_TO_EDGE
        gl_MIRRORED_REPEAT = JS.gl_MIRRORED_REPEAT
        gl_FLOAT_VEC2 = JS.gl_FLOAT_VEC2
        gl_FLOAT_VEC3 = JS.gl_FLOAT_VEC3
        gl_FLOAT_VEC4 = JS.gl_FLOAT_VEC4
        gl_INT_VEC2 = JS.gl_INT_VEC2
        gl_INT_VEC3 = JS.gl_INT_VEC3
        gl_INT_VEC4 = JS.gl_INT_VEC4
        gl_BOOL = JS.gl_BOOL
        gl_BOOL_VEC2 = JS.gl_BOOL_VEC2
        gl_BOOL_VEC3 = JS.gl_BOOL_VEC3
        gl_BOOL_VEC4 = JS.gl_BOOL_VEC4
        gl_FLOAT_MAT2 = JS.gl_FLOAT_MAT2
        gl_FLOAT_MAT3 = JS.gl_FLOAT_MAT3
        gl_FLOAT_MAT4 = JS.gl_FLOAT_MAT4
        gl_SAMPLER_2D = JS.gl_SAMPLER_2D
        gl_SAMPLER_CUBE = JS.gl_SAMPLER_CUBE
        gl_VERTEX_ATTRIB_ARRAY_ENABLED = JS.gl_VERTEX_ATTRIB_ARRAY_ENABLED
        gl_VERTEX_ATTRIB_ARRAY_SIZE = JS.gl_VERTEX_ATTRIB_ARRAY_SIZE
        gl_VERTEX_ATTRIB_ARRAY_STRIDE = JS.gl_VERTEX_ATTRIB_ARRAY_STRIDE
        gl_VERTEX_ATTRIB_ARRAY_TYPE = JS.gl_VERTEX_ATTRIB_ARRAY_TYPE
        gl_VERTEX_ATTRIB_ARRAY_NORMALIZED = JS.gl_VERTEX_ATTRIB_ARRAY_NORMALIZED
        gl_VERTEX_ATTRIB_ARRAY_POINTER = JS.gl_VERTEX_ATTRIB_ARRAY_POINTER
        gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = JS.gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
        gl_COMPILE_STATUS = JS.gl_COMPILE_STATUS
        gl_LOW_FLOAT = JS.gl_LOW_FLOAT
        gl_MEDIUM_FLOAT = JS.gl_MEDIUM_FLOAT
        gl_HIGH_FLOAT = JS.gl_HIGH_FLOAT
        gl_LOW_INT = JS.gl_LOW_INT
        gl_MEDIUM_INT = JS.gl_MEDIUM_INT
        gl_HIGH_INT = JS.gl_HIGH_INT
        gl_FRAMEBUFFER = JS.gl_FRAMEBUFFER
        gl_RENDERBUFFER = JS.gl_RENDERBUFFER
        gl_RGBA4 = JS.gl_RGBA4
        gl_RGB5_A1 = JS.gl_RGB5_A1
        gl_RGB565 = JS.gl_RGB565
        gl_DEPTH_COMPONENT16 = JS.gl_DEPTH_COMPONENT16
        gl_STENCIL_INDEX8 = JS.gl_STENCIL_INDEX8
        gl_RENDERBUFFER_WIDTH = JS.gl_RENDERBUFFER_WIDTH
        gl_RENDERBUFFER_HEIGHT = JS.gl_RENDERBUFFER_HEIGHT
        gl_RENDERBUFFER_INTERNAL_FORMAT = JS.gl_RENDERBUFFER_INTERNAL_FORMAT
        gl_RENDERBUFFER_RED_SIZE = JS.gl_RENDERBUFFER_RED_SIZE
        gl_RENDERBUFFER_GREEN_SIZE = JS.gl_RENDERBUFFER_GREEN_SIZE
        gl_RENDERBUFFER_BLUE_SIZE = JS.gl_RENDERBUFFER_BLUE_SIZE
        gl_RENDERBUFFER_ALPHA_SIZE = JS.gl_RENDERBUFFER_ALPHA_SIZE
        gl_RENDERBUFFER_DEPTH_SIZE = JS.gl_RENDERBUFFER_DEPTH_SIZE
        gl_RENDERBUFFER_STENCIL_SIZE = JS.gl_RENDERBUFFER_STENCIL_SIZE
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = JS.gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = JS.gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = JS.gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = JS.gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
        gl_MAX_DRAW_BUFFERS = JS.gl_MAX_DRAW_BUFFERS_WEBGL
        gl_DRAW_BUFFER0 = JS.gl_DRAW_BUFFER0_WEBGL
        gl_DRAW_BUFFER1 = JS.gl_DRAW_BUFFER1_WEBGL
        gl_DRAW_BUFFER2 = JS.gl_DRAW_BUFFER2_WEBGL
        gl_DRAW_BUFFER3 = JS.gl_DRAW_BUFFER3_WEBGL
        gl_DRAW_BUFFER4 = JS.gl_DRAW_BUFFER4_WEBGL
        gl_DRAW_BUFFER5 = JS.gl_DRAW_BUFFER5_WEBGL
        gl_DRAW_BUFFER6 = JS.gl_DRAW_BUFFER6_WEBGL
        gl_DRAW_BUFFER7 = JS.gl_DRAW_BUFFER7_WEBGL
        gl_DRAW_BUFFER8 = JS.gl_DRAW_BUFFER8_WEBGL
        gl_DRAW_BUFFER9 = JS.gl_DRAW_BUFFER9_WEBGL
        gl_DRAW_BUFFER10 = JS.gl_DRAW_BUFFER10_WEBGL
        gl_DRAW_BUFFER11 = JS.gl_DRAW_BUFFER11_WEBGL
        gl_DRAW_BUFFER12 = JS.gl_DRAW_BUFFER12_WEBGL
        gl_DRAW_BUFFER13 = JS.gl_DRAW_BUFFER13_WEBGL
        gl_DRAW_BUFFER14 = JS.gl_DRAW_BUFFER14_WEBGL
        gl_DRAW_BUFFER15 = JS.gl_DRAW_BUFFER15_WEBGL
        gl_MAX_COLOR_ATTACHMENTS = JS.gl_MAX_COLOR_ATTACHMENTS_WEBGL
        gl_COLOR_ATTACHMENT1 = JS.gl_COLOR_ATTACHMENT1_WEBGL
        gl_COLOR_ATTACHMENT2 = JS.gl_COLOR_ATTACHMENT2_WEBGL
        gl_COLOR_ATTACHMENT3 = JS.gl_COLOR_ATTACHMENT3_WEBGL
        gl_COLOR_ATTACHMENT4 = JS.gl_COLOR_ATTACHMENT4_WEBGL
        gl_COLOR_ATTACHMENT5 = JS.gl_COLOR_ATTACHMENT5_WEBGL
        gl_COLOR_ATTACHMENT6 = JS.gl_COLOR_ATTACHMENT6_WEBGL
        gl_COLOR_ATTACHMENT7 = JS.gl_COLOR_ATTACHMENT7_WEBGL
        gl_COLOR_ATTACHMENT8 = JS.gl_COLOR_ATTACHMENT8_WEBGL
        gl_COLOR_ATTACHMENT9 = JS.gl_COLOR_ATTACHMENT9_WEBGL
        gl_COLOR_ATTACHMENT10 = JS.gl_COLOR_ATTACHMENT10_WEBGL
        gl_COLOR_ATTACHMENT11 = JS.gl_COLOR_ATTACHMENT11_WEBGL
        gl_COLOR_ATTACHMENT12 = JS.gl_COLOR_ATTACHMENT12_WEBGL
        gl_COLOR_ATTACHMENT13 = JS.gl_COLOR_ATTACHMENT13_WEBGL
        gl_COLOR_ATTACHMENT14 = JS.gl_COLOR_ATTACHMENT14_WEBGL
        gl_COLOR_ATTACHMENT15 = JS.gl_COLOR_ATTACHMENT15_WEBGL
        gl_COLOR_ATTACHMENT0 = JS.gl_COLOR_ATTACHMENT0
        gl_DEPTH_ATTACHMENT = JS.gl_DEPTH_ATTACHMENT
        gl_STENCIL_ATTACHMENT = JS.gl_STENCIL_ATTACHMENT
        gl_NONE = JS.gl_NONE
        gl_FRAMEBUFFER_COMPLETE = JS.gl_FRAMEBUFFER_COMPLETE
        gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = JS.gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
        gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = JS.gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
        gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = JS.gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
        gl_FRAMEBUFFER_UNSUPPORTED = JS.gl_FRAMEBUFFER_UNSUPPORTED
        gl_FRAMEBUFFER_BINDING = JS.gl_FRAMEBUFFER_BINDING
        gl_RENDERBUFFER_BINDING = JS.gl_RENDERBUFFER_BINDING
        gl_MAX_RENDERBUFFER_SIZE = JS.gl_MAX_RENDERBUFFER_SIZE
        gl_INVALID_FRAMEBUFFER_OPERATION = JS.gl_INVALID_FRAMEBUFFER_OPERATION
