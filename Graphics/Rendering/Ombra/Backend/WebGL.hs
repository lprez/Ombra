{-# LANGUAGE NullaryTypeClasses, TypeFamilies, UndecidableInstances #-}

{-| The GHCJS/WebGL backend. This just exports the instance for 'GLES'. -}
module Graphics.Rendering.Ombra.Backend.WebGL (
        makeContext
) where

import Control.Applicative
import Control.Concurrent
import Data.Maybe
import qualified Data.HashMap.Strict as H
import Data.Int (Int32)
import Data.IORef
import Data.Vect.Float
import Data.Word
import Graphics.Rendering.Ombra.Backend
import qualified Graphics.Rendering.Ombra.Backend.WebGL.Const as JS
import qualified Graphics.Rendering.Ombra.Backend.WebGL.Raw as JS
import qualified Graphics.Rendering.Ombra.Backend.WebGL.Types as JS
import Graphics.Rendering.Ombra.Color
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

makeContext :: JSRef a -- ^ Canvas element.
            -> IO JS.Ctx
makeContext element = do ctx <- JS.getCtx element
                         JS.getExtension ctx $ toJSString "WEBGL_depth_texture"
                         vaoExt <- JS.getExtension ctx $
                                        toJSString "OES_vertex_array_object"
                         setProp "vaoExt" vaoExt ctx
                         return ctx

instance GLES where
        type Ctx = JS.Ctx
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
        type Texture = JS.Texture
        type Shader = JS.Shader
        type Program = JS.Program
        type FrameBuffer = JS.FrameBuffer
        type RenderBuffer = JS.RenderBuffer
        type VertexArrayObject = JS.VertexArrayObject
        -- type ActiveInfo = JS.ActiveInfo
        -- type ShaderPrecisionFormat = JS.ShaderPrecisionFormat
        type Array = JS.ArrayBufferView
        type Float32Array = JS.Float32Array
        type Int32Array = JS.Int32Array

        true = True
        false = False
        nullGLPtr = 0
        toGLString = toJSString
        noBuffer = JS.noBuffer
        noTexture = JS.noTexture
        noArray = JS.noArray
        noVAO = JS.noVAO

        encodeMat2 (Mat2 (Vec2 a1 a2) (Vec2 b1 b2)) =
                JS.listToJSArray [ a1, a2, b1, b2] >>= JS.float32Array

        encodeMat3 (Mat3 (Vec3 a1 a2 a3)
                         (Vec3 b1 b2 b3)
                         (Vec3 c1 c2 c3)) = JS.listToJSArray [ a1, a2, a3
                                                             , b1, b2, b3
                                                             , c1, c2, c3]
                                            >>= JS.float32Array
        encodeMat4 (Mat4 (Vec4 a1 a2 a3 a4)
                         (Vec4 b1 b2 b3 b4)
                         (Vec4 c1 c2 c3 c4)
                         (Vec4 d1 d2 d3 d4) ) =
                                 JS.listToJSArray [ a1, a2, a3, a4
                                                  , b1, b2, b3, b4
                                                  , c1, c2, c3, c4
                                                  , d1, d2, d3, d4 ]
                                 >>= JS.float32Array
        encodeFloats v = JS.listToJSArray v >>= JS.float32Array
        encodeInts v = JS.listToJSArray v >>= JS.int32Array

        -- TODO: decent implementation
        encodeVec2s v = JS.toJSArray next (False, v) >>= JS.float32Array
                where next (False, xs@(Vec2 x _ : _)) = Just (x, (True, xs))
                      next (True, Vec2 _ y : xs) = Just (y, (False, xs))
                      next (_, []) = Nothing

        encodeVec3s v = JS.toJSArray next (0, v) >>= JS.float32Array
                where next (0, xs@(Vec3 x _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(Vec3 _ y _ : _)) = Just (y, (2, xs))
                      next (2, Vec3 _ _ z : xs) = Just (z, (0, xs))
                      next (_, []) = Nothing

        encodeVec4s v = JS.toJSArray next (0, v) >>= JS.float32Array
                where next (0, xs@(Vec4 x _ _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(Vec4 _ y _ _ : _)) = Just (y, (2, xs))
                      next (2, xs@(Vec4 _ _ z _ : _)) = Just (z, (3, xs))
                      next (3, Vec4 _ _ _ w : xs) = Just (w, (0, xs))
                      next (_, []) = Nothing

        encodeIVec2s v = JS.toJSArray next (False, v) >>= JS.int32Array
                where next (False, xs@(IVec2 x _ : _)) = Just (x, (True, xs))
                      next (True, IVec2 _ y : xs) = Just (y, (False, xs))
                      next (_, []) = Nothing

        encodeIVec3s v = JS.toJSArray next (0, v) >>= JS.int32Array
                where next (0, xs@(IVec3 x _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(IVec3 _ y _ : _)) = Just (y, (2, xs))
                      next (2, IVec3 _ _ z : xs) = Just (z, (0, xs))
                      next (_, []) = Nothing

        encodeIVec4s v = JS.toJSArray next (0, v) >>= JS.int32Array
                where next (0, xs@(IVec4 x _ _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(IVec4 _ y _ _ : _)) = Just (y, (2, xs))
                      next (2, xs@(IVec4 _ _ z _ : _)) = Just (z, (3, xs))
                      next (3, IVec4 _ _ _ w : xs) = Just (w, (0, xs))
                      next (_, []) = Nothing

        encodeUShorts v = JS.listToJSArray v >>= JS.uint16View

        encodeColors v = JS.toJSArray next (0, v) >>= JS.uint8View
                where next (0, xs@(Color x _ _ _ : _)) = Just (x, (1, xs))
                      next (1, xs@(Color _ y _ _ : _)) = Just (y, (2, xs))
                      next (2, xs@(Color _ _ z _ : _)) = Just (z, (3, xs))
                      next (3, Color _ _ _ w : xs) = Just (w, (0, xs))
                      next (_, []) = Nothing

        newByteArray = fmap castRef . JS.uint8ArraySize
        fromFloat32Array = castRef
        fromInt32Array = castRef

        decodeBytes = (>>= mapM (fmap fromJust . fromJSRef))
                      . fromArray . castRef

        glActiveTexture = JS.glActiveTexture
        glAttachShader = JS.glAttachShader
        glBindAttribLocation = JS.glBindAttribLocation
        glBindBuffer = JS.glBindBuffer
        glBindFramebuffer = JS.glBindFramebuffer
        glBindRenderbuffer = JS.glBindRenderbuffer
        glBindTexture = JS.glBindTexture
        glBindVertexArray = JS.glBindVertexArrayOES
        glBlendColor = JS.glBlendColor
        glBlendEquation = JS.glBlendEquation
        glBlendEquationSeparate = JS.glBlendEquationSeparate
        glBlendFunc = JS.glBlendFunc
        glBlendFuncSeparate = JS.glBlendFuncSeparate
        glBufferData = JS.glBufferData
        glBufferSubData = JS.glBufferSubData
        glCheckFramebufferStatus = JS.glCheckFramebufferStatus
        glClear = JS.glClear
        glClearColor = JS.glClearColor
        glClearDepth = JS.glClearDepth
        glClearStencil = JS.glClearStencil
        glColorMask = JS.glColorMask
        glCompileShader = JS.glCompileShader
        glCompressedTexImage2D = JS.glCompressedTexImage2D
        glCompressedTexSubImage2D = JS.glCompressedTexSubImage2D
        glCopyTexImage2D = JS.glCopyTexImage2D
        glCopyTexSubImage2D = JS.glCopyTexSubImage2D
        glCreateBuffer = JS.glCreateBuffer
        glCreateFramebuffer = JS.glCreateFramebuffer
        glCreateProgram = JS.glCreateProgram
        glCreateRenderbuffer = JS.glCreateRenderbuffer
        glCreateShader = JS.glCreateShader
        glCreateTexture = JS.glCreateTexture
        glCreateVertexArray = JS.glCreateVertexArrayOES
        glCullFace = JS.glCullFace
        glDeleteBuffer = JS.glDeleteBuffer
        glDeleteFramebuffer = JS.glDeleteFramebuffer
        glDeleteProgram = JS.glDeleteProgram
        glDeleteRenderbuffer = JS.glDeleteRenderbuffer
        glDeleteShader = JS.glDeleteShader
        glDeleteTexture = JS.glDeleteTexture
        glDeleteVertexArray = JS.glDeleteVertexArrayOES
        glDepthFunc = JS.glDepthFunc
        glDepthMask = JS.glDepthMask
        glDepthRange = JS.glDepthRange
        glDetachShader = JS.glDetachShader
        glDisable = JS.glDisable
        glDisableVertexAttribArray = JS.glDisableVertexAttribArray
        glDrawArrays = JS.glDrawArrays
        glDrawElements = JS.glDrawElements
        glEnable = JS.glEnable
        glEnableVertexAttribArray = JS.glEnableVertexAttribArray
        glFinish = JS.glFinish
        glFlush = JS.glFlush
        glFramebufferRenderbuffer = JS.glFramebufferRenderbuffer
        glFramebufferTexture2D = JS.glFramebufferTexture2D
        glFrontFace = JS.glFrontFace
        glGenerateMipmap = JS.glGenerateMipmap
        -- glGetActiveAttrib = JS.glGetActiveAttrib
        -- glGetActiveUniform = JS.glGetActiveUniform
        glGetAttribLocation = JS.glGetAttribLocation
        -- glGetBufferParameter = JS.glGetBufferParameter
        -- glGetParameter = JS.glGetParameter
        glGetError = JS.glGetError
        -- glGetFramebufferAttachmentParameter = JS.glGetFramebufferAttachmentParameter
        glGetProgramInfoLog = JS.glGetProgramInfoLog
        -- glGetRenderbufferParameter = JS.glGetRenderbufferParameter
        -- glGetShaderParameter = JS.glGetShaderParameter
        -- glGetShaderPrecisionFormat = JS.glGetShaderPrecisionFormat
        glGetShaderInfoLog = JS.glGetShaderInfoLog
        glGetShaderSource = JS.glGetShaderSource
        -- glGetTexParameter = JS.glGetTexParameter
        -- glGetUniform = JS.glGetUniform
        glGetUniformLocation = JS.glGetUniformLocation
        -- glGetVertexAttrib = JS.glGetVertexAttrib
        -- glGetVertexAttribOffset = JS.glGetVertexAttribOffset
        glHint = JS.glHint
        glIsBuffer = JS.glIsBuffer
        glIsEnabled = JS.glIsEnabled
        glIsFramebuffer = JS.glIsFramebuffer
        glIsProgram = JS.glIsProgram
        glIsRenderbuffer = JS.glIsRenderbuffer
        glIsShader = JS.glIsShader
        glIsTexture = JS.glIsTexture
        glIsVertexArray = JS.glIsVertexArrayOES
        glLineWidth = JS.glLineWidth
        glLinkProgram = JS.glLinkProgram
        glPixelStorei = JS.glPixelStorei
        glPolygonOffset = JS.glPolygonOffset
        glReadPixels = JS.glReadPixels
        glRenderbufferStorage = JS.glRenderbufferStorage
        glSampleCoverage = JS.glSampleCoverage
        glScissor = JS.glScissor
        glShaderSource = JS.glShaderSource
        glStencilFunc = JS.glStencilFunc
        glStencilFuncSeparate = JS.glStencilFuncSeparate
        glStencilMask = JS.glStencilMask
        glStencilMaskSeparate = JS.glStencilMaskSeparate
        glStencilOp = JS.glStencilOp
        glStencilOpSeparate = JS.glStencilOpSeparate
        glTexImage2D = JS.glTexImage2D
        glTexParameterf = JS.glTexParameterf
        glTexParameteri = JS.glTexParameteri
        glTexSubImage2D = JS.glTexSubImage2D
        glUniform1f = JS.glUniform1f
        glUniform1fv = JS.glUniform1fv
        glUniform1i = JS.glUniform1i
        glUniform1iv = JS.glUniform1iv
        glUniform2f = JS.glUniform2f
        glUniform2fv = JS.glUniform2fv
        glUniform2i = JS.glUniform2i
        glUniform2iv = JS.glUniform2iv
        glUniform3f = JS.glUniform3f
        glUniform3fv = JS.glUniform3fv
        glUniform3i = JS.glUniform3i
        glUniform3iv = JS.glUniform3iv
        glUniform4f = JS.glUniform4f
        glUniform4fv = JS.glUniform4fv
        glUniform4i = JS.glUniform4i
        glUniform4iv = JS.glUniform4iv
        -- XXX
        glUniformMatrix2fv c loc _ arr = JS.glUniformMatrix2fv c loc False arr
        glUniformMatrix3fv c loc _ arr = JS.glUniformMatrix3fv c loc False arr
        glUniformMatrix4fv c loc _ arr = JS.glUniformMatrix4fv c loc False arr
        glUseProgram = JS.glUseProgram
        glValidateProgram = JS.glValidateProgram
        glVertexAttrib1f = JS.glVertexAttrib1f
        glVertexAttrib1fv = JS.glVertexAttrib1fv
        glVertexAttrib2f = JS.glVertexAttrib2f
        glVertexAttrib2fv = JS.glVertexAttrib2fv
        glVertexAttrib3f = JS.glVertexAttrib3f
        glVertexAttrib3fv = JS.glVertexAttrib3fv
        glVertexAttrib4f = JS.glVertexAttrib4f
        glVertexAttrib4fv = JS.glVertexAttrib4fv
        glVertexAttribPointer = JS.glVertexAttribPointer
        glViewport = JS.glViewport

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
