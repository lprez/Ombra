{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.Rendering.Ombra.Backend where

import Control.Concurrent (ThreadId)
import Data.Bits (Bits)
import Data.Vect.Float
import Data.Int
import Data.Word
import Foreign.Storable
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Ombra.Color

data IVec2 = IVec2 !Int32 !Int32 
data IVec3 = IVec3 !Int32 !Int32 !Int32
data IVec4 = IVec4 !Int32 !Int32 !Int32 !Int32

-- | Mixed OpenGL ES 2.0/WebGL 1.0/OpenGL 2.0 API, with VAOs and FBOs.
class ( Integral GLEnum
      , Integral GLUInt
      , Integral GLInt
      , Integral GLSize
      , Bits GLEnum
      , Num GLEnum
      , Num GLUInt
      , Num GLInt
      , Num GLPtrDiff
      , Num GLSize
      , Eq GLEnum
      , Eq GLUInt
      , Eq GLInt
      , Eq GLPtrDiff
      , Eq GLSize
      , Eq Texture) => GLES where
        type Ctx
        type GLEnum
        type GLUInt
        type GLInt
        type GLPtr
        type GLPtrDiff
        type GLSize
        type GLString
        type GLBool
        type Buffer
        type UniformLocation
        type Texture
        type Shader
        type Program
        type FrameBuffer
        type RenderBuffer
        type VertexArrayObject
        -- type ActiveInfo
        -- type ShaderPrecisionFormat
        type Array
        type Float32Array
        type Int32Array

        true :: GLBool
        false :: GLBool
        nullGLPtr :: GLPtr
        -- arrayGLPtr :: Array -> (GLPtr -> IO a) -> IO a
        toGLString :: String -> GLString
        noBuffer :: Buffer
        noTexture :: Texture
        noVAO :: VertexArrayObject
        noArray :: IO Array
        encodeMat2 :: Mat2 -> IO Float32Array
        encodeMat3 :: Mat3 -> IO Float32Array
        encodeMat4 :: Mat4 -> IO Float32Array
        encodeFloats :: [Float] -> IO Float32Array
        encodeInts :: [Int32] -> IO Int32Array
        encodeVec2s :: [Vec2] -> IO Float32Array
        encodeVec3s :: [Vec3] -> IO Float32Array
        encodeVec4s :: [Vec4] -> IO Float32Array
        encodeIVec2s :: [IVec2] -> IO Int32Array
        encodeIVec3s :: [IVec3] -> IO Int32Array
        encodeIVec4s :: [IVec4] -> IO Int32Array
        encodeUShorts :: [Word16] -> IO Array
        encodeColors :: [Color] -> IO Array

        newByteArray :: Int -> IO Array
        fromFloat32Array :: Float32Array -> Array
        fromInt32Array :: Int32Array -> Array
        decodeBytes :: Array -> IO [Word8]

        glActiveTexture :: Ctx -> GLEnum -> IO ()
        glAttachShader :: Ctx -> Program -> Shader -> IO ()
        glBindAttribLocation :: Ctx -> Program -> GLUInt -> GLString -> IO ()
        glBindBuffer :: Ctx -> GLEnum -> Buffer -> IO ()
        glBindFramebuffer :: Ctx -> GLEnum -> FrameBuffer -> IO ()
        glBindRenderbuffer :: Ctx -> GLEnum -> RenderBuffer -> IO ()
        glBindTexture :: Ctx -> GLEnum -> Texture -> IO ()
        glBindVertexArray :: Ctx -> VertexArrayObject -> IO ()
        glBlendColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()
        glBlendEquation :: Ctx -> GLEnum -> IO ()
        glBlendEquationSeparate :: Ctx -> GLEnum -> GLEnum -> IO ()
        glBlendFunc :: Ctx -> GLEnum -> GLEnum -> IO ()
        glBlendFuncSeparate :: Ctx -> GLEnum -> GLEnum -> GLEnum -> GLEnum -> IO ()
        glBufferData :: Ctx -> GLEnum -> Array -> GLEnum -> IO ()
        glBufferSubData :: Ctx -> GLEnum -> GLPtrDiff -> Array -> IO ()
        glCheckFramebufferStatus :: Ctx -> GLEnum -> IO GLEnum
        glClear :: Ctx -> GLEnum -> IO ()
        glClearColor :: Ctx -> Float -> Float -> Float -> Float -> IO ()
        glClearDepth :: Ctx -> Float -> IO ()
        glClearStencil :: Ctx -> GLInt -> IO ()
        glColorMask :: Ctx -> GLBool -> GLBool -> GLBool -> GLBool -> IO ()
        glCompileShader :: Ctx -> Shader -> IO ()
        glCompressedTexImage2D :: Ctx -> GLEnum -> GLInt -> GLEnum -> GLSize -> GLSize -> GLInt -> Array -> IO ()
        glCompressedTexSubImage2D :: Ctx -> GLEnum -> GLInt -> GLInt -> GLInt -> GLSize -> GLSize -> GLEnum -> Array -> IO ()
        glCopyTexImage2D :: Ctx -> GLEnum -> GLInt -> GLEnum -> GLInt -> GLInt -> GLSize -> GLSize -> GLInt -> IO ()
        glCopyTexSubImage2D :: Ctx -> GLEnum -> GLInt -> GLInt -> GLInt -> GLInt -> GLInt -> GLSize -> GLSize -> IO ()
        glCreateBuffer :: Ctx -> IO Buffer
        glCreateFramebuffer :: Ctx -> IO FrameBuffer
        glCreateProgram :: Ctx -> IO Program
        glCreateRenderbuffer :: Ctx -> IO RenderBuffer
        glCreateShader :: Ctx -> GLEnum -> IO Shader
        glCreateTexture :: Ctx -> IO Texture
        glCreateVertexArray :: Ctx -> IO VertexArrayObject
        glCullFace :: Ctx -> GLEnum -> IO ()
        glDeleteBuffer :: Ctx -> Buffer -> IO ()
        glDeleteFramebuffer :: Ctx -> FrameBuffer -> IO ()
        glDeleteProgram :: Ctx -> Program -> IO ()
        glDeleteRenderbuffer :: Ctx -> RenderBuffer -> IO ()
        glDeleteShader :: Ctx -> Shader -> IO ()
        glDeleteTexture :: Ctx -> Texture -> IO ()
        glDeleteVertexArray :: Ctx -> VertexArrayObject -> IO ()
        glDepthFunc :: Ctx -> GLEnum -> IO ()
        glDepthMask :: Ctx -> GLBool -> IO ()
        glDepthRange :: Ctx -> Float -> Float -> IO ()
        glDetachShader :: Ctx -> Program -> Shader -> IO ()
        glDisable :: Ctx -> GLEnum -> IO ()
        glDisableVertexAttribArray :: Ctx -> GLUInt -> IO ()
        glDrawArrays :: Ctx -> GLEnum -> GLInt -> GLSize -> IO ()
        glDrawBuffers :: Ctx -> Int32Array -> IO ()
        glDrawElements :: Ctx -> GLEnum -> GLSize -> GLEnum -> GLPtr-> IO ()
        glEnable :: Ctx -> GLEnum -> IO ()
        glEnableVertexAttribArray :: Ctx -> GLUInt -> IO ()
        glFinish :: Ctx -> IO ()
        glFlush :: Ctx -> IO ()
        glFramebufferRenderbuffer :: Ctx -> GLEnum -> GLEnum -> GLEnum -> RenderBuffer -> IO ()
        glFramebufferTexture2D :: Ctx -> GLEnum -> GLEnum -> GLEnum -> Texture -> GLInt -> IO ()
        glFrontFace :: Ctx -> GLEnum -> IO ()
        glGenerateMipmap :: Ctx -> GLEnum -> IO ()
        -- glGetActiveAttrib :: Ctx -> Program -> GLEnum -> IO ActiveInfo
        -- glGetActiveUniform :: Ctx -> Program -> GLEnum -> IO ActiveInfo
        glGetAttribLocation :: Ctx -> Program -> GLString -> IO GLInt
        -- glGetBufferParameter :: Ctx -> Word -> Word -> IO (JSRef a)
        -- glGetParameter :: Ctx -> Word -> IO (JSRef a)
        glGetError :: Ctx -> IO GLEnum
        -- glGetFramebufferAttachmentParameter :: Ctx -> GLEnum -> GLEnum -> IO Word
        glGetProgramInfoLog :: Ctx -> Program -> IO GLString
        -- glGetRenderbufferParameter :: Ctx -> Word -> Word -> IO (JSRef a)
        -- glGetShaderParameter :: Ctx -> Shader -> Word -> IO (JSRef a)
        -- glGetShaderPrecisionFormat :: Ctx -> GLEnum -> GLEnum -> IO ShaderPrecisionFormat
        glGetShaderInfoLog :: Ctx -> Shader -> IO GLString
        glGetShaderSource :: Ctx -> Shader -> IO GLString
        -- glGetTexParameter :: Ctx -> Word -> Word -> IO (JSRef a)
        -- glGetUniform :: Ctx -> Program -> UniformLocation -> IO (JSRef a)
        glGetUniformLocation :: Ctx -> Program -> GLString -> IO UniformLocation
        -- glGetVertexAttrib :: Ctx -> Word -> Word -> IO (JSRef a)
        -- glGetVertexAttribOffset :: Ctx -> Word -> GLEnum -> IO Word
        glHint :: Ctx -> GLEnum -> GLEnum -> IO ()
        glIsBuffer :: Ctx -> Buffer -> IO GLBool
        glIsEnabled :: Ctx -> GLEnum -> IO GLBool
        glIsFramebuffer :: Ctx -> FrameBuffer -> IO GLBool
        glIsProgram :: Ctx -> Program -> IO GLBool
        glIsRenderbuffer :: Ctx -> RenderBuffer -> IO GLBool
        glIsShader :: Ctx -> Shader -> IO GLBool
        glIsTexture :: Ctx -> Texture -> IO GLBool
        glIsVertexArray :: Ctx -> VertexArrayObject -> IO GLBool
        glLineWidth :: Ctx -> Float -> IO ()
        glLinkProgram :: Ctx -> Program -> IO ()
        glPixelStorei :: Ctx -> GLEnum -> GLInt -> IO ()
        glPolygonOffset :: Ctx -> Float -> Float -> IO ()
        glReadPixels :: Ctx -> GLInt -> GLInt -> GLSize -> GLSize -> GLEnum -> GLEnum -> Array -> IO ()
        glRenderbufferStorage :: Ctx -> GLEnum -> GLEnum -> GLSize -> GLSize -> IO ()
        glSampleCoverage :: Ctx -> Float -> GLBool -> IO ()
        glScissor :: Ctx -> GLInt -> GLInt -> GLSize -> GLSize -> IO ()
        glShaderSource :: Ctx -> Shader -> GLString -> IO ()
        glStencilFunc :: Ctx -> GLEnum -> GLInt -> GLUInt -> IO ()
        glStencilFuncSeparate :: Ctx -> GLEnum -> GLEnum -> GLInt -> GLUInt -> IO ()
        glStencilMask :: Ctx -> GLUInt -> IO ()
        glStencilMaskSeparate :: Ctx -> GLEnum -> GLUInt -> IO ()
        glStencilOp :: Ctx -> GLEnum -> GLEnum -> GLEnum -> IO ()
        glStencilOpSeparate :: Ctx -> GLEnum -> GLEnum -> GLEnum -> GLEnum -> IO ()
        glTexImage2D :: Ctx -> GLEnum -> GLInt -> GLInt -> GLSize -> GLSize -> GLInt -> GLEnum -> GLEnum -> Array -> IO ()
        glTexParameterf :: Ctx -> GLEnum -> GLEnum -> Float -> IO ()
        glTexParameteri :: Ctx -> GLEnum -> GLEnum -> GLInt -> IO ()
        glTexSubImage2D :: Ctx -> GLEnum -> GLInt -> GLInt -> GLInt -> GLSize -> GLSize -> GLEnum -> GLEnum -> Array -> IO ()
        glUniform1f :: Ctx -> UniformLocation -> Float -> IO ()
        glUniform1fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform1i :: Ctx -> UniformLocation -> Int32 -> IO ()
        glUniform1iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniform2f :: Ctx -> UniformLocation -> Float -> Float -> IO ()
        glUniform2fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform2i :: Ctx -> UniformLocation -> Int32 -> Int32 -> IO ()
        glUniform2iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniform3f :: Ctx -> UniformLocation -> Float -> Float -> Float -> IO ()
        glUniform3fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform3i :: Ctx -> UniformLocation -> Int32 -> Int32 -> Int32 -> IO ()
        glUniform3iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniform4f :: Ctx -> UniformLocation -> Float -> Float -> Float -> Float -> IO ()
        glUniform4fv :: Ctx -> UniformLocation -> Float32Array -> IO ()
        glUniform4i :: Ctx -> UniformLocation -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
        glUniform4iv :: Ctx -> UniformLocation -> Int32Array -> IO ()
        glUniformMatrix2fv :: Ctx -> UniformLocation -> GLBool -> Float32Array -> IO ()
        glUniformMatrix3fv :: Ctx -> UniformLocation -> GLBool -> Float32Array -> IO ()
        glUniformMatrix4fv :: Ctx -> UniformLocation -> GLBool -> Float32Array -> IO ()
        glUseProgram :: Ctx -> Program -> IO ()
        glValidateProgram :: Ctx -> Program -> IO ()
        glVertexAttrib1f :: Ctx -> GLUInt -> Float -> IO ()
        glVertexAttrib1fv :: Ctx -> GLUInt -> Float32Array -> IO ()
        glVertexAttrib2f :: Ctx -> GLUInt -> Float -> Float -> IO ()
        glVertexAttrib2fv :: Ctx -> GLUInt -> Float32Array -> IO ()
        glVertexAttrib3f :: Ctx -> GLUInt -> Float -> Float -> Float -> IO ()
        glVertexAttrib3fv :: Ctx -> GLUInt -> Float32Array -> IO ()
        glVertexAttrib4f :: Ctx -> GLUInt -> Float -> Float -> Float -> Float -> IO ()
        glVertexAttrib4fv :: Ctx -> GLUInt -> Float32Array -> IO ()
        glVertexAttribPointer :: Ctx -> GLUInt -> GLInt -> GLEnum -> GLBool -> GLSize -> GLPtr -> IO ()
        glViewport :: Ctx -> GLInt -> GLInt -> GLSize -> GLSize -> IO ()

        gl_DEPTH_BUFFER_BIT :: GLEnum
        gl_STENCIL_BUFFER_BIT :: GLEnum
        gl_COLOR_BUFFER_BIT :: GLEnum
        gl_POINTS :: GLEnum
        gl_LINES :: GLEnum
        gl_LINE_LOOP :: GLEnum
        gl_LINE_STRIP :: GLEnum
        gl_TRIANGLES :: GLEnum
        gl_TRIANGLE_STRIP :: GLEnum
        gl_TRIANGLE_FAN :: GLEnum
        gl_ZERO :: GLEnum
        gl_ONE :: GLEnum
        gl_SRC_COLOR :: GLEnum
        gl_ONE_MINUS_SRC_COLOR :: GLEnum
        gl_SRC_ALPHA :: GLEnum
        gl_ONE_MINUS_SRC_ALPHA :: GLEnum
        gl_DST_ALPHA :: GLEnum
        gl_ONE_MINUS_DST_ALPHA :: GLEnum
        gl_DST_COLOR :: GLEnum
        gl_ONE_MINUS_DST_COLOR :: GLEnum
        gl_SRC_ALPHA_SATURATE :: GLEnum
        gl_FUNC_ADD :: GLEnum
        gl_BLEND_EQUATION :: GLEnum
        gl_BLEND_EQUATION_RGB :: GLEnum
        gl_BLEND_EQUATION_ALPHA :: GLEnum
        gl_FUNC_SUBTRACT :: GLEnum
        gl_FUNC_REVERSE_SUBTRACT :: GLEnum
        gl_BLEND_DST_RGB :: GLEnum
        gl_BLEND_SRC_RGB :: GLEnum
        gl_BLEND_DST_ALPHA :: GLEnum
        gl_BLEND_SRC_ALPHA :: GLEnum
        gl_CONSTANT_COLOR :: GLEnum
        gl_ONE_MINUS_CONSTANT_COLOR :: GLEnum
        gl_CONSTANT_ALPHA :: GLEnum
        gl_ONE_MINUS_CONSTANT_ALPHA :: GLEnum
        gl_BLEND_COLOR :: GLEnum
        gl_ARRAY_BUFFER :: GLEnum
        gl_ELEMENT_ARRAY_BUFFER :: GLEnum
        gl_ARRAY_BUFFER_BINDING :: GLEnum
        gl_ELEMENT_ARRAY_BUFFER_BINDING :: GLEnum
        gl_STREAM_DRAW :: GLEnum
        gl_STATIC_DRAW :: GLEnum
        gl_DYNAMIC_DRAW :: GLEnum
        gl_BUFFER_SIZE :: GLEnum
        gl_BUFFER_USAGE :: GLEnum
        gl_CURRENT_VERTEX_ATTRIB :: GLEnum
        gl_FRONT :: GLEnum
        gl_BACK :: GLEnum
        gl_FRONT_AND_BACK :: GLEnum
        gl_CULL_FACE :: GLEnum
        gl_BLEND :: GLEnum
        gl_DITHER :: GLEnum
        gl_STENCIL_TEST :: GLEnum
        gl_DEPTH_TEST :: GLEnum
        gl_SCISSOR_TEST :: GLEnum
        gl_POLYGON_OFFSET_FILL :: GLEnum
        gl_SAMPLE_ALPHA_TO_COVERAGE :: GLEnum
        gl_SAMPLE_COVERAGE :: GLEnum
        gl_NO_ERROR :: GLEnum
        gl_INVALID_ENUM :: GLEnum
        gl_INVALID_VALUE :: GLEnum
        gl_INVALID_OPERATION :: GLEnum
        gl_OUT_OF_MEMORY :: GLEnum
        gl_CW :: GLEnum
        gl_CCW :: GLEnum
        gl_LINE_WIDTH :: GLEnum
        gl_ALIASED_POINT_SIZE_RANGE :: GLEnum
        gl_ALIASED_LINE_WIDTH_RANGE :: GLEnum
        gl_CULL_FACE_MODE :: GLEnum
        gl_FRONT_FACE :: GLEnum
        gl_DEPTH_RANGE :: GLEnum
        gl_DEPTH_WRITEMASK :: GLEnum
        gl_DEPTH_CLEAR_VALUE :: GLEnum
        gl_DEPTH_FUNC :: GLEnum
        gl_STENCIL_CLEAR_VALUE :: GLEnum
        gl_STENCIL_FUNC :: GLEnum
        gl_STENCIL_FAIL :: GLEnum
        gl_STENCIL_PASS_DEPTH_FAIL :: GLEnum
        gl_STENCIL_PASS_DEPTH_PASS :: GLEnum
        gl_STENCIL_REF :: GLEnum
        gl_STENCIL_VALUE_MASK :: GLEnum
        gl_STENCIL_WRITEMASK :: GLEnum
        gl_STENCIL_BACK_FUNC :: GLEnum
        gl_STENCIL_BACK_FAIL :: GLEnum
        gl_STENCIL_BACK_PASS_DEPTH_FAIL :: GLEnum
        gl_STENCIL_BACK_PASS_DEPTH_PASS :: GLEnum
        gl_STENCIL_BACK_REF :: GLEnum
        gl_STENCIL_BACK_VALUE_MASK :: GLEnum
        gl_STENCIL_BACK_WRITEMASK :: GLEnum
        gl_VIEWPORT :: GLEnum
        gl_SCISSOR_BOX :: GLEnum
        gl_COLOR_CLEAR_VALUE :: GLEnum
        gl_COLOR_WRITEMASK :: GLEnum
        gl_UNPACK_ALIGNMENT :: GLEnum
        gl_PACK_ALIGNMENT :: GLEnum
        gl_MAX_TEXTURE_SIZE :: GLEnum
        gl_MAX_VIEWPORT_DIMS :: GLEnum
        gl_SUBPIXEL_BITS :: GLEnum
        gl_RED_BITS :: GLEnum
        gl_GREEN_BITS :: GLEnum
        gl_BLUE_BITS :: GLEnum
        gl_ALPHA_BITS :: GLEnum
        gl_DEPTH_BITS :: GLEnum
        gl_STENCIL_BITS :: GLEnum
        gl_POLYGON_OFFSET_UNITS :: GLEnum
        gl_POLYGON_OFFSET_FACTOR :: GLEnum
        gl_TEXTURE_BINDING_2D :: GLEnum
        gl_SAMPLE_BUFFERS :: GLEnum
        gl_SAMPLES :: GLEnum
        gl_SAMPLE_COVERAGE_VALUE :: GLEnum
        gl_SAMPLE_COVERAGE_INVERT :: GLEnum
        gl_COMPRESSED_TEXTURE_FORMATS :: GLEnum
        gl_DONT_CARE :: GLEnum
        gl_FASTEST :: GLEnum
        gl_NICEST :: GLEnum
        gl_GENERATE_MIPMAP_HINT :: GLEnum
        gl_BYTE :: GLEnum
        gl_UNSIGNED_BYTE :: GLEnum
        gl_SHORT :: GLEnum
        gl_UNSIGNED_SHORT :: GLEnum
        gl_INT :: GLEnum
        gl_UNSIGNED_INT :: GLEnum
        gl_FLOAT :: GLEnum
        gl_DEPTH_COMPONENT :: GLEnum
        gl_ALPHA :: GLEnum
        gl_RGB :: GLEnum
        gl_RGBA :: GLEnum
        gl_LUMINANCE :: GLEnum
        gl_LUMINANCE_ALPHA :: GLEnum
        gl_UNSIGNED_SHORT_4_4_4_4 :: GLEnum
        gl_UNSIGNED_SHORT_5_5_5_1 :: GLEnum
        gl_UNSIGNED_SHORT_5_6_5 :: GLEnum
        gl_FRAGMENT_SHADER :: GLEnum
        gl_VERTEX_SHADER :: GLEnum
        gl_MAX_VERTEX_ATTRIBS :: GLEnum
        gl_MAX_VERTEX_UNIFORM_VECTORS :: GLEnum
        gl_MAX_VARYING_VECTORS :: GLEnum
        gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: GLEnum
        gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: GLEnum
        gl_MAX_TEXTURE_IMAGE_UNITS :: GLEnum
        gl_MAX_FRAGMENT_UNIFORM_VECTORS :: GLEnum
        gl_SHADER_TYPE :: GLEnum
        gl_DELETE_STATUS :: GLEnum
        gl_LINK_STATUS :: GLEnum
        gl_VALIDATE_STATUS :: GLEnum
        gl_ATTACHED_SHADERS :: GLEnum
        gl_ACTIVE_UNIFORMS :: GLEnum
        gl_ACTIVE_ATTRIBUTES :: GLEnum
        gl_SHADING_LANGUAGE_VERSION :: GLEnum
        gl_CURRENT_PROGRAM :: GLEnum
        gl_NEVER :: GLEnum
        gl_LESS :: GLEnum
        gl_EQUAL :: GLEnum
        gl_LEQUAL :: GLEnum
        gl_GREATER :: GLEnum
        gl_NOTEQUAL :: GLEnum
        gl_GEQUAL :: GLEnum
        gl_ALWAYS :: GLEnum
        gl_KEEP :: GLEnum
        gl_REPLACE :: GLEnum
        gl_INCR :: GLEnum
        gl_DECR :: GLEnum
        gl_INVERT :: GLEnum
        gl_INCR_WRAP :: GLEnum
        gl_DECR_WRAP :: GLEnum
        gl_VENDOR :: GLEnum
        gl_RENDERER :: GLEnum
        gl_VERSION :: GLEnum
        gl_NEAREST :: GLEnum
        gl_LINEAR :: GLEnum
        gl_NEAREST_MIPMAP_NEAREST :: GLEnum
        gl_LINEAR_MIPMAP_NEAREST :: GLEnum
        gl_NEAREST_MIPMAP_LINEAR :: GLEnum
        gl_LINEAR_MIPMAP_LINEAR :: GLEnum
        gl_TEXTURE_MAG_FILTER :: GLEnum
        gl_TEXTURE_MIN_FILTER :: GLEnum
        gl_TEXTURE_WRAP_S :: GLEnum
        gl_TEXTURE_WRAP_T :: GLEnum
        gl_TEXTURE_2D :: GLEnum
        gl_TEXTURE :: GLEnum
        gl_TEXTURE_CUBE_MAP :: GLEnum
        gl_TEXTURE_BINDING_CUBE_MAP :: GLEnum
        gl_TEXTURE_CUBE_MAP_POSITIVE_X :: GLEnum
        gl_TEXTURE_CUBE_MAP_NEGATIVE_X :: GLEnum
        gl_TEXTURE_CUBE_MAP_POSITIVE_Y :: GLEnum
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: GLEnum
        gl_TEXTURE_CUBE_MAP_POSITIVE_Z :: GLEnum
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: GLEnum
        gl_MAX_CUBE_MAP_TEXTURE_SIZE :: GLEnum
        gl_TEXTURE0 :: GLEnum
        gl_TEXTURE1 :: GLEnum
        gl_TEXTURE2 :: GLEnum
        gl_TEXTURE3 :: GLEnum
        gl_TEXTURE4 :: GLEnum
        gl_TEXTURE5 :: GLEnum
        gl_TEXTURE6 :: GLEnum
        gl_TEXTURE7 :: GLEnum
        gl_TEXTURE8 :: GLEnum
        gl_TEXTURE9 :: GLEnum
        gl_TEXTURE10 :: GLEnum
        gl_TEXTURE11 :: GLEnum
        gl_TEXTURE12 :: GLEnum
        gl_TEXTURE13 :: GLEnum
        gl_TEXTURE14 :: GLEnum
        gl_TEXTURE15 :: GLEnum
        gl_TEXTURE16 :: GLEnum
        gl_TEXTURE17 :: GLEnum
        gl_TEXTURE18 :: GLEnum
        gl_TEXTURE19 :: GLEnum
        gl_TEXTURE20 :: GLEnum
        gl_TEXTURE21 :: GLEnum
        gl_TEXTURE22 :: GLEnum
        gl_TEXTURE23 :: GLEnum
        gl_TEXTURE24 :: GLEnum
        gl_TEXTURE25 :: GLEnum
        gl_TEXTURE26 :: GLEnum
        gl_TEXTURE27 :: GLEnum
        gl_TEXTURE28 :: GLEnum
        gl_TEXTURE29 :: GLEnum
        gl_TEXTURE30 :: GLEnum
        gl_TEXTURE31 :: GLEnum
        gl_ACTIVE_TEXTURE :: GLEnum
        gl_REPEAT :: GLEnum
        gl_CLAMP_TO_EDGE :: GLEnum
        gl_MIRRORED_REPEAT :: GLEnum
        gl_FLOAT_VEC2 :: GLEnum
        gl_FLOAT_VEC3 :: GLEnum
        gl_FLOAT_VEC4 :: GLEnum
        gl_INT_VEC2 :: GLEnum
        gl_INT_VEC3 :: GLEnum
        gl_INT_VEC4 :: GLEnum
        gl_BOOL :: GLEnum
        gl_BOOL_VEC2 :: GLEnum
        gl_BOOL_VEC3 :: GLEnum
        gl_BOOL_VEC4 :: GLEnum
        gl_FLOAT_MAT2 :: GLEnum
        gl_FLOAT_MAT3 :: GLEnum
        gl_FLOAT_MAT4 :: GLEnum
        gl_SAMPLER_2D :: GLEnum
        gl_SAMPLER_CUBE :: GLEnum
        gl_VERTEX_ATTRIB_ARRAY_ENABLED :: GLEnum
        gl_VERTEX_ATTRIB_ARRAY_SIZE :: GLEnum
        gl_VERTEX_ATTRIB_ARRAY_STRIDE :: GLEnum
        gl_VERTEX_ATTRIB_ARRAY_TYPE :: GLEnum
        gl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: GLEnum
        gl_VERTEX_ATTRIB_ARRAY_POINTER :: GLEnum
        gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: GLEnum
        gl_COMPILE_STATUS :: GLEnum
        gl_LOW_FLOAT :: GLEnum
        gl_MEDIUM_FLOAT :: GLEnum
        gl_HIGH_FLOAT :: GLEnum
        gl_LOW_INT :: GLEnum
        gl_MEDIUM_INT :: GLEnum
        gl_HIGH_INT :: GLEnum
        gl_FRAMEBUFFER :: GLEnum
        gl_RENDERBUFFER :: GLEnum
        gl_RGBA4 :: GLEnum
        gl_RGB5_A1 :: GLEnum
        gl_RGB565 :: GLEnum
        gl_DEPTH_COMPONENT16 :: GLEnum
        gl_STENCIL_INDEX8 :: GLEnum
        gl_RENDERBUFFER_WIDTH :: GLEnum
        gl_RENDERBUFFER_HEIGHT :: GLEnum
        gl_RENDERBUFFER_INTERNAL_FORMAT :: GLEnum
        gl_RENDERBUFFER_RED_SIZE :: GLEnum
        gl_RENDERBUFFER_GREEN_SIZE :: GLEnum
        gl_RENDERBUFFER_BLUE_SIZE :: GLEnum
        gl_RENDERBUFFER_ALPHA_SIZE :: GLEnum
        gl_RENDERBUFFER_DEPTH_SIZE :: GLEnum
        gl_RENDERBUFFER_STENCIL_SIZE :: GLEnum
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: GLEnum
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: GLEnum
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: GLEnum
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: GLEnum
        gl_MAX_DRAW_BUFFERS :: GLEnum
        gl_DRAW_BUFFER0 :: GLEnum
        gl_DRAW_BUFFER1 :: GLEnum
        gl_DRAW_BUFFER2 :: GLEnum
        gl_DRAW_BUFFER3 :: GLEnum
        gl_DRAW_BUFFER4 :: GLEnum
        gl_DRAW_BUFFER5 :: GLEnum
        gl_DRAW_BUFFER6 :: GLEnum
        gl_DRAW_BUFFER7 :: GLEnum
        gl_DRAW_BUFFER8 :: GLEnum
        gl_DRAW_BUFFER9 :: GLEnum
        gl_DRAW_BUFFER10 :: GLEnum
        gl_DRAW_BUFFER11 :: GLEnum
        gl_DRAW_BUFFER12 :: GLEnum
        gl_DRAW_BUFFER13 :: GLEnum
        gl_DRAW_BUFFER14 :: GLEnum
        gl_DRAW_BUFFER15 :: GLEnum
        gl_MAX_COLOR_ATTACHMENTS :: GLEnum
        gl_COLOR_ATTACHMENT0 :: GLEnum
        gl_COLOR_ATTACHMENT1 :: GLEnum
        gl_COLOR_ATTACHMENT2 :: GLEnum
        gl_COLOR_ATTACHMENT3 :: GLEnum
        gl_COLOR_ATTACHMENT4 :: GLEnum
        gl_COLOR_ATTACHMENT5 :: GLEnum
        gl_COLOR_ATTACHMENT6 :: GLEnum
        gl_COLOR_ATTACHMENT7 :: GLEnum
        gl_COLOR_ATTACHMENT8 :: GLEnum
        gl_COLOR_ATTACHMENT9 :: GLEnum
        gl_COLOR_ATTACHMENT10 :: GLEnum
        gl_COLOR_ATTACHMENT11 :: GLEnum
        gl_COLOR_ATTACHMENT12 :: GLEnum
        gl_COLOR_ATTACHMENT13 :: GLEnum
        gl_COLOR_ATTACHMENT14 :: GLEnum
        gl_COLOR_ATTACHMENT15 :: GLEnum
        gl_DEPTH_ATTACHMENT :: GLEnum
        gl_STENCIL_ATTACHMENT :: GLEnum
        gl_NONE :: GLEnum
        gl_FRAMEBUFFER_COMPLETE :: GLEnum
        gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: GLEnum
        gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: GLEnum
        gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: GLEnum
        gl_FRAMEBUFFER_UNSUPPORTED :: GLEnum
        gl_FRAMEBUFFER_BINDING :: GLEnum
        gl_RENDERBUFFER_BINDING :: GLEnum
        gl_MAX_RENDERBUFFER_SIZE :: GLEnum
        gl_INVALID_FRAMEBUFFER_OPERATION :: GLEnum

instance Storable IVec2 where
        sizeOf _ = 8
        alignment _ = 4
        peek ptr = IVec2 <$> peekElemOff (castPtr ptr) 0
                         <*> peekElemOff (castPtr ptr) 1
        poke ptr (IVec2 x y) = do pokeElemOff (castPtr ptr) 0 x
                                  pokeElemOff (castPtr ptr) 1 y

instance Storable IVec3 where
        sizeOf _ = 12
        alignment _ = 4
        peek ptr = IVec3 <$> peekElemOff (castPtr ptr) 0
                         <*> peekElemOff (castPtr ptr) 1
                         <*> peekElemOff (castPtr ptr) 2
        poke ptr (IVec3 x y z) = do pokeElemOff (castPtr ptr) 0 x
                                    pokeElemOff (castPtr ptr) 1 y
                                    pokeElemOff (castPtr ptr) 2 z

instance Storable IVec4 where
        sizeOf _ = 16
        alignment _ = 4
        peek ptr = IVec4 <$> peekElemOff (castPtr ptr) 0
                         <*> peekElemOff (castPtr ptr) 1
                         <*> peekElemOff (castPtr ptr) 2
                         <*> peekElemOff (castPtr ptr) 3
        poke ptr (IVec4 x y z w) = do pokeElemOff (castPtr ptr) 0 x
                                      pokeElemOff (castPtr ptr) 1 y
                                      pokeElemOff (castPtr ptr) 2 z
                                      pokeElemOff (castPtr ptr) 3 w
