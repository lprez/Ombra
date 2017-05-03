{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Backend.OpenGL (makeContext) where
        
import Data.Word
import Foreign
import Foreign.C.String
import Graphics.Rendering.Ombra.Backend
import Graphics.Rendering.Ombra.Vector
import qualified Graphics.GL.Standard20 as GL
import qualified Graphics.GL.Ext.ARB.FramebufferObject as GL
import qualified Graphics.GL.Ext.ARB.TextureFloat as GL
import qualified Graphics.GL.Ext.ARB.VertexArrayObject as GL
import qualified Graphics.GL.Ext.EXT.BlendColor as GL
import Graphics.GL.Types as GL

makeContext :: IO Ctx
makeContext =
        words <$> (GL.glGetString GL.GL_EXTENSIONS >>= peekCString . castPtr)

genToCreate :: Storable a => (GLsizei -> Ptr a -> IO ()) -> ctx -> IO a
genToCreate gen _ = do ptr <- malloc
                       gen 1 ptr
                       value <- peek ptr
                       free ptr
                       return value

deleteToDelete :: Storable a => (GLsizei -> Ptr a -> IO ()) -> ctx -> a -> IO ()
deleteToDelete del _ = flip with $ del 1

getString :: (a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
          -> ctx -> a -> IO String
getString f _ x = do cstr <- mallocArray len
                     f x (fromIntegral len) nullPtr cstr
                     str <- peekCString cstr
                     free cstr
                     return str
        where len = 4096

uniform :: (a -> GLsizei -> Ptr b -> IO ())
        -> ctx -> a -> (GLsizei, ForeignPtr b) -> IO ()
uniform f _ a (len, fp) = withForeignPtr fp $ f a (quot len 4)

uniformMatrix :: (a -> GLsizei -> GLboolean -> Ptr b -> IO ()) -> GLsizei
              -> ctx -> a -> GLboolean -> (GLsizei, ForeignPtr b) -> IO ()
uniformMatrix f dv _ a b (len, fp) =
        withForeignPtr fp $ f a 1 {- (quot len dv) -} b

vertexAttrib :: (a -> Ptr b -> IO ())
             -> ctx -> a -> (GLsizei, ForeignPtr b) -> IO ()
vertexAttrib f _ a (_, fp) = withForeignPtr fp $ f a

mkArrayLen :: Int -> IO (GLsizei, ForeignPtr b)
mkArrayLen len = do arr <- mallocForeignPtrArray (fromIntegral len)
                           :: IO (ForeignPtr Word8)
                    return (fromIntegral len, castForeignPtr arr)

arrayToList :: Storable a => (GLsizei, ForeignPtr ()) -> IO [a]
arrayToList (sz, fptr) = withForeignPtr (castForeignPtr fptr) $ \ptr ->
                                peekArray (fromIntegral sz) ptr

mkArray :: Storable a => [a] -> IO (GLsizei, ForeignPtr b)
mkArray xs = do arr <- mallocForeignPtrArray len
                withForeignPtr arr $ flip pokeArray xs
                return (fromIntegral size, castForeignPtr arr)
        where len = length xs
              size = len * sizeOf (head xs)

instance GLES where
        type Ctx = [String]
        type GLEnum = GLenum
        type GLUInt = GLuint
        type GLInt = GLint
        type GLPtr = Ptr ()
        type GLPtrDiff = GLintptr
        type GLSize = GLsizei
        type GLString = String
        type GLBool = GLboolean
        type Buffer = GLuint
        type UniformLocation = GLint
        type Texture = GLuint
        type Shader = GLuint
        type Program = GLuint
        type FrameBuffer = GLuint
        type RenderBuffer = GLuint
        type VertexArrayObject = GLuint
        -- type ShaderPrecisionFormat = GLint
        type AnyArray = (GLsizei, ForeignPtr ())
        type Float32Array = (GLsizei, ForeignPtr GLfloat)
        type Int32Array = (GLsizei, ForeignPtr GLint)
        type UInt16Array = (GLsizei, ForeignPtr GLushort)
        type UInt8Array = (GLsizei, ForeignPtr GLubyte)

        true = 1
        false = 0
        isTrue = (== 1)
        nullGLPtr = nullPtr
        toGLString = id
        fromGLString = id
        noBuffer = 0
        noTexture = 0
        noVAO = 0
        noUInt8Array = fmap ((,) 0) $ newForeignPtr_ nullPtr
        noFloat32Array = fmap ((,) 0) $ newForeignPtr_ nullPtr

        encodeMat2 (Mat2 (Vec2 a1 a2) (Vec2 b1 b2)) =
                mkArray [ a1, a2, b1, b2 ]

        encodeMat3 (Mat3 (Vec3 a1 a2 a3)
                         (Vec3 b1 b2 b3)
                         (Vec3 c1 c2 c3)) = mkArray [ a1, a2, a3
                                                    , b1, b2, b3
                                                    , c1, c2, c3 ]
        encodeMat4 (Mat4 (Vec4 a1 a2 a3 a4)
                         (Vec4 b1 b2 b3 b4)
                         (Vec4 c1 c2 c3 c4)
                         (Vec4 d1 d2 d3 d4) ) = mkArray [ a1, a2, a3, a4
                                                        , b1, b2, b3, b4
                                                        , c1, c2, c3, c4
                                                        , d1, d2, d3, d4 ]

        encodeFloats = mkArray
        encodeVec2s = mkArray
        encodeVec3s = mkArray
        encodeVec4s = mkArray
        encodeInts = mkArray
        encodeIVec2s = mkArray
        encodeIVec3s = mkArray
        encodeIVec4s = mkArray
        encodeUShorts = mkArray
        encodeUInt8s = mkArray

        newByteArray = mkArrayLen
        fromFloat32Array (size, fptr) = (size, castForeignPtr fptr)
        fromInt32Array (size, fptr) = (size, castForeignPtr fptr)
        fromUInt16Array (size, fptr) = (size, castForeignPtr fptr)
        fromUInt8Array (size, fptr) = (size, castForeignPtr fptr)
        decodeBytes (s, f) = arrayToList (s, castForeignPtr f)

        hasVertexArrayObjects = return . elem "GL_ARB_vertex_array_object"
        hasFloatTextures = return . elem "GL_ARB_texture_float"
        hasDrawBuffers = return . const True

        glActiveTexture = const GL.glActiveTexture
        glAttachShader = const GL.glAttachShader
        glBindAttribLocation _ a b c = withCString c $ GL.glBindAttribLocation a b
        glBindBuffer = const GL.glBindBuffer
        glBindFramebuffer = const GL.glBindFramebuffer
        glBindRenderbuffer = const GL.glBindRenderbuffer
        glBindTexture = const GL.glBindTexture
        glBindVertexArray = const GL.glBindVertexArray 
        glBlendColor = const GL.glBlendColor
        glBlendEquation = const GL.glBlendEquation
        glBlendEquationSeparate = const GL.glBlendEquationSeparate
        glBlendFunc = const GL.glBlendFunc
        glBlendFuncSeparate = const GL.glBlendFuncSeparate
        glBufferData _ a (l, fp) b = withForeignPtr fp $ \p ->
                GL.glBufferData a (fromIntegral l) (castPtr p) b
        glBufferSubData _ a b (l, fp) = withForeignPtr fp $ \p ->
                GL.glBufferSubData a b (fromIntegral l) (castPtr p)
        glCheckFramebufferStatus = const GL.glCheckFramebufferStatus
        glClear = const GL.glClear
        glClearColor = const GL.glClearColor
        glClearDepth _ = GL.glClearDepth . realToFrac
        glClearStencil = const GL.glClearStencil
        glColorMask = const GL.glColorMask
        glCompileShader = const GL.glCompileShader
        glCompressedTexImage2D _ a b c d e f (l, fp) = withForeignPtr fp $
                \p -> GL.glCompressedTexImage2D a b c d e f l (castPtr p)
        glCompressedTexSubImage2D _ a b c d e f g (l, fp) = withForeignPtr fp $
                \p -> GL.glCompressedTexSubImage2D a b c d e f g l (castPtr p)
        glCopyTexImage2D = const GL.glCopyTexImage2D
        glCopyTexSubImage2D = const GL.glCopyTexSubImage2D
        glCreateBuffer = genToCreate GL.glGenBuffers
        glCreateFramebuffer = genToCreate GL.glGenFramebuffers
        glCreateProgram = const GL.glCreateProgram
        glCreateRenderbuffer = genToCreate GL.glGenRenderbuffers
        glCreateShader = const GL.glCreateShader
        glCreateTexture = genToCreate GL.glGenTextures
        glCreateVertexArray = genToCreate GL.glGenVertexArrays
        glCullFace = const GL.glCullFace
        glDeleteBuffer = deleteToDelete GL.glDeleteBuffers
        glDeleteFramebuffer = deleteToDelete GL.glDeleteFramebuffers
        glDeleteProgram = const GL.glDeleteProgram
        glDeleteRenderbuffer = deleteToDelete GL.glDeleteRenderbuffers
        glDeleteShader = const GL.glDeleteShader
        glDeleteTexture = deleteToDelete GL.glDeleteTextures
        glDeleteVertexArray = deleteToDelete GL.glDeleteVertexArrays
        glDepthFunc = const GL.glDepthFunc
        glDepthMask = const GL.glDepthMask
        glDepthRange _ a b = GL.glDepthRange (realToFrac a) (realToFrac b)
        glDetachShader = const GL.glDetachShader
        glDisable = const GL.glDisable
        glDisableVertexAttribArray = const GL.glDisableVertexAttribArray
        glDrawArrays = const GL.glDrawArrays
        glDrawElements = const GL.glDrawElements
        glDrawBuffers _ (l, fp) = withForeignPtr fp $
                \p -> GL.glDrawBuffers (l `quot` 4) $ castPtr p
        glEnable = const GL.glEnable
        glEnableVertexAttribArray = const GL.glEnableVertexAttribArray
        glFinish = const GL.glFinish
        glFlush = const GL.glFlush
        glFramebufferRenderbuffer = const GL.glFramebufferRenderbuffer
        glFramebufferTexture2D = const GL.glFramebufferTexture2D
        glFrontFace = const GL.glFrontFace
        glGenerateMipmap = const GL.glGenerateMipmap
        glGetAttribLocation _ a b = withCString b $ GL.glGetAttribLocation a
        glGetError = const GL.glGetError
        glGetProgramInfoLog = getString GL.glGetProgramInfoLog
        glGetShaderInfoLog = getString GL.glGetShaderInfoLog
        glGetShaderSource = getString GL.glGetShaderSource
        glGetShaderParameterBool _ a b = do ptr <- malloc
                                            GL.glGetShaderiv a b ptr
                                            value <- peek ptr
                                            free ptr
                                            return $ fromIntegral value
        glGetUniformLocation _ a b = withCString b $ GL.glGetUniformLocation a
        glHint = const GL.glHint
        glIsBuffer = const GL.glIsBuffer
        glIsEnabled = const GL.glIsEnabled
        glIsFramebuffer = const GL.glIsFramebuffer
        glIsProgram = const GL.glIsProgram
        glIsRenderbuffer = const GL.glIsRenderbuffer
        glIsShader = const GL.glIsShader
        glIsTexture = const GL.glIsTexture
        glIsVertexArray = const GL.glIsVertexArray
        glLineWidth = const GL.glLineWidth
        glLinkProgram = const GL.glLinkProgram
        glPixelStorei = const GL.glPixelStorei
        glPolygonOffset = const GL.glPolygonOffset
        glReadPixels _ a b c d e f (_, fp)  = withForeignPtr fp $
                GL.glReadPixels a b c d e f . castPtr
        glRenderbufferStorage = const GL.glRenderbufferStorage
        glSampleCoverage = const GL.glSampleCoverage
        glScissor = const GL.glScissor
        glShaderSource _ shader src =
                withCString src $ \csrc ->
                        with (fromIntegral $ length src) $ \lenptr ->
                               with csrc $ \csrcptr ->
                                      GL.glShaderSource shader 1 csrcptr lenptr
        glStencilFunc = const GL.glStencilFunc
        glStencilFuncSeparate = const GL.glStencilFuncSeparate
        glStencilMask = const GL.glStencilMask
        glStencilMaskSeparate = const GL.glStencilMaskSeparate
        glStencilOp = const GL.glStencilOp
        glStencilOpSeparate = const GL.glStencilOpSeparate
        glTexImage2DUInt _ a b c d e f g h (_, fp) = withForeignPtr fp $
                GL.glTexImage2D a b c d e f g h . castPtr
        glTexImage2DFloat _ a b c d e f g h (_, fp) = withForeignPtr fp $
                GL.glTexImage2D a b c d e f g h . castPtr
        glTexParameterf = const GL.glTexParameterf
        glTexParameteri = const GL.glTexParameteri
        glTexSubImage2D _ a b c d e f g h (_, fp) = withForeignPtr fp $
                GL.glTexSubImage2D a b c d e f g h . castPtr
        glUniform1f = const GL.glUniform1f
        glUniform1fv = uniform GL.glUniform1fv
        glUniform1i = const GL.glUniform1i
        glUniform1iv = uniform GL.glUniform1iv
        glUniform2f = const GL.glUniform2f
        glUniform2fv = uniform GL.glUniform2fv
        glUniform2i = const GL.glUniform2i
        glUniform2iv = uniform GL.glUniform2iv
        glUniform3f = const GL.glUniform3f
        glUniform3fv = uniform GL.glUniform3fv
        glUniform3i = const GL.glUniform3i
        glUniform3iv = uniform GL.glUniform3iv
        glUniform4f = const GL.glUniform4f
        glUniform4fv = uniform GL.glUniform4fv
        glUniform4i = const GL.glUniform4i
        glUniform4iv = uniform GL.glUniform4iv
        glUniformMatrix2fv = uniformMatrix GL.glUniformMatrix2fv 4
        glUniformMatrix3fv = uniformMatrix GL.glUniformMatrix3fv 9
        glUniformMatrix4fv = uniformMatrix GL.glUniformMatrix4fv 16
        glUseProgram = const GL.glUseProgram
        glValidateProgram = const GL.glValidateProgram
        glVertexAttrib1f = const GL.glVertexAttrib1f
        glVertexAttrib1fv = vertexAttrib GL.glVertexAttrib1fv
        glVertexAttrib2f = const GL.glVertexAttrib2f
        glVertexAttrib2fv = vertexAttrib GL.glVertexAttrib2fv
        glVertexAttrib3f = const GL.glVertexAttrib3f
        glVertexAttrib3fv = vertexAttrib GL.glVertexAttrib3fv
        glVertexAttrib4f = const GL.glVertexAttrib4f
        glVertexAttrib4fv = vertexAttrib GL.glVertexAttrib4fv
        glVertexAttribPointer = const GL.glVertexAttribPointer
        glViewport = const GL.glViewport

        gl_DEPTH_BUFFER_BIT = GL.GL_DEPTH_BUFFER_BIT
        gl_STENCIL_BUFFER_BIT = GL.GL_STENCIL_BUFFER_BIT
        gl_COLOR_BUFFER_BIT = GL.GL_COLOR_BUFFER_BIT
        gl_POINTS = GL.GL_POINTS
        gl_LINES = GL.GL_LINES
        gl_LINE_LOOP = GL.GL_LINE_LOOP
        gl_LINE_STRIP = GL.GL_LINE_STRIP
        gl_TRIANGLES = GL.GL_TRIANGLES
        gl_TRIANGLE_STRIP = GL.GL_TRIANGLE_STRIP
        gl_TRIANGLE_FAN = GL.GL_TRIANGLE_FAN
        gl_ZERO = GL.GL_ZERO
        gl_ONE = GL.GL_ONE
        gl_SRC_COLOR = GL.GL_SRC_COLOR
        gl_ONE_MINUS_SRC_COLOR = GL.GL_ONE_MINUS_SRC_COLOR
        gl_SRC_ALPHA = GL.GL_SRC_ALPHA
        gl_ONE_MINUS_SRC_ALPHA = GL.GL_ONE_MINUS_SRC_ALPHA
        gl_DST_ALPHA = GL.GL_DST_ALPHA
        gl_ONE_MINUS_DST_ALPHA = GL.GL_ONE_MINUS_DST_ALPHA
        gl_DST_COLOR = GL.GL_DST_COLOR
        gl_ONE_MINUS_DST_COLOR = GL.GL_ONE_MINUS_DST_COLOR
        gl_SRC_ALPHA_SATURATE = GL.GL_SRC_ALPHA_SATURATE
        gl_FUNC_ADD = GL.GL_FUNC_ADD
        gl_BLEND_EQUATION = error "GL_BLEND_EQUATION: not present in OpenGL 2.1"
        gl_BLEND_EQUATION_RGB = GL.GL_BLEND_EQUATION_RGB
        gl_BLEND_EQUATION_ALPHA = GL.GL_BLEND_EQUATION_ALPHA
        gl_FUNC_SUBTRACT = GL.GL_FUNC_SUBTRACT
        gl_FUNC_REVERSE_SUBTRACT = GL.GL_FUNC_REVERSE_SUBTRACT
        gl_BLEND_DST_RGB = GL.GL_BLEND_DST_RGB
        gl_BLEND_SRC_RGB = GL.GL_BLEND_SRC_RGB
        gl_BLEND_DST_ALPHA = GL.GL_BLEND_DST_ALPHA
        gl_BLEND_SRC_ALPHA = GL.GL_BLEND_SRC_ALPHA
        gl_CONSTANT_COLOR = GL.GL_CONSTANT_COLOR
        gl_ONE_MINUS_CONSTANT_COLOR = GL.GL_ONE_MINUS_CONSTANT_COLOR
        gl_CONSTANT_ALPHA = GL.GL_CONSTANT_ALPHA
        gl_ONE_MINUS_CONSTANT_ALPHA = GL.GL_ONE_MINUS_CONSTANT_ALPHA
        gl_BLEND_COLOR = GL.GL_BLEND_COLOR_EXT
        gl_ARRAY_BUFFER = GL.GL_ARRAY_BUFFER
        gl_ELEMENT_ARRAY_BUFFER = GL.GL_ELEMENT_ARRAY_BUFFER
        gl_ARRAY_BUFFER_BINDING = GL.GL_ARRAY_BUFFER_BINDING
        gl_ELEMENT_ARRAY_BUFFER_BINDING = GL.GL_ELEMENT_ARRAY_BUFFER_BINDING
        gl_STREAM_DRAW = GL.GL_STREAM_DRAW
        gl_STATIC_DRAW = GL.GL_STATIC_DRAW
        gl_DYNAMIC_DRAW = GL.GL_DYNAMIC_DRAW
        gl_BUFFER_SIZE = GL.GL_BUFFER_SIZE
        gl_BUFFER_USAGE = GL.GL_BUFFER_USAGE
        gl_CURRENT_VERTEX_ATTRIB = GL.GL_CURRENT_VERTEX_ATTRIB
        gl_FRONT = GL.GL_FRONT
        gl_BACK = GL.GL_BACK
        gl_FRONT_AND_BACK = GL.GL_FRONT_AND_BACK
        gl_CULL_FACE = GL.GL_CULL_FACE
        gl_BLEND = GL.GL_BLEND
        gl_DITHER = GL.GL_DITHER
        gl_STENCIL_TEST = GL.GL_STENCIL_TEST
        gl_DEPTH_TEST = GL.GL_DEPTH_TEST
        gl_SCISSOR_TEST = GL.GL_SCISSOR_TEST
        gl_POLYGON_OFFSET_FILL = GL.GL_POLYGON_OFFSET_FILL
        gl_SAMPLE_ALPHA_TO_COVERAGE = GL.GL_SAMPLE_ALPHA_TO_COVERAGE
        gl_SAMPLE_COVERAGE = GL.GL_SAMPLE_COVERAGE
        gl_NO_ERROR = GL.GL_NO_ERROR
        gl_INVALID_ENUM = GL.GL_INVALID_ENUM
        gl_INVALID_VALUE = GL.GL_INVALID_VALUE
        gl_INVALID_OPERATION = GL.GL_INVALID_OPERATION
        gl_OUT_OF_MEMORY = GL.GL_OUT_OF_MEMORY
        gl_CW = GL.GL_CW
        gl_CCW = GL.GL_CCW
        gl_LINE_WIDTH = GL.GL_LINE_WIDTH
        gl_ALIASED_POINT_SIZE_RANGE = error "GL_ALIASED_POINT_SIZE_RANGE: not present in OpenGL 2.1"
        gl_ALIASED_LINE_WIDTH_RANGE = GL.GL_ALIASED_LINE_WIDTH_RANGE
        gl_CULL_FACE_MODE = GL.GL_CULL_FACE_MODE
        gl_FRONT_FACE = GL.GL_FRONT_FACE
        gl_DEPTH_RANGE = GL.GL_DEPTH_RANGE
        gl_DEPTH_WRITEMASK = GL.GL_DEPTH_WRITEMASK
        gl_DEPTH_CLEAR_VALUE = GL.GL_DEPTH_CLEAR_VALUE
        gl_DEPTH_FUNC = GL.GL_DEPTH_FUNC
        gl_STENCIL_CLEAR_VALUE = GL.GL_STENCIL_CLEAR_VALUE
        gl_STENCIL_FUNC = GL.GL_STENCIL_FUNC
        gl_STENCIL_FAIL = GL.GL_STENCIL_FAIL
        gl_STENCIL_PASS_DEPTH_FAIL = GL.GL_STENCIL_PASS_DEPTH_FAIL
        gl_STENCIL_PASS_DEPTH_PASS = GL.GL_STENCIL_PASS_DEPTH_PASS
        gl_STENCIL_REF = GL.GL_STENCIL_REF
        gl_STENCIL_VALUE_MASK = GL.GL_STENCIL_VALUE_MASK
        gl_STENCIL_WRITEMASK = GL.GL_STENCIL_WRITEMASK
        gl_STENCIL_BACK_FUNC = GL.GL_STENCIL_BACK_FUNC
        gl_STENCIL_BACK_FAIL = GL.GL_STENCIL_BACK_FAIL
        gl_STENCIL_BACK_PASS_DEPTH_FAIL = GL.GL_STENCIL_BACK_PASS_DEPTH_FAIL
        gl_STENCIL_BACK_PASS_DEPTH_PASS = GL.GL_STENCIL_BACK_PASS_DEPTH_PASS
        gl_STENCIL_BACK_REF = GL.GL_STENCIL_BACK_REF
        gl_STENCIL_BACK_VALUE_MASK = GL.GL_STENCIL_BACK_VALUE_MASK
        gl_STENCIL_BACK_WRITEMASK = GL.GL_STENCIL_BACK_WRITEMASK
        gl_VIEWPORT = GL.GL_VIEWPORT
        gl_SCISSOR_BOX = GL.GL_SCISSOR_BOX
        gl_COLOR_CLEAR_VALUE = GL.GL_COLOR_CLEAR_VALUE
        gl_COLOR_WRITEMASK = GL.GL_COLOR_WRITEMASK
        gl_UNPACK_ALIGNMENT = GL.GL_UNPACK_ALIGNMENT
        gl_PACK_ALIGNMENT = GL.GL_PACK_ALIGNMENT
        gl_MAX_TEXTURE_SIZE = GL.GL_MAX_TEXTURE_SIZE
        gl_MAX_VIEWPORT_DIMS = GL.GL_MAX_VIEWPORT_DIMS
        gl_SUBPIXEL_BITS = GL.GL_SUBPIXEL_BITS
        gl_RED_BITS = error "GL_RED_BITS: not present in OpenGL 2.1"
        gl_GREEN_BITS = error "GL_GREEN_BITS: not present in OpenGL 2.1"
        gl_BLUE_BITS = error "GL_BLUE_BITS: not present in OpenGL 2.1"
        gl_ALPHA_BITS = error "GL_ALPHA_BITS: not present in OpenGL 2.1"
        gl_DEPTH_BITS = error "GL_DEPTH_BITS: not present in OpenGL 2.1"
        gl_STENCIL_BITS = error "GL_STENCIL_BITS: not present in OpenGL 2.1"
        gl_POLYGON_OFFSET_UNITS = GL.GL_POLYGON_OFFSET_UNITS
        gl_POLYGON_OFFSET_FACTOR = GL.GL_POLYGON_OFFSET_FACTOR
        gl_TEXTURE_BINDING_2D = GL.GL_TEXTURE_BINDING_2D
        gl_SAMPLE_BUFFERS = GL.GL_SAMPLE_BUFFERS
        gl_SAMPLES = GL.GL_SAMPLES
        gl_SAMPLE_COVERAGE_VALUE = GL.GL_SAMPLE_COVERAGE_VALUE
        gl_SAMPLE_COVERAGE_INVERT = GL.GL_SAMPLE_COVERAGE_INVERT
        gl_COMPRESSED_TEXTURE_FORMATS = GL.GL_COMPRESSED_TEXTURE_FORMATS
        gl_DONT_CARE = GL.GL_DONT_CARE
        gl_FASTEST = GL.GL_FASTEST
        gl_NICEST = GL.GL_NICEST
        gl_GENERATE_MIPMAP_HINT = error "GL_GENERATE_MIPMAP_HINT: not present in OpenGL 2.1"
        gl_BYTE = GL.GL_BYTE
        gl_UNSIGNED_BYTE = GL.GL_UNSIGNED_BYTE
        gl_SHORT = GL.GL_SHORT
        gl_UNSIGNED_SHORT = GL.GL_UNSIGNED_SHORT
        gl_INT = GL.GL_INT
        gl_UNSIGNED_INT = GL.GL_UNSIGNED_INT
        gl_UNSIGNED_INT_24_8 = GL.GL_UNSIGNED_INT_24_8
        gl_FLOAT = GL.GL_FLOAT
        gl_DEPTH_COMPONENT = GL.GL_DEPTH_COMPONENT
        gl_DEPTH_STENCIL = GL.GL_DEPTH_STENCIL
        gl_ALPHA = GL.GL_ALPHA
        gl_RGB = GL.GL_RGB
        gl_RGBA = GL.GL_RGBA
        gl_RGBA32F = GL.GL_RGBA32F_ARB
        gl_LUMINANCE = error "GL_LUMINANCE: not present in OpenGL 2.1"
        gl_LUMINANCE_ALPHA = error "GL_LUMINANCE_ALPHA: not present in OpenGL 2.1"
        gl_UNSIGNED_SHORT_4_4_4_4 = GL.GL_UNSIGNED_SHORT_4_4_4_4
        gl_UNSIGNED_SHORT_5_5_5_1 = GL.GL_UNSIGNED_SHORT_5_5_5_1
        gl_UNSIGNED_SHORT_5_6_5 = GL.GL_UNSIGNED_SHORT_5_6_5
        gl_FRAGMENT_SHADER = GL.GL_FRAGMENT_SHADER
        gl_VERTEX_SHADER = GL.GL_VERTEX_SHADER
        gl_MAX_VERTEX_ATTRIBS = GL.GL_MAX_VERTEX_ATTRIBS
        gl_MAX_VERTEX_UNIFORM_VECTORS = error "GL_MAX_VERTEX_UNIFORM_VECTORS: not present in OpenGL 2.1"
        gl_MAX_VARYING_VECTORS = error "GL_MAX_VARYING_VECTORS: not present in OpenGL 2.1"
        gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS = GL.GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
        gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS = GL.GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
        gl_MAX_TEXTURE_IMAGE_UNITS = GL.GL_MAX_TEXTURE_IMAGE_UNITS
        gl_MAX_FRAGMENT_UNIFORM_VECTORS = error "GL_MAX_FRAGMENT_UNIFORM_VECTORS: not present in OpenGL 2.1"
        gl_SHADER_TYPE = GL.GL_SHADER_TYPE
        gl_DELETE_STATUS = GL.GL_DELETE_STATUS
        gl_LINK_STATUS = GL.GL_LINK_STATUS
        gl_VALIDATE_STATUS = GL.GL_VALIDATE_STATUS
        gl_ATTACHED_SHADERS = GL.GL_ATTACHED_SHADERS
        gl_ACTIVE_UNIFORMS = GL.GL_ACTIVE_UNIFORMS
        gl_ACTIVE_ATTRIBUTES = GL.GL_ACTIVE_ATTRIBUTES
        gl_SHADING_LANGUAGE_VERSION = GL.GL_SHADING_LANGUAGE_VERSION
        gl_CURRENT_PROGRAM = GL.GL_CURRENT_PROGRAM
        gl_NEVER = GL.GL_NEVER
        gl_LESS = GL.GL_LESS
        gl_EQUAL = GL.GL_EQUAL
        gl_LEQUAL = GL.GL_LEQUAL
        gl_GREATER = GL.GL_GREATER
        gl_NOTEQUAL = GL.GL_NOTEQUAL
        gl_GEQUAL = GL.GL_GEQUAL
        gl_ALWAYS = GL.GL_ALWAYS
        gl_KEEP = GL.GL_KEEP
        gl_REPLACE = GL.GL_REPLACE
        gl_INCR = GL.GL_INCR
        gl_DECR = GL.GL_DECR
        gl_INVERT = GL.GL_INVERT
        gl_INCR_WRAP = GL.GL_INCR_WRAP
        gl_DECR_WRAP = GL.GL_DECR_WRAP
        gl_VENDOR = GL.GL_VENDOR
        gl_RENDERER = GL.GL_RENDERER
        gl_VERSION = GL.GL_VERSION
        gl_NEAREST = GL.GL_NEAREST
        gl_LINEAR = GL.GL_LINEAR
        gl_NEAREST_MIPMAP_NEAREST = GL.GL_NEAREST_MIPMAP_NEAREST
        gl_LINEAR_MIPMAP_NEAREST = GL.GL_LINEAR_MIPMAP_NEAREST
        gl_NEAREST_MIPMAP_LINEAR = GL.GL_NEAREST_MIPMAP_LINEAR
        gl_LINEAR_MIPMAP_LINEAR = GL.GL_LINEAR_MIPMAP_LINEAR
        gl_TEXTURE_MAG_FILTER = GL.GL_TEXTURE_MAG_FILTER
        gl_TEXTURE_MIN_FILTER = GL.GL_TEXTURE_MIN_FILTER
        gl_TEXTURE_WRAP_S = GL.GL_TEXTURE_WRAP_S
        gl_TEXTURE_WRAP_T = GL.GL_TEXTURE_WRAP_T
        gl_TEXTURE_2D = GL.GL_TEXTURE_2D
        gl_TEXTURE = GL.GL_TEXTURE
        gl_TEXTURE_CUBE_MAP = GL.GL_TEXTURE_CUBE_MAP
        gl_TEXTURE_BINDING_CUBE_MAP = GL.GL_TEXTURE_BINDING_CUBE_MAP
        gl_TEXTURE_CUBE_MAP_POSITIVE_X = GL.GL_TEXTURE_CUBE_MAP_POSITIVE_X
        gl_TEXTURE_CUBE_MAP_NEGATIVE_X = GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_X
        gl_TEXTURE_CUBE_MAP_POSITIVE_Y = GL.GL_TEXTURE_CUBE_MAP_POSITIVE_Y
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Y = GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
        gl_TEXTURE_CUBE_MAP_POSITIVE_Z = GL.GL_TEXTURE_CUBE_MAP_POSITIVE_Z
        gl_TEXTURE_CUBE_MAP_NEGATIVE_Z = GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
        gl_MAX_CUBE_MAP_TEXTURE_SIZE = GL.GL_MAX_CUBE_MAP_TEXTURE_SIZE
        gl_TEXTURE0 = GL.GL_TEXTURE0
        gl_TEXTURE1 = GL.GL_TEXTURE1
        gl_TEXTURE2 = GL.GL_TEXTURE2
        gl_TEXTURE3 = GL.GL_TEXTURE3
        gl_TEXTURE4 = GL.GL_TEXTURE4
        gl_TEXTURE5 = GL.GL_TEXTURE5
        gl_TEXTURE6 = GL.GL_TEXTURE6
        gl_TEXTURE7 = GL.GL_TEXTURE7
        gl_TEXTURE8 = GL.GL_TEXTURE8
        gl_TEXTURE9 = GL.GL_TEXTURE9
        gl_TEXTURE10 = GL.GL_TEXTURE10
        gl_TEXTURE11 = GL.GL_TEXTURE11
        gl_TEXTURE12 = GL.GL_TEXTURE12
        gl_TEXTURE13 = GL.GL_TEXTURE13
        gl_TEXTURE14 = GL.GL_TEXTURE14
        gl_TEXTURE15 = GL.GL_TEXTURE15
        gl_TEXTURE16 = GL.GL_TEXTURE16
        gl_TEXTURE17 = GL.GL_TEXTURE17
        gl_TEXTURE18 = GL.GL_TEXTURE18
        gl_TEXTURE19 = GL.GL_TEXTURE19
        gl_TEXTURE20 = GL.GL_TEXTURE20
        gl_TEXTURE21 = GL.GL_TEXTURE21
        gl_TEXTURE22 = GL.GL_TEXTURE22
        gl_TEXTURE23 = GL.GL_TEXTURE23
        gl_TEXTURE24 = GL.GL_TEXTURE24
        gl_TEXTURE25 = GL.GL_TEXTURE25
        gl_TEXTURE26 = GL.GL_TEXTURE26
        gl_TEXTURE27 = GL.GL_TEXTURE27
        gl_TEXTURE28 = GL.GL_TEXTURE28
        gl_TEXTURE29 = GL.GL_TEXTURE29
        gl_TEXTURE30 = GL.GL_TEXTURE30
        gl_TEXTURE31 = GL.GL_TEXTURE31
        gl_ACTIVE_TEXTURE = GL.GL_ACTIVE_TEXTURE
        gl_REPEAT = GL.GL_REPEAT
        gl_CLAMP_TO_EDGE = GL.GL_CLAMP_TO_EDGE
        gl_MIRRORED_REPEAT = GL.GL_MIRRORED_REPEAT
        gl_FLOAT_VEC2 = GL.GL_FLOAT_VEC2
        gl_FLOAT_VEC3 = GL.GL_FLOAT_VEC3
        gl_FLOAT_VEC4 = GL.GL_FLOAT_VEC4
        gl_INT_VEC2 = GL.GL_INT_VEC2
        gl_INT_VEC3 = GL.GL_INT_VEC3
        gl_INT_VEC4 = GL.GL_INT_VEC4
        gl_BOOL = GL.GL_BOOL
        gl_BOOL_VEC2 = GL.GL_BOOL_VEC2
        gl_BOOL_VEC3 = GL.GL_BOOL_VEC3
        gl_BOOL_VEC4 = GL.GL_BOOL_VEC4
        gl_FLOAT_MAT2 = GL.GL_FLOAT_MAT2
        gl_FLOAT_MAT3 = GL.GL_FLOAT_MAT3
        gl_FLOAT_MAT4 = GL.GL_FLOAT_MAT4
        gl_SAMPLER_2D = GL.GL_SAMPLER_2D
        gl_SAMPLER_CUBE = GL.GL_SAMPLER_CUBE
        gl_VERTEX_ATTRIB_ARRAY_ENABLED = GL.GL_VERTEX_ATTRIB_ARRAY_ENABLED
        gl_VERTEX_ATTRIB_ARRAY_SIZE = GL.GL_VERTEX_ATTRIB_ARRAY_SIZE
        gl_VERTEX_ATTRIB_ARRAY_STRIDE = GL.GL_VERTEX_ATTRIB_ARRAY_STRIDE
        gl_VERTEX_ATTRIB_ARRAY_TYPE = GL.GL_VERTEX_ATTRIB_ARRAY_TYPE
        gl_VERTEX_ATTRIB_ARRAY_NORMALIZED = GL.GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
        gl_VERTEX_ATTRIB_ARRAY_POINTER = GL.GL_VERTEX_ATTRIB_ARRAY_POINTER
        gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = GL.GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
        gl_COMPILE_STATUS = GL.GL_COMPILE_STATUS
        gl_LOW_FLOAT = error "GL_LOW_FLOAT: not present in OpenGL 2.1"
        gl_MEDIUM_FLOAT = error "GL_MEDIUM_FLOAT: not present in OpenGL 2.1"
        gl_HIGH_FLOAT = error "GL_HIGH_FLOAT: not present in OpenGL 2.1"
        gl_LOW_INT = error "GL_LOW_INT: not present in OpenGL 2.1"
        gl_MEDIUM_INT = error "GL_MEDIUM_INT: not present in OpenGL 2.1"
        gl_HIGH_INT = error "GL_HIGH_INT: not present in OpenGL 2.1"
        gl_FRAMEBUFFER = GL.GL_FRAMEBUFFER
        gl_RENDERBUFFER = GL.GL_RENDERBUFFER
        gl_RGBA4 = GL.GL_RGBA4
        gl_RGB5_A1 = GL.GL_RGB5_A1
        gl_RGB565 = error "GL_RGB565: not present in OpenGL 2.1"
        gl_DEPTH_COMPONENT16 = GL.GL_DEPTH_COMPONENT16
        gl_STENCIL_INDEX8 = GL.GL_STENCIL_INDEX8
        gl_RENDERBUFFER_WIDTH = GL.GL_RENDERBUFFER_WIDTH
        gl_RENDERBUFFER_HEIGHT = GL.GL_RENDERBUFFER_HEIGHT
        gl_RENDERBUFFER_INTERNAL_FORMAT = GL.GL_RENDERBUFFER_INTERNAL_FORMAT
        gl_RENDERBUFFER_RED_SIZE = GL.GL_RENDERBUFFER_RED_SIZE
        gl_RENDERBUFFER_GREEN_SIZE = GL.GL_RENDERBUFFER_GREEN_SIZE
        gl_RENDERBUFFER_BLUE_SIZE = GL.GL_RENDERBUFFER_BLUE_SIZE
        gl_RENDERBUFFER_ALPHA_SIZE = GL.GL_RENDERBUFFER_ALPHA_SIZE
        gl_RENDERBUFFER_DEPTH_SIZE = GL.GL_RENDERBUFFER_DEPTH_SIZE
        gl_RENDERBUFFER_STENCIL_SIZE = GL.GL_RENDERBUFFER_STENCIL_SIZE
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
        gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = GL.GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
        gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = GL.GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
        gl_MAX_DRAW_BUFFERS = GL.GL_MAX_DRAW_BUFFERS
        gl_DRAW_BUFFER0 = GL.GL_DRAW_BUFFER0
        gl_DRAW_BUFFER1 = GL.GL_DRAW_BUFFER1
        gl_DRAW_BUFFER2 = GL.GL_DRAW_BUFFER2
        gl_DRAW_BUFFER3 = GL.GL_DRAW_BUFFER3
        gl_DRAW_BUFFER4 = GL.GL_DRAW_BUFFER4
        gl_DRAW_BUFFER5 = GL.GL_DRAW_BUFFER5
        gl_DRAW_BUFFER6 = GL.GL_DRAW_BUFFER6
        gl_DRAW_BUFFER7 = GL.GL_DRAW_BUFFER7
        gl_DRAW_BUFFER8 = GL.GL_DRAW_BUFFER8
        gl_DRAW_BUFFER9 = GL.GL_DRAW_BUFFER9
        gl_DRAW_BUFFER10 = GL.GL_DRAW_BUFFER10
        gl_DRAW_BUFFER11 = GL.GL_DRAW_BUFFER11
        gl_DRAW_BUFFER12 = GL.GL_DRAW_BUFFER12
        gl_DRAW_BUFFER13 = GL.GL_DRAW_BUFFER13
        gl_DRAW_BUFFER14 = GL.GL_DRAW_BUFFER14
        gl_DRAW_BUFFER15 = GL.GL_DRAW_BUFFER15
        gl_MAX_COLOR_ATTACHMENTS = GL.GL_MAX_COLOR_ATTACHMENTS
        gl_COLOR_ATTACHMENT0 = GL.GL_COLOR_ATTACHMENT0
        gl_COLOR_ATTACHMENT1 = GL.GL_COLOR_ATTACHMENT1
        gl_COLOR_ATTACHMENT2 = GL.GL_COLOR_ATTACHMENT2
        gl_COLOR_ATTACHMENT3 = GL.GL_COLOR_ATTACHMENT3
        gl_COLOR_ATTACHMENT4 = GL.GL_COLOR_ATTACHMENT4
        gl_COLOR_ATTACHMENT5 = GL.GL_COLOR_ATTACHMENT5
        gl_COLOR_ATTACHMENT6 = GL.GL_COLOR_ATTACHMENT6
        gl_COLOR_ATTACHMENT7 = GL.GL_COLOR_ATTACHMENT7
        gl_COLOR_ATTACHMENT8 = GL.GL_COLOR_ATTACHMENT8
        gl_COLOR_ATTACHMENT9 = GL.GL_COLOR_ATTACHMENT9
        gl_COLOR_ATTACHMENT10 = GL.GL_COLOR_ATTACHMENT10
        gl_COLOR_ATTACHMENT11 = GL.GL_COLOR_ATTACHMENT11
        gl_COLOR_ATTACHMENT12 = GL.GL_COLOR_ATTACHMENT12
        gl_COLOR_ATTACHMENT13 = GL.GL_COLOR_ATTACHMENT13
        gl_COLOR_ATTACHMENT14 = GL.GL_COLOR_ATTACHMENT14
        gl_COLOR_ATTACHMENT15 = GL.GL_COLOR_ATTACHMENT15
        gl_DEPTH_ATTACHMENT = GL.GL_DEPTH_ATTACHMENT
        gl_STENCIL_ATTACHMENT = GL.GL_STENCIL_ATTACHMENT
        gl_DEPTH_STENCIL_ATTACHMENT = GL.GL_DEPTH_STENCIL_ATTACHMENT
        gl_NONE = GL.GL_NONE
        gl_FRAMEBUFFER_COMPLETE = GL.GL_FRAMEBUFFER_COMPLETE
        gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = GL.GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
        gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = GL.GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
        gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = error "GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS: not present in OpenGL 2.1"
        gl_FRAMEBUFFER_UNSUPPORTED = GL.GL_FRAMEBUFFER_UNSUPPORTED
        gl_FRAMEBUFFER_BINDING = GL.GL_FRAMEBUFFER_BINDING
        gl_RENDERBUFFER_BINDING = GL.GL_RENDERBUFFER_BINDING
        gl_MAX_RENDERBUFFER_SIZE = GL.GL_MAX_RENDERBUFFER_SIZE
        gl_INVALID_FRAMEBUFFER_OPERATION = GL.GL_INVALID_FRAMEBUFFER_OPERATION
