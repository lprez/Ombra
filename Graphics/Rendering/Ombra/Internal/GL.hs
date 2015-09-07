{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Ombra.Internal.GL (
        GL,
        ActiveTexture(..),
        module Graphics.Rendering.Ombra.Backend,
        liftIO,
        evalGL,
        forkGL,
        asyncGL,
        getCtx,
        activeTexture,
        attachShader,
        bindAttribLocation,
        bindBuffer,
        bindFramebuffer,
        bindRenderbuffer,
        bindTexture,
        bindVertexArray,
        blendColor,
        blendEquation,
        blendEquationSeparate,
        blendFunc,
        blendFuncSeparate,
        bufferData,
        bufferSubData,
        checkFramebufferStatus,
        clear,
        clearColor,
        clearDepth,
        clearStencil,
        colorMask,
        compileShader,
        compressedTexImage2D,
        compressedTexSubImage2D,
        copyTexImage2D,
        copyTexSubImage2D,
        createBuffer,
        createFramebuffer,
        createProgram,
        createRenderbuffer,
        createShader,
        createTexture,
        createVertexArray,
        cullFace,
        deleteBuffer,
        deleteFramebuffer,
        deleteProgram,
        deleteRenderbuffer,
        deleteShader,
        deleteTexture,
        deleteVertexArray,
        depthFunc,
        depthMask,
        depthRange,
        detachShader,
        disable,
        disableVertexAttribArray,
        drawArrays,
        drawElements,
        enable,
        enableVertexAttribArray,
        finish,
        flush,
        framebufferRenderbuffer,
        framebufferTexture2D,
        frontFace,
        generateMipmap,
        getAttribLocation,
        getError,
        getProgramInfoLog,
        -- getShaderPrecisionFormat,
        getShaderInfoLog,
        getShaderSource,
        getUniformLocation,
        hint,
        isBuffer,
        isEnabled,
        isFramebuffer,
        isProgram,
        isRenderbuffer,
        isShader,
        isTexture,
        isVertexArray,
        lineWidth,
        linkProgram,
        pixelStorei,
        polygonOffset,
        readPixels,
        renderbufferStorage,
        sampleCoverage,
        scissor,
        shaderSource,
        stencilFunc,
        stencilFuncSeparate,
        stencilMask,
        stencilMaskSeparate,
        stencilOp,
        stencilOpSeparate,
        texImage2D,
        texParameterf,
        texParameteri,
        texSubImage2D,
        uniform1f,
        uniform1fv,
        uniform1i,
        uniform1iv,
        uniform2f,
        uniform2fv,
        uniform2i,
        uniform2iv,
        uniform3f,
        uniform3fv,
        uniform3i,
        uniform3iv,
        uniform4f,
        uniform4fv,
        uniform4i,
        uniform4iv,
        uniformMatrix2fv,
        uniformMatrix3fv,
        uniformMatrix4fv,
        useProgram,
        validateProgram,
        vertexAttrib1f,
        vertexAttrib1fv,
        vertexAttrib2f,
        vertexAttrib2fv,
        vertexAttrib3f,
        vertexAttrib3fv,
        vertexAttrib4f,
        vertexAttrib4fv,
        vertexAttribPointer,
        viewport
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Int (Int32)
import Data.Word
import Graphics.Rendering.Ombra.Backend
        
-- TODO: context loss
newtype GL a = GL (StateT Ctx IO a)
        deriving (Functor, Applicative, Monad, MonadIO)

newtype ActiveTexture = ActiveTexture Word

evalGL :: GL a -> Ctx -> IO a
evalGL (GL m) = evalStateT m

forkGL :: GLES => GL () -> GL ThreadId
forkGL a = getCtx >>= \ctx ->
        liftIO . safeFork ctx forkIO $ evalGL a ctx

asyncGL :: GLES => GL a -> (a -> GL ()) -> GL ()
asyncGL r f = forkGL (r >>= f) >> return ()

getCtx :: GLES => GL Ctx
getCtx = GL get

activeTexture :: GLES => GLEnum -> GL ()
activeTexture a = GL get >>= \ctx -> liftIO $ glActiveTexture ctx a

attachShader :: GLES => Program -> Shader -> GL ()
attachShader a b = GL get >>= \ctx -> liftIO $ glAttachShader ctx a b

bindAttribLocation :: GLES => Program -> GLUInt -> GLString -> GL ()
bindAttribLocation a b c = GL get >>= \ctx -> liftIO $ glBindAttribLocation ctx a b c

bindBuffer :: GLES => GLEnum -> Buffer -> GL ()
bindBuffer a b = GL get >>= \ctx -> liftIO $ glBindBuffer ctx a b

bindFramebuffer :: GLES => GLEnum -> FrameBuffer -> GL ()
bindFramebuffer a b = GL get >>= \ctx -> liftIO $ glBindFramebuffer ctx a b

bindRenderbuffer :: GLES => GLEnum -> RenderBuffer -> GL ()
bindRenderbuffer a b = GL get >>= \ctx -> liftIO $ glBindRenderbuffer ctx a b

bindTexture :: GLES => GLEnum -> Texture -> GL ()
bindTexture a b = GL get >>= \ctx -> liftIO $ glBindTexture ctx a b

bindVertexArray :: GLES => VertexArrayObject -> GL ()
bindVertexArray a = GL get >>= \ctx -> liftIO $ glBindVertexArray ctx a

blendColor :: GLES => Float -> Float -> Float -> Float -> GL ()
blendColor a b c d = GL get >>= \ctx -> liftIO $ glBlendColor ctx a b c d

blendEquation :: GLES => GLEnum -> GL ()
blendEquation a = GL get >>= \ctx -> liftIO $ glBlendEquation ctx a

blendEquationSeparate :: GLES => GLEnum -> GLEnum -> GL ()
blendEquationSeparate a b = GL get >>= \ctx -> liftIO $ glBlendEquationSeparate ctx a b

blendFunc :: GLES => GLEnum -> GLEnum -> GL ()
blendFunc a b = GL get >>= \ctx -> liftIO $ glBlendFunc ctx a b

blendFuncSeparate :: GLES => GLEnum -> GLEnum -> GLEnum -> GLEnum -> GL ()
blendFuncSeparate a b c d = GL get >>= \ctx -> liftIO $ glBlendFuncSeparate ctx a b c d

bufferData :: GLES => GLEnum -> Array -> GLEnum -> GL ()
bufferData a b c = GL get >>= \ctx -> liftIO $ glBufferData ctx a b c

bufferSubData :: GLES => GLEnum -> GLPtrDiff -> Array -> GL ()
bufferSubData a b c = GL get >>= \ctx -> liftIO $ glBufferSubData ctx a b c

checkFramebufferStatus :: GLES => GLEnum -> GL GLEnum
checkFramebufferStatus a = GL get >>= \ctx -> liftIO $ glCheckFramebufferStatus ctx a

clear :: GLES => GLEnum -> GL ()
clear a = GL get >>= \ctx -> liftIO $ glClear ctx a

clearColor :: GLES => Float -> Float -> Float -> Float -> GL ()
clearColor a b c d = GL get >>= \ctx -> liftIO $ glClearColor ctx a b c d

clearDepth :: GLES => Float -> GL ()
clearDepth a = GL get >>= \ctx -> liftIO $ glClearDepth ctx a

clearStencil :: GLES => GLInt -> GL ()
clearStencil a = GL get >>= \ctx -> liftIO $ glClearStencil ctx a

colorMask :: GLES => GLBool -> GLBool -> GLBool -> GLBool -> GL ()
colorMask a b c d = GL get >>= \ctx -> liftIO $ glColorMask ctx a b c d

compileShader :: GLES => Shader -> GL ()
compileShader a = GL get >>= \ctx -> liftIO $ glCompileShader ctx a

compressedTexImage2D :: GLES => GLEnum -> GLInt -> GLEnum -> GLSize -> GLSize -> GLInt -> Array -> GL ()
compressedTexImage2D a b c d e f g = GL get >>= \ctx -> liftIO $ glCompressedTexImage2D ctx a b c d e f g

compressedTexSubImage2D :: GLES => GLEnum -> GLInt -> GLInt -> GLInt -> GLSize -> GLSize -> GLEnum -> Array -> GL ()
compressedTexSubImage2D a b c d e f g h = GL get >>= \ctx -> liftIO $ glCompressedTexSubImage2D ctx a b c d e f g h

copyTexImage2D :: GLES => GLEnum -> GLInt -> GLEnum -> GLInt -> GLInt -> GLSize -> GLSize -> GLInt -> GL ()
copyTexImage2D a b c d e f g h = GL get >>= \ctx -> liftIO $ glCopyTexImage2D ctx a b c d e f g h

copyTexSubImage2D :: GLES => GLEnum -> GLInt -> GLInt -> GLInt -> GLInt -> GLInt -> GLSize -> GLSize -> GL ()
copyTexSubImage2D a b c d e f g h = GL get >>= \ctx -> liftIO $ glCopyTexSubImage2D ctx a b c d e f g h

createBuffer :: GLES => GL Buffer
createBuffer = GL get >>= liftIO . glCreateBuffer

createFramebuffer :: GLES => GL FrameBuffer
createFramebuffer = GL get >>= liftIO . glCreateFramebuffer

createProgram :: GLES => GL Program
createProgram = GL get >>= liftIO . glCreateProgram

createRenderbuffer :: GLES => GL RenderBuffer
createRenderbuffer = GL get >>= liftIO . glCreateRenderbuffer

createShader :: GLES => GLEnum -> GL Shader
createShader a = GL get >>= \ctx -> liftIO $ glCreateShader ctx a

createTexture :: GLES => GL Texture
createTexture = GL get >>= liftIO . glCreateTexture

createVertexArray :: GLES => GL VertexArrayObject
createVertexArray = GL get >>= liftIO . glCreateVertexArray

cullFace :: GLES => GLEnum -> GL ()
cullFace a = GL get >>= \ctx -> liftIO $ glCullFace ctx a

deleteBuffer :: GLES => Buffer -> GL ()
deleteBuffer a = GL get >>= \ctx -> liftIO $ glDeleteBuffer ctx a

deleteFramebuffer :: GLES => FrameBuffer -> GL ()
deleteFramebuffer a = GL get >>= \ctx -> liftIO $ glDeleteFramebuffer ctx a

deleteProgram :: GLES => Program -> GL ()
deleteProgram a = GL get >>= \ctx -> liftIO $ glDeleteProgram ctx a

deleteRenderbuffer :: GLES => RenderBuffer -> GL ()
deleteRenderbuffer a = GL get >>= \ctx -> liftIO $ glDeleteRenderbuffer ctx a

deleteShader :: GLES => Shader -> GL ()
deleteShader a = GL get >>= \ctx -> liftIO $ glDeleteShader ctx a

deleteVertexArray :: GLES => VertexArrayObject -> GL ()
deleteVertexArray a = GL get >>= \ctx -> liftIO $ glDeleteVertexArray ctx a

deleteTexture :: GLES => Texture -> GL ()
deleteTexture a = GL get >>= \ctx -> liftIO $ glDeleteTexture ctx a

depthFunc :: GLES => GLEnum -> GL ()
depthFunc a = GL get >>= \ctx -> liftIO $ glDepthFunc ctx a

depthMask :: GLES => GLBool -> GL ()
depthMask a = GL get >>= \ctx -> liftIO $ glDepthMask ctx a

depthRange :: GLES => Float -> Float -> GL ()
depthRange a b = GL get >>= \ctx -> liftIO $ glDepthRange ctx a b

detachShader :: GLES => Program -> Shader -> GL ()
detachShader a b = GL get >>= \ctx -> liftIO $ glDetachShader ctx a b

disable :: GLES => GLEnum -> GL ()
disable a = GL get >>= \ctx -> liftIO $ glDisable ctx a

disableVertexAttribArray :: GLES => GLUInt -> GL ()
disableVertexAttribArray a = GL get >>= \ctx -> liftIO $ glDisableVertexAttribArray ctx a

drawArrays :: GLES => GLEnum -> GLInt -> GLSize -> GL ()
drawArrays a b c = GL get >>= \ctx -> liftIO $ glDrawArrays ctx a b c

drawElements :: GLES => GLEnum -> GLSize -> GLEnum -> GLPtr -> GL ()
drawElements a b c d = GL get >>= \ctx -> liftIO $ glDrawElements ctx a b c d

enable :: GLES => GLEnum -> GL ()
enable a = GL get >>= \ctx -> liftIO $ glEnable ctx a

enableVertexAttribArray :: GLES => GLUInt -> GL ()
enableVertexAttribArray a = GL get >>= \ctx -> liftIO $ glEnableVertexAttribArray ctx a

finish :: GLES => GL ()
finish = GL get >>= liftIO . glFinish

flush :: GLES => GL ()
flush = GL get >>= liftIO . glFlush

framebufferRenderbuffer :: GLES => GLEnum -> GLEnum -> GLEnum -> RenderBuffer -> GL ()
framebufferRenderbuffer a b c d = GL get >>= \ctx -> liftIO $ glFramebufferRenderbuffer ctx a b c d

framebufferTexture2D :: GLES => GLEnum -> GLEnum -> GLEnum -> Texture -> GLInt -> GL ()
framebufferTexture2D a b c d e = GL get >>= \ctx -> liftIO $ glFramebufferTexture2D ctx a b c d e

frontFace :: GLES => GLEnum -> GL ()
frontFace a = GL get >>= \ctx -> liftIO $ glFrontFace ctx a

generateMipmap :: GLES => GLEnum -> GL ()
generateMipmap a = GL get >>= \ctx -> liftIO $ glGenerateMipmap ctx a

-- glGetActiveAttrib :: GLES => Program -> GLEnum -> GL ActiveInfo
-- getActiveAttrib a b = GL get >>= \ctx -> liftIO $ glGetActiveAttrib ctx a b

-- glGetActiveUniform :: GLES => Program -> GLEnum -> GL ActiveInfo
-- getActiveUniform a b = GL get >>= \ctx -> liftIO $ glGetActiveUniform ctx a b

getAttribLocation :: GLES => Program -> GLString -> GL GLInt
getAttribLocation a b = GL get >>= \ctx -> liftIO $ glGetAttribLocation ctx a b

-- glGetBufferParameter :: GLES => Word -> Word -> GL (JSRef a)
-- getBufferParameter a b = GL get >>= \ctx -> liftIO $ glGetBufferParameter ctx a b

-- glGetParameter :: GLES => Word -> GL (JSRef a)
-- getParameter a = GL get >>= \ctx -> liftIO $ glGetParameter ctx a

getError :: GLES => GL GLEnum
getError = GL get >>= liftIO . glGetError

-- glGetFramebufferAttachmentParameter :: GLES => GLEnum -> GLEnum -> GL Word
-- getFramebufferAttachmentParameter a b = GL get >>= \ctx -> liftIO $ glGetFramebufferAttachmentParameter ctx a b

getProgramInfoLog :: GLES => Program -> GL GLString
getProgramInfoLog a = GL get >>= \ctx -> liftIO $ glGetProgramInfoLog ctx a

-- glGetRenderbufferParameter :: GLES => Word -> Word -> GL (JSRef a)
-- getRenderbufferParameter a b = GL get >>= \ctx -> liftIO $ glGetRenderbufferParameter ctx a b

-- glGetShaderParameter :: GLES => Shader -> Word -> GL (JSRef a)
-- getShaderParameter a b = GL get >>= \ctx -> liftIO $ glGetShaderParameter ctx a b

-- getShaderPrecisionFormat :: GLES => GLEnum -> GLEnum -> GL ShaderPrecisionFormat
-- getShaderPrecisionFormat a b = GL get >>= \ctx -> liftIO $ glGetShaderPrecisionFormat ctx a b

getShaderInfoLog :: GLES => Shader -> GL GLString
getShaderInfoLog a = GL get >>= \ctx -> liftIO $ glGetShaderInfoLog ctx a

getShaderSource :: GLES => Shader -> GL GLString
getShaderSource a = GL get >>= \ctx -> liftIO $ glGetShaderSource ctx a

-- glGetTexParameter :: GLES => Word -> Word -> GL (JSRef a)
-- getTexParameter a b = GL get >>= \ctx -> liftIO $ glGetTexParameter ctx a b

-- glGetUniform :: GLES => Program -> UniformLocation -> GL (JSRef a)
-- getUniform a b = GL get >>= \ctx -> liftIO $ glGetUniform ctx a b

getUniformLocation :: GLES => Program -> GLString -> GL UniformLocation
getUniformLocation a b = GL get >>= \ctx -> liftIO $ glGetUniformLocation ctx a b

-- glGetVertexAttrib :: GLES => Word -> Word -> GL (JSRef a)
-- getVertexAttrib a b = GL get >>= \ctx -> liftIO $ glGetVertexAttrib ctx a b

-- glGetVertexAttribOffset :: GLES => Word -> GLEnum -> GL Word
-- getVertexAttribOffset a b = GL get >>= \ctx -> liftIO $ glGetVertexAttribOffset ctx a b

hint :: GLES => GLEnum -> GLEnum -> GL ()
hint a b = GL get >>= \ctx -> liftIO $ glHint ctx a b

isBuffer :: GLES => Buffer -> GL GLBool
isBuffer a = GL get >>= \ctx -> liftIO $ glIsBuffer ctx a

isEnabled :: GLES => GLEnum -> GL GLBool
isEnabled a = GL get >>= \ctx -> liftIO $ glIsEnabled ctx a

isFramebuffer :: GLES => FrameBuffer -> GL GLBool
isFramebuffer a = GL get >>= \ctx -> liftIO $ glIsFramebuffer ctx a

isProgram :: GLES => Program -> GL GLBool
isProgram a = GL get >>= \ctx -> liftIO $ glIsProgram ctx a

isRenderbuffer :: GLES => RenderBuffer -> GL GLBool
isRenderbuffer a = GL get >>= \ctx -> liftIO $ glIsRenderbuffer ctx a

isShader :: GLES => Shader -> GL GLBool
isShader a = GL get >>= \ctx -> liftIO $ glIsShader ctx a

isTexture :: GLES => Texture -> GL GLBool
isTexture a = GL get >>= \ctx -> liftIO $ glIsTexture ctx a

isVertexArray :: GLES => VertexArrayObject -> GL GLBool
isVertexArray a = GL get >>= \ctx -> liftIO $ glIsVertexArray ctx a

lineWidth :: GLES => Float -> GL ()
lineWidth a = GL get >>= \ctx -> liftIO $ glLineWidth ctx a

linkProgram :: GLES => Program -> GL ()
linkProgram a = GL get >>= \ctx -> liftIO $ glLinkProgram ctx a

pixelStorei :: GLES => GLEnum -> GLInt -> GL ()
pixelStorei a b = GL get >>= \ctx -> liftIO $ glPixelStorei ctx a b

polygonOffset :: GLES => Float -> Float -> GL ()
polygonOffset a b = GL get >>= \ctx -> liftIO $ glPolygonOffset ctx a b

readPixels :: GLES => GLInt -> GLInt -> GLSize -> GLSize -> GLEnum -> GLEnum -> Array -> GL ()
readPixels a b c d e f g = GL get >>= \ctx -> liftIO $ glReadPixels ctx a b c d e f g

renderbufferStorage :: GLES => GLEnum -> GLEnum -> GLSize -> GLSize -> GL ()
renderbufferStorage a b c d = GL get >>= \ctx -> liftIO $ glRenderbufferStorage ctx a b c d

sampleCoverage :: GLES => Float -> GLBool -> GL ()
sampleCoverage a b = GL get >>= \ctx -> liftIO $ glSampleCoverage ctx a b

scissor :: GLES => GLInt -> GLInt -> GLSize -> GLSize -> GL ()
scissor a b c d = GL get >>= \ctx -> liftIO $ glScissor ctx a b c d

shaderSource :: GLES => Shader -> GLString -> GL ()
shaderSource a b = GL get >>= \ctx -> liftIO $ glShaderSource ctx a b

stencilFunc :: GLES => GLEnum -> GLInt -> GLUInt -> GL ()
stencilFunc a b c = GL get >>= \ctx -> liftIO $ glStencilFunc ctx a b c

stencilFuncSeparate :: GLES => GLEnum -> GLEnum -> GLInt -> GLUInt -> GL ()
stencilFuncSeparate a b c d = GL get >>= \ctx -> liftIO $ glStencilFuncSeparate ctx a b c d

stencilMask :: GLES => GLUInt -> GL ()
stencilMask a = GL get >>= \ctx -> liftIO $ glStencilMask ctx a

stencilMaskSeparate :: GLES => GLEnum -> GLUInt -> GL ()
stencilMaskSeparate a b = GL get >>= \ctx -> liftIO $ glStencilMaskSeparate ctx a b

stencilOp :: GLES => GLEnum -> GLEnum -> GLEnum -> GL ()
stencilOp a b c = GL get >>= \ctx -> liftIO $ glStencilOp ctx a b c

stencilOpSeparate :: GLES => GLEnum -> GLEnum -> GLEnum -> GLEnum -> GL ()
stencilOpSeparate a b c d = GL get >>= \ctx -> liftIO $ glStencilOpSeparate ctx a b c d

texImage2D :: GLES => GLEnum -> GLInt -> GLInt -> GLSize -> GLSize -> GLInt -> GLEnum -> GLEnum -> Array -> GL ()
texImage2D a b c d e f g h i = GL get >>= \ctx -> liftIO $ glTexImage2D ctx a b c d e f g h i

texParameterf :: GLES => GLEnum -> GLEnum -> Float -> GL ()
texParameterf a b c = GL get >>= \ctx -> liftIO $ glTexParameterf ctx a b c

texParameteri :: GLES => GLEnum -> GLEnum -> GLInt -> GL ()
texParameteri a b c = GL get >>= \ctx -> liftIO $ glTexParameteri ctx a b c

texSubImage2D :: GLES => GLEnum -> GLInt -> GLInt -> GLInt -> GLSize -> GLSize -> GLEnum -> GLEnum -> Array -> GL ()
texSubImage2D a b c d e f g h i = GL get >>= \ctx -> liftIO $ glTexSubImage2D ctx a b c d e f g h i

uniform1f :: GLES => UniformLocation -> Float -> GL ()
uniform1f a b = GL get >>= \ctx -> liftIO $ glUniform1f ctx a b

uniform1fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform1fv a b = GL get >>= \ctx -> liftIO $ glUniform1fv ctx a b

uniform1i :: GLES => UniformLocation -> Int32 -> GL ()
uniform1i a b = GL get >>= \ctx -> liftIO $ glUniform1i ctx a b

uniform1iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform1iv a b = GL get >>= \ctx -> liftIO $ glUniform1iv ctx a b

uniform2f :: GLES => UniformLocation -> Float -> Float -> GL ()
uniform2f a b c = GL get >>= \ctx -> liftIO $ glUniform2f ctx a b c

uniform2fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform2fv a b = GL get >>= \ctx -> liftIO $ glUniform2fv ctx a b

uniform2i :: GLES => UniformLocation -> Int32 -> Int32 -> GL ()
uniform2i a b c = GL get >>= \ctx -> liftIO $ glUniform2i ctx a b c

uniform2iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform2iv a b = GL get >>= \ctx -> liftIO $ glUniform2iv ctx a b

uniform3f :: GLES => UniformLocation -> Float -> Float -> Float -> GL ()
uniform3f a b c d = GL get >>= \ctx -> liftIO $ glUniform3f ctx a b c d

uniform3fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform3fv a b = GL get >>= \ctx -> liftIO $ glUniform3fv ctx a b

uniform3i :: GLES => UniformLocation -> Int32 -> Int32 -> Int32 -> GL ()
uniform3i a b c d = GL get >>= \ctx -> liftIO $ glUniform3i ctx a b c d

uniform3iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform3iv a b = GL get >>= \ctx -> liftIO $ glUniform3iv ctx a b

uniform4f :: GLES => UniformLocation -> Float -> Float -> Float -> Float -> GL ()
uniform4f a b c d e = GL get >>= \ctx -> liftIO $ glUniform4f ctx a b c d e

uniform4fv :: GLES => UniformLocation -> Float32Array -> GL ()
uniform4fv a b = GL get >>= \ctx -> liftIO $ glUniform4fv ctx a b

uniform4i :: GLES => UniformLocation -> Int32 -> Int32 -> Int32 -> Int32 -> GL ()
uniform4i a b c d e = GL get >>= \ctx -> liftIO $ glUniform4i ctx a b c d e

uniform4iv :: GLES => UniformLocation -> Int32Array -> GL ()
uniform4iv a b = GL get >>= \ctx -> liftIO $ glUniform4iv ctx a b

uniformMatrix2fv :: GLES => UniformLocation -> GLBool -> Float32Array -> GL ()
uniformMatrix2fv a b c = GL get >>= \ctx -> liftIO $ glUniformMatrix2fv ctx a b c

uniformMatrix3fv :: GLES => UniformLocation -> GLBool -> Float32Array -> GL ()
uniformMatrix3fv a b c = GL get >>= \ctx -> liftIO $ glUniformMatrix3fv ctx a b c

uniformMatrix4fv :: GLES => UniformLocation -> GLBool -> Float32Array -> GL ()
uniformMatrix4fv a b c = GL get >>= \ctx -> liftIO $ glUniformMatrix4fv ctx a b c

useProgram :: GLES => Program -> GL ()
useProgram a = GL get >>= \ctx -> liftIO $ glUseProgram ctx a

validateProgram :: GLES => Program -> GL ()
validateProgram a = GL get >>= \ctx -> liftIO $ glValidateProgram ctx a

vertexAttrib1f :: GLES => GLUInt -> Float -> GL ()
vertexAttrib1f a b = GL get >>= \ctx -> liftIO $ glVertexAttrib1f ctx a b

vertexAttrib1fv :: GLES => GLUInt -> Float32Array -> GL ()
vertexAttrib1fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib1fv ctx a b

vertexAttrib2f :: GLES => GLUInt -> Float -> Float -> GL ()
vertexAttrib2f a b c = GL get >>= \ctx -> liftIO $ glVertexAttrib2f ctx a b c

vertexAttrib2fv :: GLES => GLUInt -> Float32Array -> GL ()
vertexAttrib2fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib2fv ctx a b

vertexAttrib3f :: GLES => GLUInt -> Float -> Float -> Float -> GL ()
vertexAttrib3f a b c d = GL get >>= \ctx -> liftIO $ glVertexAttrib3f ctx a b c d

vertexAttrib3fv :: GLES => GLUInt -> Float32Array -> GL ()
vertexAttrib3fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib3fv ctx a b

vertexAttrib4f :: GLES => GLUInt -> Float -> Float -> Float -> Float -> GL ()
vertexAttrib4f a b c d e = GL get >>= \ctx -> liftIO $ glVertexAttrib4f ctx a b c d e

vertexAttrib4fv :: GLES => GLUInt -> Float32Array -> GL ()
vertexAttrib4fv a b = GL get >>= \ctx -> liftIO $ glVertexAttrib4fv ctx a b

vertexAttribPointer :: GLES => GLUInt -> GLInt -> GLEnum -> GLBool -> GLSize -> GLPtr -> GL ()
vertexAttribPointer a b c d e f = GL get >>= \ctx -> liftIO $ glVertexAttribPointer ctx a b c d e f

viewport :: GLES => GLInt -> GLInt -> GLSize -> GLSize -> GL ()
viewport a b c d = GL get >>= \ctx -> liftIO $ glViewport ctx a b c d
