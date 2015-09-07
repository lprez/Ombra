{-# LANGUAGE TypeFamilies, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleInstances #-}

-- Graphics.Rendering.Ombra.Shader.Variables? (+ loadUniform, loadAttribute, inputName, etc.)
module Graphics.Rendering.Ombra.Shader.CPU (UniformCPU(..), AttributeCPU(..), toGPUBool) where

import qualified Data.Int as CPU
import Data.Word (Word)
import Data.Typeable
import qualified Graphics.Rendering.Ombra.Shader.Language.Types as GPU
import Graphics.Rendering.Ombra.Internal.GL as CPU
import qualified Data.Vect.Float as CPU
import Prelude as CPU

-- | CPU types convertible to GPU types (as uniforms).
class Typeable g => UniformCPU c g | g -> c where
        setUniform :: UniformLocation -> g -> c -> GL ()

-- | CPU types convertible to GPU types (as attributes).
class Typeable g => AttributeCPU c g | g -> c where
        encodeAttribute :: g -> [c] -> GL Array
        setAttribute :: g -> GLUInt -> GL ()

-- Float

instance GLES => UniformCPU CPU.Float GPU.Float where
        setUniform l _ v = uniform1f l v

instance (Typeable n, GLES) =>
         UniformCPU [CPU.Float] (GPU.Array n GPU.Float) where
        setUniform l _ v = liftIO (encodeFloats v) >>= uniform1fv l

instance GLES => AttributeCPU CPU.Float GPU.Float where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeFloats a
        setAttribute _ i = attr gl_FLOAT i 1

-- Bool

instance GLES => UniformCPU CPU.Int32 GPU.Bool where
        setUniform l _ v = uniform1i l v

instance (Typeable n, GLES) =>
         UniformCPU [CPU.Int32] (GPU.Array n GPU.Bool) where
        setUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => AttributeCPU CPU.Int32 GPU.Bool where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setAttribute _ i = attr gl_INT i 1

-- Int

instance GLES => UniformCPU CPU.Int32 GPU.Int where
        setUniform l _ v = uniform1i l v

instance (Typeable n, GLES) =>
         UniformCPU [CPU.Int32] (GPU.Array n GPU.Int) where
        setUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => AttributeCPU CPU.Int32 GPU.Int where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setAttribute _ i = attr gl_INT i 1

-- TODO: sampler arrays (they're problematic to safely access in the shaders)
-- Samplers

instance GLES => UniformCPU CPU.ActiveTexture GPU.Sampler2D where
        setUniform l _ (CPU.ActiveTexture v) = uniform1i l $ fromIntegral v

instance GLES => UniformCPU CPU.ActiveTexture GPU.SamplerCube where
        setUniform l _ (CPU.ActiveTexture v) = uniform1i l $ fromIntegral v

-- Vec2

instance GLES => UniformCPU CPU.Vec2 GPU.Vec2 where
        setUniform l _ (CPU.Vec2 x y) = uniform2f l x y

instance (Typeable n, GLES) =>
         UniformCPU [CPU.Vec2] (GPU.Array n GPU.Vec2) where
        setUniform l _ v = liftIO (encodeVec2s v) >>= uniform2fv l

instance GLES => AttributeCPU CPU.Vec2 GPU.Vec2 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec2s a
        setAttribute _ i = attr gl_FLOAT i 2

-- Vec3

instance GLES => UniformCPU CPU.Vec3 GPU.Vec3 where
        setUniform l _ (CPU.Vec3 x y z) = uniform3f l x y z

instance (Typeable n, GLES) =>
         UniformCPU [CPU.Vec3] (GPU.Array n GPU.Vec3) where
        setUniform l _ v = liftIO (encodeVec3s v) >>= uniform3fv l

instance GLES => AttributeCPU CPU.Vec3 GPU.Vec3 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec3s a
        setAttribute _ i = attr gl_FLOAT i 3

-- Vec4

instance GLES => UniformCPU CPU.Vec4 GPU.Vec4 where
        setUniform l _ (CPU.Vec4 x y z w) = uniform4f l x y z w

instance (Typeable n, GLES) =>
         UniformCPU [CPU.Vec4] (GPU.Array n GPU.Vec4) where
        setUniform l _ v = liftIO (encodeVec4s v) >>= uniform4fv l

instance GLES => AttributeCPU CPU.Vec4 GPU.Vec4 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec4s a
        setAttribute _ i = attr gl_FLOAT i 4

-- IVec2

instance GLES => UniformCPU CPU.IVec2 GPU.IVec2 where
        setUniform l _ (CPU.IVec2 x y) = uniform2i l x y

instance (Typeable n, GLES) =>
         UniformCPU [CPU.IVec2] (GPU.Array n GPU.IVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => AttributeCPU CPU.IVec2 GPU.IVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- IVec3

instance GLES => UniformCPU CPU.IVec3 GPU.IVec3 where
        setUniform l _ (CPU.IVec3 x y z) = uniform3i l x y z

instance (Typeable n, GLES) =>
         UniformCPU [CPU.IVec3] (GPU.Array n GPU.IVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => AttributeCPU CPU.IVec3 GPU.IVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- IVec4

instance GLES => UniformCPU CPU.IVec4 GPU.IVec4 where
        setUniform l _ (CPU.IVec4 x y z w) = uniform4i l x y z w

instance (Typeable n, GLES) =>
         UniformCPU [CPU.IVec4] (GPU.Array n GPU.IVec4) where
        setUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => AttributeCPU CPU.IVec4 GPU.IVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setAttribute _ i = attr gl_INT i 4

-- BVec2

instance GLES => UniformCPU CPU.IVec2 GPU.BVec2 where
        setUniform l _ (CPU.IVec2 x y) = uniform2i l x y

instance (Typeable n, GLES) =>
         UniformCPU [CPU.IVec2] (GPU.Array n GPU.BVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => AttributeCPU CPU.IVec2 GPU.BVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- BVec3

instance GLES => UniformCPU CPU.IVec3 GPU.BVec3 where
        setUniform l _ (CPU.IVec3 x y z) = uniform3i l x y z

instance (Typeable n, GLES) =>
         UniformCPU [CPU.IVec3] (GPU.Array n GPU.BVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => AttributeCPU CPU.IVec3 GPU.BVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- BVec4

instance GLES => UniformCPU CPU.IVec4 GPU.BVec4 where
        setUniform l _ (CPU.IVec4 x y z w) = uniform4i l x y z w

instance (Typeable n, GLES) =>
         UniformCPU [CPU.IVec4] (GPU.Array n GPU.BVec4) where
        setUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => AttributeCPU CPU.IVec4 GPU.BVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setAttribute _ i = attr gl_INT i 4

-- Matrices

instance GLES => UniformCPU CPU.Mat2 GPU.Mat2 where
        setUniform l _ m = liftIO (encodeMat2 m) >>= uniformMatrix2fv l false

instance GLES => UniformCPU CPU.Mat3 GPU.Mat3 where
        setUniform l _ m = liftIO (encodeMat3 m) >>= uniformMatrix3fv l false

instance GLES => UniformCPU CPU.Mat4 GPU.Mat4 where
        setUniform l _ m = liftIO (encodeMat4 m) >>= uniformMatrix4fv l false

attr :: GLES => GLEnum -> GLUInt -> GLInt -> GL ()
attr t i s = vertexAttribPointer i s t false 0 nullGLPtr

toGPUBool :: CPU.Bool -> CPU.Int32
toGPUBool True = 1
toGPUBool False = 0
