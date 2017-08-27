{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds, TypeOperators,
             FlexibleInstances, RankNTypes, PolyKinds, FlexibleContexts,
             UndecidableInstances, ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Shader.CPU (
        CPUBase,
        BaseUniform(..),
        BaseAttribute(..),
        toGPUBool
) where

import Data.Int
import Data.Typeable
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Vector
import GHC.Generics hiding (S)
import qualified GHC.Generics as G
import GHC.TypeLits
import Prelude as CPU

type family CPUBase g


-- type family CPUAutoSetter (g :: * -> *) :: CPUSetterType
-- type CPUAuto g = CPU (CPUAutoSetter g) g

-- | CPU types convertible to GPU types (as uniforms).
class ShaderType g => BaseUniform g where
        setUniform :: UniformLocation -> proxy g -> CPUBase g -> GL ()

-- | CPU types convertible to GPU types (as attributes).
class ShaderType g => BaseAttribute g where
        encodeAttribute :: proxy g -> [CPUBase g] -> GL AnyArray
        setAttribute :: proxy g -> GLUInt -> GL ()

-- Float

type instance CPUBase GFloat = Float
type instance CPUBase (GArray n GFloat) = [Float]

instance GLES => BaseUniform GFloat where
        setUniform l _ = uniform1f l

instance (GLES, KnownNat n) => BaseUniform (GArray n GFloat) where
        setUniform l _ v = liftIO (encodeFloats v) >>= uniform1fv l

instance GLES => BaseAttribute GFloat where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeFloats a
        setAttribute _ i = attr gl_FLOAT i 1

-- Bool

type instance CPUBase GBool = Bool
type instance CPUBase (GArray n GBool) = [Bool]

toBool :: Bool -> Int32
toBool True = 1
toBool False = 0

instance GLES => BaseUniform GBool where
        setUniform l _ = uniform1i l . toBool

instance (GLES, KnownNat n) => BaseUniform (GArray n GBool) where
        setUniform l _ v = liftIO (encodeInts $ map toBool v) >>= uniform1iv l

instance GLES => BaseAttribute GBool where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $
                                encodeInts (map toBool a)
        setAttribute _ i = attr gl_INT i 1

-- Int

type instance CPUBase GInt = Int32
type instance CPUBase (GArray n GInt) = [Int32]

instance GLES => BaseUniform GInt where
        setUniform l _ = uniform1i l

instance (GLES, KnownNat n) => BaseUniform (GArray n GInt) where
        setUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => BaseAttribute GInt where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setAttribute _ i = attr gl_INT i 1

-- TODO: sampler arrays (they're problematic to safely access in the shaders)
-- Samplers

type instance CPUBase GSampler2D = Sampler2D
-- type instance CPUBase GSamplerCube = ActiveTexture

instance GLES => BaseUniform GSampler2D where
        setUniform l _ (Sampler2D v) = uniform1i l $ fromIntegral v

{-
instance GLES => BaseUniform GSamplerCube where
        setUniform l _ (ActiveTexture v) = uniform1i l $ fromIntegral v
-}

-- Vec2

type instance CPUBase GVec2 = Vec2
type instance CPUBase (GArray n GVec2) = [Vec2]

instance GLES => BaseUniform GVec2 where
        setUniform l _ (Vec2 x y) = uniform2f l x y

instance (GLES, KnownNat n) => BaseUniform (GArray n GVec2) where
        setUniform l _ v = liftIO (encodeVec2s v) >>= uniform2fv l

instance GLES => BaseAttribute GVec2 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec2s a
        setAttribute _ i = attr gl_FLOAT i 2

-- Vec3

type instance CPUBase GVec3 = Vec3
type instance CPUBase (GArray n GVec3) = [Vec3]

instance GLES => BaseUniform GVec3 where
        setUniform l _ (Vec3 x y z) = uniform3f l x y z

instance (GLES, KnownNat n) => BaseUniform (GArray n GVec3) where
        setUniform l _ v = liftIO (encodeVec3s v) >>= uniform3fv l

instance GLES => BaseAttribute GVec3 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec3s a
        setAttribute _ i = attr gl_FLOAT i 3

-- Vec4

type instance CPUBase GVec4 = Vec4
type instance CPUBase (GArray n GVec4) = [Vec4]

instance GLES => BaseUniform GVec4 where
        setUniform l _ (Vec4 x y z w) = uniform4f l x y z w

instance (GLES, KnownNat n) => BaseUniform (GArray n GVec4) where
        setUniform l _ v = liftIO (encodeVec4s v) >>= uniform4fv l

instance GLES => BaseAttribute GVec4 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec4s a
        setAttribute _ i = attr gl_FLOAT i 4

-- IVec2

type instance CPUBase GIVec2 = IVec2
type instance CPUBase (GArray n GIVec2) = [IVec2]

instance GLES => BaseUniform GIVec2 where
        setUniform l _ (IVec2 x y) = uniform2i l x y

instance (GLES, KnownNat n) => BaseUniform (GArray n GIVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => BaseAttribute GIVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- IVec3

type instance CPUBase GIVec3 = IVec3
type instance CPUBase (GArray n GIVec3) = [IVec3]

instance GLES => BaseUniform GIVec3 where
        setUniform l _ (IVec3 x y z) = uniform3i l x y z

instance (GLES, KnownNat n) => BaseUniform (GArray n GIVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => BaseAttribute GIVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- IVec4

type instance CPUBase GIVec4 = IVec4
type instance CPUBase (GArray n GIVec4) = [IVec4]

instance GLES => BaseUniform GIVec4 where
        setUniform l _ (IVec4 x y z w) = uniform4i l x y z w

instance (GLES, KnownNat n) => BaseUniform (GArray n GIVec4) where
        setUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => BaseAttribute GIVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setAttribute _ i = attr gl_INT i 4

-- BVec2

type instance CPUBase GBVec2 = IVec2
type instance CPUBase (GArray n GBVec2) = [IVec2]

instance GLES => BaseUniform GBVec2 where
        setUniform l _ (IVec2 x y) = uniform2i l x y

instance (GLES, KnownNat n) => BaseUniform (GArray n GBVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => BaseAttribute GBVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- BVec3

type instance CPUBase GBVec3 = IVec3
type instance CPUBase (GArray n GBVec3) = [IVec3]

instance GLES => BaseUniform GBVec3 where
        setUniform l _ (IVec3 x y z) = uniform3i l x y z

instance (GLES, KnownNat n) => BaseUniform (GArray n GBVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => BaseAttribute GBVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- BVec4

type instance CPUBase GBVec4 = IVec4
type instance CPUBase (GArray n GBVec4) = [IVec4]

instance GLES => BaseUniform GBVec4 where
        setUniform l _ (IVec4 x y z w) = uniform4i l x y z w

instance (GLES, KnownNat n) => BaseUniform (GArray n GBVec4) where
        setUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => BaseAttribute GBVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setAttribute _ i = attr gl_INT i 4

-- Matrices

type instance CPUBase GMat2 = Mat2
type instance CPUBase GMat3 = Mat3
type instance CPUBase GMat4 = Mat4

instance GLES => BaseUniform GMat2 where
        setUniform l _ m = liftIO (encodeMat2 m) >>= uniformMatrix2fv l false

instance GLES => BaseUniform GMat3 where
        setUniform l _ m = liftIO (encodeMat3 m) >>= uniformMatrix3fv l false

instance GLES => BaseUniform GMat4 where
        setUniform l _ m = liftIO (encodeMat4 m) >>= uniformMatrix4fv l false

class BaseUniforms (xs :: [*])
instance BaseUniform x => BaseUniforms (x ': '[])
instance (BaseUniform x, BaseUniforms (y ': xs)) =>
         BaseUniforms (x ': y ': xs)

class BaseAttributes (xs :: [*])
instance BaseAttribute x => BaseAttributes (x ': '[])
instance (BaseAttribute x, BaseAttributes (y ': xs)) =>
         BaseAttributes (x ': y ': xs)

attr :: GLES => GLEnum -> GLUInt -> GLInt -> GL ()
attr t i s = vertexAttribPointer i s t false 0 nullGLPtr

toGPUBool :: Bool -> Int32
toGPUBool True = 1
toGPUBool False = 0
