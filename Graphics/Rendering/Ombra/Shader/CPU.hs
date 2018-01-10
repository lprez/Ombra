{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds, TypeOperators,
             FlexibleInstances, RankNTypes, PolyKinds, FlexibleContexts,
             UndecidableInstances, ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Shader.CPU (
        CPUBase,
        BaseUniform(..),
        BaseAttribute(..),
        ArrayUniform(..),
        toGPUBool
) where

import Data.Hashable (Hashable)
import Data.Int
import Data.Proxy
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
class (ShaderType g, Hashable (CPUBase g)) => BaseUniform g where
        setBaseUniform :: UniformLocation -> proxy g -> CPUBase g -> GL ()

-- | CPU types convertible to GPU types (as attributes).
class ShaderType g => BaseAttribute g where
        encodeAttribute :: proxy g -> [CPUBase g] -> GL AnyArray
        setBaseAttribute :: proxy g -> GLUInt -> GL ()

class ShaderType t => ArrayUniform t where
        baseUniformGArray :: KnownNat n
                          => Proxy n
                          -> Proxy t
                          -> (BaseUniform (GArray n t) => a)
                          -> a

type instance CPUBase (GArray n t) = [CPUBase t]

-- Float

type instance CPUBase GFloat = Float

instance GLES => BaseUniform GFloat where
        setBaseUniform l _ = uniform1f l

instance (GLES, KnownNat n) => BaseUniform (GArray n GFloat) where
        setBaseUniform l _ v = liftIO (encodeFloats v) >>= uniform1fv l

instance GLES => ArrayUniform GFloat where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GFloat where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeFloats a
        setBaseAttribute _ i = attr gl_FLOAT i 1

-- Bool

type instance CPUBase GBool = Bool

toBool :: Bool -> Int32
toBool True = 1
toBool False = 0

instance GLES => BaseUniform GBool where
        setBaseUniform l _ = uniform1i l . toBool

instance (GLES, KnownNat n) => BaseUniform (GArray n GBool) where
        setBaseUniform l _ v = liftIO (encodeInts $ map toBool v) >>=
                               uniform1iv l

instance GLES => ArrayUniform GBool where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GBool where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $
                                encodeInts (map toBool a)
        setBaseAttribute _ i = attr gl_INT i 1

-- Int

type instance CPUBase GInt = Int32

instance GLES => BaseUniform GInt where
        setBaseUniform l _ = uniform1i l

instance (GLES, KnownNat n) => BaseUniform (GArray n GInt) where
        setBaseUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => ArrayUniform GInt where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GInt where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setBaseAttribute _ i = attr gl_INT i 1

-- TODO: sampler arrays (they're problematic to safely access in the shaders)
-- Samplers

type instance CPUBase GSampler2D = Sampler2D
-- type instance CPUBase GSamplerCube = ActiveTexture

instance GLES => BaseUniform GSampler2D where
        setBaseUniform l _ (Sampler2D v) = uniform1i l $ fromIntegral v

{-
instance GLES => ArrayUniform GSampler2D where
        baseUniformGArray _ _ = id
-}

{-
instance GLES => BaseUniform GSamplerCube where
        setBaseUniform l _ (ActiveTexture v) = uniform1i l $ fromIntegral v
-}

-- Vec2

type instance CPUBase GVec2 = Vec2

instance GLES => BaseUniform GVec2 where
        setBaseUniform l _ (Vec2 x y) = uniform2f l x y

instance (GLES, KnownNat n) => BaseUniform (GArray n GVec2) where
        setBaseUniform l _ v = liftIO (encodeVec2s v) >>= uniform2fv l

instance GLES => ArrayUniform GVec2 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GVec2 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec2s a
        setBaseAttribute _ i = attr gl_FLOAT i 2

-- Vec3

type instance CPUBase GVec3 = Vec3

instance GLES => BaseUniform GVec3 where
        setBaseUniform l _ (Vec3 x y z) = uniform3f l x y z

instance (GLES, KnownNat n) => BaseUniform (GArray n GVec3) where
        setBaseUniform l _ v = liftIO (encodeVec3s v) >>= uniform3fv l

instance GLES => ArrayUniform GVec3 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GVec3 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec3s a
        setBaseAttribute _ i = attr gl_FLOAT i 3

-- Vec4

type instance CPUBase GVec4 = Vec4

instance GLES => BaseUniform GVec4 where
        setBaseUniform l _ (Vec4 x y z w) = uniform4f l x y z w

instance (GLES, KnownNat n) => BaseUniform (GArray n GVec4) where
        setBaseUniform l _ v = liftIO (encodeVec4s v) >>= uniform4fv l

instance GLES => ArrayUniform GVec4 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GVec4 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec4s a
        setBaseAttribute _ i = attr gl_FLOAT i 4

-- IVec2

type instance CPUBase GIVec2 = IVec2

instance GLES => BaseUniform GIVec2 where
        setBaseUniform l _ (IVec2 x y) = uniform2i l x y

instance (GLES, KnownNat n) => BaseUniform (GArray n GIVec2) where
        setBaseUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => ArrayUniform GIVec2 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GIVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setBaseAttribute _ i = attr gl_INT i 2

-- IVec3

type instance CPUBase GIVec3 = IVec3

instance GLES => BaseUniform GIVec3 where
        setBaseUniform l _ (IVec3 x y z) = uniform3i l x y z

instance (GLES, KnownNat n) => BaseUniform (GArray n GIVec3) where
        setBaseUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => ArrayUniform GIVec3 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GIVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setBaseAttribute _ i = attr gl_INT i 3

-- IVec4

type instance CPUBase GIVec4 = IVec4

instance GLES => BaseUniform GIVec4 where
        setBaseUniform l _ (IVec4 x y z w) = uniform4i l x y z w

instance (GLES, KnownNat n) => BaseUniform (GArray n GIVec4) where
        setBaseUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => ArrayUniform GIVec4 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GIVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setBaseAttribute _ i = attr gl_INT i 4

-- BVec2

type instance CPUBase GBVec2 = IVec2

instance GLES => BaseUniform GBVec2 where
        setBaseUniform l _ (IVec2 x y) = uniform2i l x y

instance (GLES, KnownNat n) => BaseUniform (GArray n GBVec2) where
        setBaseUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => ArrayUniform GBVec2 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GBVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setBaseAttribute _ i = attr gl_INT i 2

-- BVec3

type instance CPUBase GBVec3 = IVec3

instance GLES => BaseUniform GBVec3 where
        setBaseUniform l _ (IVec3 x y z) = uniform3i l x y z

instance (GLES, KnownNat n) => BaseUniform (GArray n GBVec3) where
        setBaseUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => ArrayUniform GBVec3 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GBVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setBaseAttribute _ i = attr gl_INT i 3

-- BVec4

type instance CPUBase GBVec4 = IVec4

instance GLES => BaseUniform GBVec4 where
        setBaseUniform l _ (IVec4 x y z w) = uniform4i l x y z w

instance (GLES, KnownNat n) => BaseUniform (GArray n GBVec4) where
        setBaseUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => ArrayUniform GBVec4 where
        baseUniformGArray _ _ = id

instance GLES => BaseAttribute GBVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setBaseAttribute _ i = attr gl_INT i 4

-- Matrices

type instance CPUBase GMat2 = Mat2
type instance CPUBase GMat3 = Mat3
type instance CPUBase GMat4 = Mat4

instance GLES => BaseUniform GMat2 where
        setBaseUniform l _ m = liftIO (encodeMat2 m) >>= uniformMatrix2fv l false

instance GLES => BaseUniform GMat3 where
        setBaseUniform l _ m = liftIO (encodeMat3 m) >>= uniformMatrix3fv l false

instance GLES => BaseUniform GMat4 where
        setBaseUniform l _ m = liftIO (encodeMat4 m) >>= uniformMatrix4fv l false

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
