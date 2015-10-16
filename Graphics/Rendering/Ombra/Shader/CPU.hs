{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds, TypeOperators,
             FunctionalDependencies, FlexibleInstances, RankNTypes,
             FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Shader.CPU (
        CPUSetterType(..),
        CPU(..),
        CPUBase(..),
        BaseUniform(..),
        BaseAttribute(..),
        Uniform(..),
        Attribute(..),
        toGPUBool,
        single,
        mirror
) where

import qualified Data.Int as CPU
import Data.Word (Word)
import Data.Typeable
import qualified Graphics.Rendering.Ombra.Shader.Language.Types as GPU
import Graphics.Rendering.Ombra.Internal.GL as CPU
import GHC.Generics hiding (S)
import qualified GHC.Generics as G
import qualified Data.Vect.Float as CPU
import Prelude as CPU

single :: Proxy S
single = Proxy

mirror :: Proxy M
mirror = Proxy

-- | This kind represents the way you are setting a GPU value.
data CPUSetterType k
        = S             -- ^ Single CPU type (only for types with one field)
        | M             -- ^ Mirror type (a data type identical to the GPU
                        -- one but with CPU single types instead of GPU)

type family CPU (s :: CPUSetterType *) g where
        CPU 'S x = CPUSingle x
        CPU 'M x = CPUMirror x

type family CPUBase g
type family CPUMirror g

-- type family CPUAutoSetter (g :: * -> *) :: CPUSetterType
-- type CPUAuto g = CPU (CPUAutoSetter g) g

-- | CPU types convertible to GPU types (as uniforms).
class BaseUniform g where
        setUniform :: UniformLocation -> proxy g -> CPUBase g -> GL ()

-- | CPU types convertible to GPU types (as attributes).
class GPU.ShaderType g => BaseAttribute g where
        encodeAttribute :: proxy g -> [CPUBase g] -> GL Array
        setAttribute :: proxy g -> GLUInt -> GL ()

class Generic g => Uniform (s :: CPUSetterType *) g where
        withUniforms :: Applicative f
                     => proxy s
                     -> g
                     -> CPU s g
                     -> (forall g. BaseUniform g => Int -> Proxy g
                                                 -> CPUBase g -> f ())
                     -> f ()

class Generic g => Attribute (s :: CPUSetterType *) g where
        withAttributes :: Applicative f
                       => proxy s
                       -> g
                       -> [CPU s g]
                       -> (forall g. BaseAttribute g => Int -> Proxy g
                                                     -> [CPUBase g] -> f ())
                       -> f ()

instance (BaseUniform (GCPUValue (Rep g)), Generic g) => Uniform S g where
        withUniforms _ (g :: g) c f =
                f 0 (Proxy :: Proxy (GCPUValue (Rep g))) c

instance (BaseAttribute (GCPUValue (Rep g)), Generic g) => Attribute S g where
        withAttributes _ (g :: g) c f =
                f 0 (Proxy :: Proxy (GCPUValue (Rep g))) c

instance ( GUniformMirror (Rep g) (Rep (CPUMirror g))
                                  (TData (Rep (CPUMirror g)))
                                  (TCons (Rep (CPUMirror g)))
         , Generic g, Generic (CPUMirror g) )
         => Uniform M g where
        withUniforms _ (g :: g) c f =
                fst $ gWithUniformMirror
                        (Proxy :: Proxy ( (TData (Rep (CPUMirror g)))
                                        , (TCons (Rep (CPUMirror g)))) )
                        0 (from g) (from c) f

{-
instance ( GAttributeMirror (Rep g) (Rep (CPUMirror g))
                                    (TData (Rep (CPUMirror g)))
                                    (TCons (Rep (CPUMirror g)))
         , Generic g, Generic (CPUMirror g) )
         => Attribute M g where
        withAttributes _ (g :: g) c f =
                fst $ gWithAttributeMirror
                        (Proxy :: Proxy ( (TData (Rep (CPUMirror g)))
                                        , (TCons (Rep (CPUMirror g)))) )
                        0 (from g) (from c) f
-}

type family TData (g :: * -> *) where
        TData (M1 D d a) = d

type family TCons (g :: * -> *) where
        TCons (M1 D d a) = TCons a
        TCons (M1 C c a) = c

type family GCPUValue (g :: * -> *) where
        GCPUValue (M1 i c a) = GCPUValue a
        GCPUValue (K1 i a) = a

type CPUSingle g = GCPUSingle (Rep g)
type GCPUSingle g = CPUBase (GCPUValue g)


type family GCPUMirror (g :: * -> *) d c :: * -> * where
        GCPUMirror (a :*: b) d c = GCPUMirror a d c :*: GCPUMirror b d c
        GCPUMirror (M1 D gd a) d c = M1 D d (GCPUMirror a d c)
        GCPUMirror (M1 C gc a) d c = M1 C c (GCPUMirror a d c)
        GCPUMirror (M1 G.S s a) d c = M1 G.S s (GCPUMirror a d c)
        GCPUMirror (K1 i a) d c = K1 i (CPUBase a)

class GUniformMirror (g :: * -> *) (m :: * -> *) d c where
        gWithUniformMirror :: Applicative f
                           => proxy (d, c)
                           -> Int
                           -> g a
                           -> m b
                           -> (forall u. BaseUniform u => Int -> Proxy u
                                                       -> CPUBase u -> f ())
                           -> (f (), Int)

instance ( GUniformMirror a (GCPUMirror a d c) d c
         , GUniformMirror b (GCPUMirror b d c) d c
         , m ~ GCPUMirror (a :*: b) d c )
         => GUniformMirror (a :*: b) m d c where
        gWithUniformMirror p i (x :*: y) (mx :*: my) f =
                let (a1, i') = gWithUniformMirror p i x mx f
                    (a2, i'') = gWithUniformMirror p i' y my f
                in (a1 *> a2, i'')

instance ( GUniformMirror a ma d c
         , M1 mi mv ma ~ GCPUMirror (M1 i v a) d c )
        => GUniformMirror (M1 i v a) (M1 mi mv ma) d c where
        gWithUniformMirror p i (M1 x) (M1 mx) f = gWithUniformMirror p i x mx f

instance (BaseUniform a, m ~ GCPUMirror (K1 i a) d c)
        => GUniformMirror (K1 i a) m d c where
        gWithUniformMirror _ i (K1 (x :: t)) (K1 mx) f =
                (f i (Proxy :: Proxy t) mx, i + 1)

{-
class GAttributeMirror (g :: * -> *) (m :: * -> *) d c where
        gWithAttributeMirror :: Applicative f
                             => proxy (d, c)
                             -> Int
                             -> g a
                             -> m b
                             -> (forall u. BaseAttribute u => Int -> Proxy u
                                                           -> CPUBase u
                                                           -> f ())
                             -> (f (), Int)

instance ( GAttributeMirror a (GCPUMirror a d c) d c
         , GAttributeMirror b (GCPUMirror b d c) d c
         , m ~ GCPUMirror (a :*: b) d c )
         => GAttributeMirror (a :*: b) m d c where
        gWithAttributeMirror p i (x :*: y) (mx :*: my) f =
                let (a1, i') = gWithAttributeMirror p i x mx f
                    (a2, i'') = gWithAttributeMirror p i' y my f
                in (a1 *> a2, i'')

instance ( GAttributeMirror a ma d c
         , M1 mi mv ma ~ GCPUMirror (M1 i v a) d c )
        => GAttributeMirror (M1 i v a) (M1 mi mv ma) d c where
        gWithAttributeMirror p i (M1 x) (M1 mx) f =
                gWithAttributeMirror p i x mx f

instance (BaseAttribute a, m ~ GCPUMirror (K1 i a) d c)
        => GAttributeMirror (K1 i a) m d c where
        gWithAttributeMirror _ i (K1 (x :: t)) (K1 mx) f =
                (f i (Proxy :: Proxy t) mx, i + 1)
-}

-- Float

type instance CPUBase GPU.Float = CPU.Float
type instance CPUBase (GPU.Array n GPU.Float) = [CPU.Float]

instance GLES => BaseUniform GPU.Float where
        setUniform l _ v = uniform1f l v

instance GLES => BaseUniform (GPU.Array n GPU.Float) where
        setUniform l _ v = liftIO (encodeFloats v) >>= uniform1fv l

instance GLES => BaseAttribute GPU.Float where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeFloats a
        setAttribute _ i = attr gl_FLOAT i 1

-- Bool

type instance CPUBase GPU.Bool = CPU.Int32
type instance CPUBase (GPU.Array n GPU.Bool) = [CPU.Int32]

instance GLES => BaseUniform GPU.Bool where
        setUniform l _ v = uniform1i l v

instance GLES => BaseUniform (GPU.Array n GPU.Bool) where
        setUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => BaseAttribute GPU.Bool where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setAttribute _ i = attr gl_INT i 1

-- Int

type instance CPUBase GPU.Int = CPU.Int32
type instance CPUBase (GPU.Array n GPU.Int) = [CPU.Int32]

instance GLES => BaseUniform GPU.Int where
        setUniform l _ v = uniform1i l v

instance GLES => BaseUniform (GPU.Array n GPU.Int) where
        setUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => BaseAttribute GPU.Int where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setAttribute _ i = attr gl_INT i 1

-- TODO: sampler arrays (they're problematic to safely access in the shaders)
-- Samplers

type instance CPUBase GPU.Sampler2D = CPU.ActiveTexture
type instance CPUBase GPU.SamplerCube = CPU.ActiveTexture

instance GLES => BaseUniform GPU.Sampler2D where
        setUniform l _ (CPU.ActiveTexture v) = uniform1i l $ fromIntegral v

instance GLES => BaseUniform GPU.SamplerCube where
        setUniform l _ (CPU.ActiveTexture v) = uniform1i l $ fromIntegral v

-- Vec2

type instance CPUBase GPU.Vec2 = CPU.Vec2
type instance CPUBase (GPU.Array n GPU.Vec2) = [CPU.Vec2]

instance GLES => BaseUniform GPU.Vec2 where
        setUniform l _ (CPU.Vec2 x y) = uniform2f l x y

instance GLES => BaseUniform (GPU.Array n GPU.Vec2) where
        setUniform l _ v = liftIO (encodeVec2s v) >>= uniform2fv l

instance GLES => BaseAttribute GPU.Vec2 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec2s a
        setAttribute _ i = attr gl_FLOAT i 2

-- Vec3

type instance CPUBase GPU.Vec3 = CPU.Vec3
type instance CPUBase (GPU.Array n GPU.Vec3) = [CPU.Vec3]

instance GLES => BaseUniform GPU.Vec3 where
        setUniform l _ (CPU.Vec3 x y z) = uniform3f l x y z

instance GLES => BaseUniform (GPU.Array n GPU.Vec3) where
        setUniform l _ v = liftIO (encodeVec3s v) >>= uniform3fv l

instance GLES => BaseAttribute GPU.Vec3 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec3s a
        setAttribute _ i = attr gl_FLOAT i 3

-- Vec4

type instance CPUBase GPU.Vec4 = CPU.Vec4
type instance CPUBase (GPU.Array n GPU.Vec4) = [CPU.Vec4]

instance GLES => BaseUniform GPU.Vec4 where
        setUniform l _ (CPU.Vec4 x y z w) = uniform4f l x y z w

instance GLES => BaseUniform (GPU.Array n GPU.Vec4) where
        setUniform l _ v = liftIO (encodeVec4s v) >>= uniform4fv l

instance GLES => BaseAttribute GPU.Vec4 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec4s a
        setAttribute _ i = attr gl_FLOAT i 4

-- IVec2

type instance CPUBase GPU.IVec2 = CPU.IVec2
type instance CPUBase (GPU.Array n GPU.IVec2) = [CPU.IVec2]

instance GLES => BaseUniform GPU.IVec2 where
        setUniform l _ (CPU.IVec2 x y) = uniform2i l x y

instance GLES => BaseUniform (GPU.Array n GPU.IVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => BaseAttribute GPU.IVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- IVec3

type instance CPUBase GPU.IVec3 = CPU.IVec3
type instance CPUBase (GPU.Array n GPU.IVec3) = [CPU.IVec3]

instance GLES => BaseUniform GPU.IVec3 where
        setUniform l _ (CPU.IVec3 x y z) = uniform3i l x y z

instance GLES => BaseUniform (GPU.Array n GPU.IVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => BaseAttribute GPU.IVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- IVec4

type instance CPUBase GPU.IVec4 = CPU.IVec4
type instance CPUBase (GPU.Array n GPU.IVec4) = [CPU.IVec4]

instance GLES => BaseUniform GPU.IVec4 where
        setUniform l _ (CPU.IVec4 x y z w) = uniform4i l x y z w

instance GLES => BaseUniform (GPU.Array n GPU.IVec4) where
        setUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => BaseAttribute GPU.IVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setAttribute _ i = attr gl_INT i 4

-- BVec2

type instance CPUBase GPU.BVec2 = CPU.IVec2
type instance CPUBase (GPU.Array n GPU.BVec2) = [CPU.IVec2]

instance GLES => BaseUniform GPU.BVec2 where
        setUniform l _ (CPU.IVec2 x y) = uniform2i l x y

instance GLES => BaseUniform (GPU.Array n GPU.BVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => BaseAttribute GPU.BVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- BVec3

type instance CPUBase GPU.BVec3 = CPU.IVec3
type instance CPUBase (GPU.Array n GPU.BVec3) = [CPU.IVec3]

instance GLES => BaseUniform GPU.BVec3 where
        setUniform l _ (CPU.IVec3 x y z) = uniform3i l x y z

instance GLES => BaseUniform (GPU.Array n GPU.BVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => BaseAttribute GPU.BVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- BVec4

type instance CPUBase GPU.BVec4 = CPU.IVec4
type instance CPUBase (GPU.Array n GPU.BVec4) = [CPU.IVec4]

instance GLES => BaseUniform GPU.BVec4 where
        setUniform l _ (CPU.IVec4 x y z w) = uniform4i l x y z w

instance GLES => BaseUniform (GPU.Array n GPU.BVec4) where
        setUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => BaseAttribute GPU.BVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setAttribute _ i = attr gl_INT i 4

-- Matrices

type instance CPUBase GPU.Mat2 = CPU.Mat2
type instance CPUBase GPU.Mat3 = CPU.Mat3
type instance CPUBase GPU.Mat4 = CPU.Mat4

instance GLES => BaseUniform GPU.Mat2 where
        setUniform l _ m = liftIO (encodeMat2 m) >>= uniformMatrix2fv l false

instance GLES => BaseUniform GPU.Mat3 where
        setUniform l _ m = liftIO (encodeMat3 m) >>= uniformMatrix3fv l false

instance GLES => BaseUniform GPU.Mat4 where
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

toGPUBool :: CPU.Bool -> CPU.Int32
toGPUBool True = 1
toGPUBool False = 0
