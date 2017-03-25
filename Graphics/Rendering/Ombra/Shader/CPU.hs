{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds, TypeOperators,
             FlexibleInstances, RankNTypes, PolyKinds, FlexibleContexts,
             UndecidableInstances, ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Shader.CPU (
        CPUSetterType(..),
        CPU,
        CPUBase,
        CPUMirror,
        BaseUniform(..),
        BaseAttribute(..),
        Uniform(..),
        Attribute(..),
        toGPUBool
) where

import Data.Int
import Data.Typeable
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Vector
import GHC.Generics hiding (S)
import qualified GHC.Generics as G
import Prelude as CPU

-- | This kind represents the way you are setting a GPU value.
data CPUSetterType k
        = S             -- ^ Single CPU type (only for types with one field)
        | M             -- ^ Mirror type (a data type identical to the GPU
                        -- one but with CPU single types instead of GPU)

type family CPU (s :: CPUSetterType *) g where
        CPU 'S x = CPUSingle x
        CPU 'M x = CPUMirror x

type family CPUBase g

-- | The mirror type of a certain global.
-- 
-- For instance:
--
-- @
--      data T = T GVec3 Float -- In the shader module
--      data T = T Vec3 Float -- CPU version of the uniform type
--      type CPUMirror GPU.T = T
-- @
type family CPUMirror g


-- type family CPUAutoSetter (g :: * -> *) :: CPUSetterType
-- type CPUAuto g = CPU (CPUAutoSetter g) g

-- | CPU types convertible to GPU types (as uniforms).
class BaseUniform g where
        setUniform :: UniformLocation -> proxy g -> CPUBase g -> GL ()

-- | CPU types convertible to GPU types (as attributes).
class ShaderType g => BaseAttribute g where
        encodeAttribute :: proxy g -> [CPUBase g] -> GL AnyArray
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

instance (BaseUniform (GGPUValue (Rep g)), Generic g) => Uniform S g where
        withUniforms _ (_ :: g) c f =
                f 0 (Proxy :: Proxy (GGPUValue (Rep g))) c

instance (BaseAttribute (GGPUValue (Rep g)), Generic g) => Attribute S g where
        withAttributes _ (_ :: g) c f =
                f 0 (Proxy :: Proxy (GGPUValue (Rep g))) c

instance ( GUniformMirror (Rep g) (Rep (CPUMirror g))
                                  (TData (Rep (CPUMirror g)))
                                  (TCons (Rep (CPUMirror g)))
         , Generic g, Generic (CPUMirror g) )
         => Uniform M g where
        withUniforms _ (g :: g) c f =
                fst $ gWithUniformMirror
                        (Proxy :: Proxy (MTuple (TData (Rep (CPUMirror g)))
                                                (TCons (Rep (CPUMirror g)))) )
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

type family TData (g :: * -> *) :: Meta where
        TData (M1 D d a) = d

type family TCons (g :: * -> *) :: Meta where
        TCons (M1 D d a) = TCons a
        TCons (M1 C c a) = c

type family GGPUValue (g :: * -> *) where
        GGPUValue (M1 i c a) = GGPUValue a
        GGPUValue (K1 i a) = a

type CPUSingle g = GCPUSingle (Rep g)
type GCPUSingle g = CPUBase (GGPUValue g)


type family GCPUMirror (g :: * -> *) d c :: * -> * where
        GCPUMirror (a :*: b) d c = GCPUMirror a d c :*: GCPUMirror b d c
        GCPUMirror (M1 D gd a) d c = M1 D d (GCPUMirror a d c)
        GCPUMirror (M1 C gc a) d c = M1 C c (GCPUMirror a d c)
        GCPUMirror (M1 G.S s a) d c = M1 G.S s (GCPUMirror a d c)
        GCPUMirror (K1 i a) d c = K1 i (CPUBase a)

data MTuple (d :: k) (c :: k)

class GUniformMirror (g :: * -> *) (m :: * -> *) (d :: Meta) (c :: Meta) where
        gWithUniformMirror :: Applicative f
                           => proxy (MTuple d c)
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
        gWithUniformMirror p i (M1 x) (M1 mx) = gWithUniformMirror p i x mx

instance (BaseUniform a, m ~ GCPUMirror (K1 i a) d c)
        => GUniformMirror (K1 i a) m d c where
        gWithUniformMirror _ i (K1 (_ :: t)) (K1 mx) f =
                (f i (Proxy :: Proxy t) mx, i + 1)

{-
class GAttributeMirror (g :: * -> *) (m :: * -> *) d c where
        gWithAttributeMirror :: Applicative f
                             => proxy (MTuple d c)
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

type instance CPUBase GFloat = Float
type instance CPUBase (GArray n GFloat) = [Float]

instance GLES => BaseUniform GFloat where
        setUniform l _ = uniform1f l

instance GLES => BaseUniform (GArray n GFloat) where
        setUniform l _ v = liftIO (encodeFloats v) >>= uniform1fv l

instance GLES => BaseAttribute GFloat where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeFloats a
        setAttribute _ i = attr gl_FLOAT i 1

-- Bool

type instance CPUBase GBool = Int32
type instance CPUBase (GArray n GBool) = [Int32]

instance GLES => BaseUniform GBool where
        setUniform l _ = uniform1i l

instance GLES => BaseUniform (GArray n GBool) where
        setUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => BaseAttribute GBool where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setAttribute _ i = attr gl_INT i 1

-- Int

type instance CPUBase GInt = Int32
type instance CPUBase (GArray n GInt) = [Int32]

instance GLES => BaseUniform GInt where
        setUniform l _ = uniform1i l

instance GLES => BaseUniform (GArray n GInt) where
        setUniform l _ v = liftIO (encodeInts v) >>= uniform1iv l

instance GLES => BaseAttribute GInt where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeInts a
        setAttribute _ i = attr gl_INT i 1

-- TODO: sampler arrays (they're problematic to safely access in the shaders)
-- Samplers

type instance CPUBase GSampler2D = ActiveTexture
type instance CPUBase GSamplerCube = ActiveTexture

instance GLES => BaseUniform GSampler2D where
        setUniform l _ (ActiveTexture v) = uniform1i l $ fromIntegral v

instance GLES => BaseUniform GSamplerCube where
        setUniform l _ (ActiveTexture v) = uniform1i l $ fromIntegral v

-- Vec2

type instance CPUBase GVec2 = Vec2
type instance CPUBase (GArray n GVec2) = [Vec2]

instance GLES => BaseUniform GVec2 where
        setUniform l _ (Vec2 x y) = uniform2f l x y

instance GLES => BaseUniform (GArray n GVec2) where
        setUniform l _ v = liftIO (encodeVec2s v) >>= uniform2fv l

instance GLES => BaseAttribute GVec2 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec2s a
        setAttribute _ i = attr gl_FLOAT i 2

-- Vec3

type instance CPUBase GVec3 = Vec3
type instance CPUBase (GArray n GVec3) = [Vec3]

instance GLES => BaseUniform GVec3 where
        setUniform l _ (Vec3 x y z) = uniform3f l x y z

instance GLES => BaseUniform (GArray n GVec3) where
        setUniform l _ v = liftIO (encodeVec3s v) >>= uniform3fv l

instance GLES => BaseAttribute GVec3 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec3s a
        setAttribute _ i = attr gl_FLOAT i 3

-- Vec4

type instance CPUBase GVec4 = Vec4
type instance CPUBase (GArray n GVec4) = [Vec4]

instance GLES => BaseUniform GVec4 where
        setUniform l _ (Vec4 x y z w) = uniform4f l x y z w

instance GLES => BaseUniform (GArray n GVec4) where
        setUniform l _ v = liftIO (encodeVec4s v) >>= uniform4fv l

instance GLES => BaseAttribute GVec4 where
        encodeAttribute _ a = liftIO . fmap fromFloat32Array $ encodeVec4s a
        setAttribute _ i = attr gl_FLOAT i 4

-- IVec2

type instance CPUBase GIVec2 = IVec2
type instance CPUBase (GArray n GIVec2) = [IVec2]

instance GLES => BaseUniform GIVec2 where
        setUniform l _ (IVec2 x y) = uniform2i l x y

instance GLES => BaseUniform (GArray n GIVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => BaseAttribute GIVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- IVec3

type instance CPUBase GIVec3 = IVec3
type instance CPUBase (GArray n GIVec3) = [IVec3]

instance GLES => BaseUniform GIVec3 where
        setUniform l _ (IVec3 x y z) = uniform3i l x y z

instance GLES => BaseUniform (GArray n GIVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => BaseAttribute GIVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- IVec4

type instance CPUBase GIVec4 = IVec4
type instance CPUBase (GArray n GIVec4) = [IVec4]

instance GLES => BaseUniform GIVec4 where
        setUniform l _ (IVec4 x y z w) = uniform4i l x y z w

instance GLES => BaseUniform (GArray n GIVec4) where
        setUniform l _ v = liftIO (encodeIVec4s v) >>= uniform4iv l

instance GLES => BaseAttribute GIVec4 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec4s a
        setAttribute _ i = attr gl_INT i 4

-- BVec2

type instance CPUBase GBVec2 = IVec2
type instance CPUBase (GArray n GBVec2) = [IVec2]

instance GLES => BaseUniform GBVec2 where
        setUniform l _ (IVec2 x y) = uniform2i l x y

instance GLES => BaseUniform (GArray n GBVec2) where
        setUniform l _ v = liftIO (encodeIVec2s v) >>= uniform2iv l

instance GLES => BaseAttribute GBVec2 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec2s a
        setAttribute _ i = attr gl_INT i 2

-- BVec3

type instance CPUBase GBVec3 = IVec3
type instance CPUBase (GArray n GBVec3) = [IVec3]

instance GLES => BaseUniform GBVec3 where
        setUniform l _ (IVec3 x y z) = uniform3i l x y z

instance GLES => BaseUniform (GArray n GBVec3) where
        setUniform l _ v = liftIO (encodeIVec3s v) >>= uniform3iv l

instance GLES => BaseAttribute GBVec3 where
        encodeAttribute _ a = liftIO . fmap fromInt32Array $ encodeIVec3s a
        setAttribute _ i = attr gl_INT i 3

-- BVec4

type instance CPUBase GBVec4 = IVec4
type instance CPUBase (GArray n GBVec4) = [IVec4]

instance GLES => BaseUniform GBVec4 where
        setUniform l _ (IVec4 x y z w) = uniform4i l x y z w

instance GLES => BaseUniform (GArray n GBVec4) where
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
