{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, DataKinds,
             KindSignatures, ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Shader.Language.Types where

import Data.Typeable
import GHC.TypeLits
import Data.Hashable
import Prelude (String, ($), error, Eq(..), (++), (*), fromInteger, (&&))
import qualified Prelude

-- | CPU integer, used in the shader compiler.
type MInt = Prelude.Int

-- | An expression.
data Expr = Empty | Read String | Op1 String Expr | Op2 String Expr Expr
          | Apply String [Expr] | X Expr | Y Expr | Z Expr | W Expr
          | Literal String | Action Action | Dummy MInt | ArrayIndex Expr Expr
          | ContextVar MInt ContextVarType
          deriving Eq

-- | Expressions that are transformed to statements.
data Action = Store String Expr | If Expr String Expr Expr
            | For MInt String Expr (Expr -> Expr -> (Expr, Expr))

data ContextVarType = LoopIteration | LoopValue deriving Eq

-- | A GPU boolean.
newtype Bool = Bool Expr 

-- | A GPU float.
newtype Float = Float Expr 

-- | A GPU integer.
newtype Int = Int Expr 

-- | A GPU 2D texture handle.
newtype Sampler2D = Sampler2D Expr 

-- | A GPU cube texture handler.
newtype SamplerCube = SamplerCube Expr 

-- | The type of a generic expression.
newtype Unknown = Unknown Expr

-- | A GPU 2D float vector.
-- NB: This is a different type from Data.Vect.Float.'Data.Vect.Float.Vec2'.
data Vec2 = Vec2 Float Float 

-- | A GPU 3D float vector.
data Vec3 = Vec3 Float Float Float 

-- | A GPU 4D float vector.
data Vec4 = Vec4 Float Float Float Float 

-- | A GPU 2D integer vector.
data IVec2 = IVec2 Int Int 

-- | A GPU 3D integer vector.
data IVec3 = IVec3 Int Int Int 

-- | A GPU 4D integer vector.
data IVec4 = IVec4 Int Int Int Int 

-- | A GPU 2D boolean vector.
data BVec2 = BVec2 Bool Bool 

-- | A GPU 3D boolean vector.
data BVec3 = BVec3 Bool Bool Bool 

-- | A GPU 4D boolean vector.
data BVec4 = BVec4 Bool Bool Bool Bool 

-- | A GPU 2x2 float matrix.
data Mat2 = Mat2 Vec2 Vec2 

-- | A GPU 3x3 float matrix.
data Mat3 = Mat3 Vec3 Vec3 Vec3 

-- | A GPU 4x4 float matrix.
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4 

-- | A GPU array.
data Array (n :: Nat) t = Array Expr 

-- | A type in the GPU.
class ShaderType t where
        zero :: t

        toExpr :: t -> Expr

        fromExpr :: Expr -> t

        typeName :: t -> String

        size :: t -> MInt

instance ShaderType Unknown where
        zero = error "zero: Unknown type."
        toExpr (Unknown e) = e
        fromExpr = Unknown
        typeName = error "typeName: Unknown type."
        size = error "size: Unknown type."

instance (ShaderType t, KnownNat n) => ShaderType (Array n t) where
        zero = error "zero: Unsupported constant arrays."
        toExpr (Array e) = e
        fromExpr = Array
        typeName (Array _ :: Array n t) =
                typeName (zero :: t) ++
                "[" ++ Prelude.show (natVal (Proxy :: Proxy n)) ++ "]"
        size (Array _ :: Array n t) =
                size (zero :: t) * fromInteger (natVal (Proxy :: Proxy n))

instance ShaderType Bool where
        zero = Bool $ Literal "false"

        toExpr (Bool e) = e

        fromExpr = Bool

        typeName _ = "bool"

        size _ = 1

instance ShaderType Int where
        zero = Int $ Literal "0"

        toExpr (Int e) = e

        fromExpr = Int

        typeName _ = "int"

        size _ = 1

instance ShaderType Float where
        zero = Float $ Literal "0.0"

        toExpr (Float e) = e

        fromExpr = Float

        typeName _ = "float"

        size _ = 1

instance ShaderType Sampler2D where
        zero = Sampler2D $ Literal "0"

        toExpr (Sampler2D e) = e

        fromExpr = Sampler2D

        typeName _ = "sampler2D"

        size _ = 1

instance ShaderType SamplerCube where
        zero = SamplerCube $ Literal "0"

        toExpr (SamplerCube e) = e

        fromExpr = SamplerCube

        typeName _ = "samplerCube"

        size _ = 1

instance ShaderType Vec2 where
        zero = Vec2 zero zero

        toExpr (Vec2 (Float (X v)) (Float (Y v'))) | v == v' = Apply "vec2" [v]
        toExpr (Vec2 (Float x) (Float y)) = Apply "vec2" [x, y]

        fromExpr v = Vec2 (Float (X v)) (Float (Y v))

        typeName _ = "vec2"

        size _ = 1

instance ShaderType Vec3 where
        zero = Vec3 zero zero zero

        toExpr (Vec3 (Float (X v)) (Float (Y v')) (Float (Z v'')))
               | v == v' && v' == v'' = Apply "vec3" [v]
        toExpr (Vec3 (Float x) (Float y) (Float z)) = Apply "vec3" [x, y, z]

        fromExpr v = Vec3 (Float (X v)) (Float (Y v)) (Float (Z v))

        typeName _ = "vec3"

        size _ = 1

instance ShaderType Vec4 where
        zero = Vec4 zero zero zero zero

        toExpr (Vec4 (Float (X v)) (Float (Y v1)) (Float (Z v2)) (Float (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 = Apply "vec4" [v]
        toExpr (Vec4 (Float x) (Float y) (Float z) (Float w)) =
                Apply "vec4" [x, y, z, w]

        fromExpr v = Vec4 (Float (X v)) (Float (Y v)) (Float (Z v)) (Float (W v))

        typeName _ = "vec4"

        size _ = 1

instance ShaderType IVec2 where
        zero = IVec2 zero zero

        toExpr (IVec2 (Int (X v)) (Int (Y v'))) | v == v' = Apply "ivec2" [v]
        toExpr (IVec2 (Int x) (Int y)) = Apply "ivec2" [x, y]

        fromExpr v = IVec2 (Int (X v)) (Int (Y v))

        typeName _ = "ivec2"

        size _ = 1

instance ShaderType IVec3 where
        zero = IVec3 zero zero zero

        toExpr (IVec3 (Int (X v)) (Int (Y v')) (Int (Z v'')))
               | v == v' && v' == v'' = Apply "ivec3" [v]
        toExpr (IVec3 (Int x) (Int y) (Int z)) = Apply "ivec3" [x, y, z]

        fromExpr v = IVec3 (Int (X v)) (Int (Y v)) (Int (Z v))

        typeName _ = "ivec3"

        size _ = 1

instance ShaderType IVec4 where
        zero = IVec4 zero zero zero zero

        toExpr (IVec4 (Int (X v)) (Int (Y v1)) (Int (Z v2)) (Int (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 = Apply "ivec4" [v]
        toExpr (IVec4 (Int x) (Int y) (Int z) (Int w)) =
                Apply "ivec4" [x, y, z, w]

        fromExpr v = IVec4 (Int (X v)) (Int (Y v)) (Int (Z v)) (Int (W v))

        typeName _ = "ivec4"

        size _ = 1

instance ShaderType BVec2 where
        zero = BVec2 zero zero

        toExpr (BVec2 (Bool (X v)) (Bool (Y v'))) | v == v' = Apply "bvec2" [v]
        toExpr (BVec2 (Bool x) (Bool y)) = Apply "bvec2" [x, y]

        fromExpr v = BVec2 (Bool (X v)) (Bool (Y v))

        typeName _ = "bvec2"

        size _ = 1

instance ShaderType BVec3 where
        zero = BVec3 zero zero zero

        toExpr (BVec3 (Bool (X v)) (Bool (Y v')) (Bool (Z v'')))
               | v == v' && v' == v'' = Apply "bvec3" [v]
        toExpr (BVec3 (Bool x) (Bool y) (Bool z)) = Apply "bvec3" [x, y, z]

        fromExpr v = BVec3 (Bool (X v)) (Bool (Y v)) (Bool (Z v))

        typeName _ = "bvec3"

        size _ = 1

instance ShaderType BVec4 where
        zero = BVec4 zero zero zero zero

        toExpr (BVec4 (Bool (X v)) (Bool (Y v1)) (Bool (Z v2)) (Bool (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 = Apply "bvec4" [v]
        toExpr (BVec4 (Bool x) (Bool y) (Bool z) (Bool w)) =
                Apply "bvec4" [x, y, z, w]

        fromExpr v = BVec4 (Bool (X v)) (Bool (Y v))
                           (Bool (Z v)) (Bool (W v))

        typeName _ = "bvec4"

        size _ = 1

instance ShaderType Mat2 where
        zero = Mat2 zero zero

        toExpr (Mat2 (Vec2 (Float (X (X m))) (Float (X (Y m1))))
                     (Vec2 (Float (Y (X m2))) (Float (Y (Y m3)))))
               | m == m1 && m1 == m2 && m2 == m3 = Apply "mat2" [m]
        toExpr (Mat2 (Vec2 (Float xx) (Float xy))
                     (Vec2 (Float yx) (Float yy)))
               = Apply "mat2" [xx, yx, xy, yy]

        fromExpr m = Mat2 (Vec2 (Float (X (X m))) (Float (Y (X m))))
                          (Vec2 (Float (Y (X m))) (Float (Y (Y m))))

        typeName _ = "mat2"

        size _ = 2

instance ShaderType Mat3 where
        zero = Mat3 zero zero zero

        toExpr (Mat3 (Vec3 (Float (X (X m)))
                           (Float (X (Y m1)))
                           (Float (X (Z m2))))
                     (Vec3 (Float (Y (X m3)))
                           (Float (Y (Y m4)))
                           (Float (Y (Z m5))))
                     (Vec3 (Float (Z (X m6)))
                           (Float (Z (Y m7)))
                           (Float (Z (Z m8)))))
               | m == m1 && m1 == m2 && m2 == m3 && m3 == m4 &&
                 m4 == m5 && m5 == m6 && m6 == m7 && m7 == m8 =
                         Apply "mat3" [m]
        toExpr (Mat3 (Vec3 (Float xx) (Float xy) (Float xz))
                     (Vec3 (Float yx) (Float yy) (Float yz))
                     (Vec3 (Float zx) (Float zy) (Float zz)))
               = Apply "mat3" [xx, yx, zx, xy, yy, zy, xz, yz, zz]

        fromExpr m = Mat3 (Vec3 (Float (X (X m)))
                                (Float (X (Y m)))
                                (Float (X (Z m))))
                          (Vec3 (Float (Y (X m)))
                                (Float (Y (Y m)))
                                (Float (Y (Z m))))
                          (Vec3 (Float (Z (X m)))
                                (Float (Z (Y m)))
                                (Float (Z (Z m))))

        typeName _ = "mat3"

        size _ = 3

instance ShaderType Mat4 where
        zero = Mat4 zero zero zero zero

        toExpr (Mat4 (Vec4 (Float (X (X m)))
                           (Float (X (Y m1)))
                           (Float (X (Z m2)))
                           (Float (X (W m3))))
                     (Vec4 (Float (Y (X m4)))
                           (Float (Y (Y m5)))
                           (Float (Y (Z m6)))
                           (Float (Y (W m7))))
                     (Vec4 (Float (Z (X m8)))
                           (Float (Z (Y m9)))
                           (Float (Z (Z m10)))
                           (Float (Z (W m11))))
                     (Vec4 (Float (W (X m12)))
                           (Float (W (Y m13)))
                           (Float (W (Z m14)))
                           (Float (W (W m15)))))
               | m == m1 && m1 == m2 && m2 == m3 && m3 == m4 &&
                 m4 == m5 && m5 == m6 && m6 == m7 && m7 == m8 &&
                 m8 == m9 && m9 == m10 && m10 == m11 && m11 == m12 &&
                 m12 == m13 && m13 == m14 && m14 == m15 = Apply "mat4" [m]
        toExpr (Mat4 (Vec4 (Float xx) (Float xy) (Float xz) (Float xw))
                     (Vec4 (Float yx) (Float yy) (Float yz) (Float yw))
                     (Vec4 (Float zx) (Float zy) (Float zz) (Float zw))
                     (Vec4 (Float wx) (Float wy) (Float wz) (Float ww)))
               = Apply "mat4" [ xx, yx, zx, wx
                              , xy, yy, zy, wy
                              , xz, yz, zz, wz
                              , xw, yw, zw, ww ]

        fromExpr m = Mat4 (Vec4 (Float (X (X m)))
                                (Float (X (Y m)))
                                (Float (X (Z m)))
                                (Float (X (W m))))
                          (Vec4 (Float (Y (X m)))
                                (Float (Y (Y m)))
                                (Float (Y (Z m)))
                                (Float (Y (W m))))
                          (Vec4 (Float (Z (X m)))
                                (Float (Z (Y m)))
                                (Float (Z (Z m)))
                                (Float (Z (W m))))
                          (Vec4 (Float (W (X m)))
                                (Float (W (Y m)))
                                (Float (W (Z m)))
                                (Float (W (W m))))

        typeName _ = "mat4"

        size _ = 4

instance Hashable Expr where
        hashWithSalt s e = case e of
                                Empty -> hash2 s 0 (0 :: MInt)
                                Read str -> hash2 s 1 str
                                Op1 str exp -> hash2 s 2 (str, exp)
                                Op2 str exp exp' -> hash2 3 s (str, exp, exp')
                                Apply str exps -> hash2 4 s exps
                                X exp -> hash2 5 s exp
                                Y exp -> hash2 6 s exp
                                Z exp -> hash2 7 s exp
                                W exp -> hash2 8 s exp
                                Literal str -> hash2 s 9 str
                                Action hash -> hash2 s 10 hash
                                Dummy i -> hash2 s 11 i
                                ContextVar i LoopIteration -> hash2 s 12 i
                                ContextVar i LoopValue -> hash2 s 13 i
                                ArrayIndex arr i -> hash2 s 14 (arr, i)

instance Hashable Action where
        hashWithSalt s (Store t e) = hash2 s 0 (t, e)
        hashWithSalt s (If eb tt et ef) = hash2 s 1 (eb, tt, et, ef)
        hashWithSalt s (For iters tv iv eFun) =
                let baseHash = hash (iters, tv, iv, eFun (Dummy 0) (Dummy 1))
                in hash2 s 2 ( baseHash
                             , eFun (Dummy baseHash)
                                    (Dummy $ baseHash Prelude.+ 1))

instance Prelude.Eq Action where
        a == a' = hash a == hash a'

hash2 :: Hashable a => MInt -> MInt -> a -> MInt
hash2 s i x = s `hashWithSalt` i `hashWithSalt` x
