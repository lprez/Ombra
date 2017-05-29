{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures,
             ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Shader.Language.Types where

import Data.Typeable
import GHC.TypeLits
import Data.Hashable
import Prelude (String, ($), error, Eq(..), (++), (*), fromInteger, (&&), Int)
import qualified Prelude

-- | An expression.
data Expr = Empty | Read String | Op1 String Expr | Op2 String Expr Expr
          | Apply String [Expr] | X Expr | Y Expr | Z Expr | W Expr
          | Literal String | Action Action | Dummy Int | ArrayIndex Expr Expr
          | ContextVar Int ContextVarType
          deriving Eq

-- | Expressions that are transformed to statements.
data Action = Store String Expr | If Expr String Expr Expr
            | For Int String Expr (Expr -> Expr -> (Expr, Expr))

data ContextVarType = LoopIteration | LoopValue deriving Eq

-- | A GPU boolean.
newtype GBool = GBool Expr 

-- | A GPU 32-bit float.
newtype GFloat = GFloat Expr 

-- | A GPU 32-bit integer.
newtype GInt = GInt Expr 

-- | This represents an active 2D texture in the GPU.
newtype GSampler2D = GSampler2D Expr 

newtype GSamplerCube = GSamplerCube Expr 

-- | The type of a generic expression.
newtype Unknown = Unknown Expr


-- | A GPU 2D float vector.
data GVec2 = GVec2 GFloat GFloat 

-- | A GPU 3D float vector.
data GVec3 = GVec3 GFloat GFloat GFloat 

-- | A GPU 4D float vector.
data GVec4 = GVec4 GFloat GFloat GFloat GFloat 

-- | A GPU 2D integer vector.
data GIVec2 = GIVec2 GInt GInt 

-- | A GPU 3D integer vector.
data GIVec3 = GIVec3 GInt GInt GInt 

-- | A GPU 4D integer vector.
data GIVec4 = GIVec4 GInt GInt GInt GInt 

-- | A GPU 2D boolean vector.
data GBVec2 = GBVec2 GBool GBool 

-- | A GPU 3D boolean vector.
data GBVec3 = GBVec3 GBool GBool GBool 

-- | A GPU 4D boolean vector.
data GBVec4 = GBVec4 GBool GBool GBool GBool 

-- | A GPU 2x2 float matrix.
data GMat2 = GMat2 GVec2 GVec2 

-- | A GPU 3x3 float matrix.
data GMat3 = GMat3 GVec3 GVec3 GVec3 

-- | A GPU 4x4 float matrix.
data GMat4 = GMat4 GVec4 GVec4 GVec4 GVec4 

-- | A GPU array.
data GArray (n :: Nat) t = GArray Expr 

-- | A type in the GPU.
class ShaderType t where
        zero :: t

        toExpr :: t -> Expr

        fromExpr :: Expr -> t

        typeName :: t -> String

        size :: t -> Int

instance ShaderType Unknown where
        zero = error "zero: Unknown type."
        toExpr (Unknown e) = e
        fromExpr = Unknown
        typeName = error "typeName: Unknown type."
        size = error "size: Unknown type."

instance (ShaderType t, KnownNat n) => ShaderType (GArray n t) where
        zero = error "zero: Unsupported constant arrays."
        toExpr (GArray e) = e
        fromExpr = GArray
        typeName (GArray _ :: GArray n t) =
                typeName (zero :: t) ++
                "[" ++ Prelude.show (natVal (Proxy :: Proxy n)) ++ "]"
        size (GArray _ :: GArray n t) =
                size (zero :: t) * fromInteger (natVal (Proxy :: Proxy n))

instance ShaderType GBool where
        zero = GBool $ Literal "false"

        toExpr (GBool e) = e

        fromExpr = GBool

        typeName _ = "bool"

        size _ = 1

instance ShaderType GInt where
        zero = GInt $ Literal "0"

        toExpr (GInt e) = e

        fromExpr = GInt

        typeName _ = "int"

        size _ = 1

instance ShaderType GFloat where
        zero = GFloat $ Literal "0.0"

        toExpr (GFloat e) = e

        fromExpr = GFloat

        typeName _ = "float"

        size _ = 1

instance ShaderType GSampler2D where
        zero = GSampler2D $ Literal "0"

        toExpr (GSampler2D e) = e

        fromExpr = GSampler2D

        typeName _ = "sampler2D"

        size _ = 1

instance ShaderType GSamplerCube where
        zero = GSamplerCube $ Literal "0"

        toExpr (GSamplerCube e) = e

        fromExpr = GSamplerCube

        typeName _ = "samplerCube"

        size _ = 1

instance ShaderType GVec2 where
        zero = GVec2 zero zero

        toExpr (GVec2 (GFloat (X v)) (GFloat (Y v'))) | v == v' =
                Apply "vec2" [v]

        toExpr (GVec2 (GFloat x) (GFloat y)) = Apply "vec2" [x, y]

        fromExpr v = GVec2 (GFloat (X v)) (GFloat (Y v))

        typeName _ = "vec2"

        size _ = 1

instance ShaderType GVec3 where
        zero = GVec3 zero zero zero

        toExpr (GVec3 (GFloat (X v)) (GFloat (Y v')) (GFloat (Z v'')))
               | v == v' && v' == v'' = Apply "vec3" [v]
        toExpr (GVec3 (GFloat x) (GFloat y) (GFloat z)) =
                Apply "vec3" [x, y, z]

        fromExpr v = GVec3 (GFloat (X v)) (GFloat (Y v)) (GFloat (Z v))

        typeName _ = "vec3"

        size _ = 1

instance ShaderType GVec4 where
        zero = GVec4 zero zero zero zero

        toExpr (GVec4 (GFloat (X v))
                      (GFloat (Y v1))
                      (GFloat (Z v2))
                      (GFloat (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 = Apply "vec4" [v]
        toExpr (GVec4 (GFloat x) (GFloat y) (GFloat z) (GFloat w)) =
                Apply "vec4" [x, y, z, w]

        fromExpr v = GVec4 (GFloat (X v)) (GFloat (Y v)) (GFloat (Z v)) (GFloat (W v))

        typeName _ = "vec4"

        size _ = 1

instance ShaderType GIVec2 where
        zero = GIVec2 zero zero

        toExpr (GIVec2 (GInt (X v)) (GInt (Y v'))) | v == v' = Apply "ivec2" [v]
        toExpr (GIVec2 (GInt x) (GInt y)) = Apply "ivec2" [x, y]

        fromExpr v = GIVec2 (GInt (X v)) (GInt (Y v))

        typeName _ = "ivec2"

        size _ = 1

instance ShaderType GIVec3 where
        zero = GIVec3 zero zero zero

        toExpr (GIVec3 (GInt (X v)) (GInt (Y v')) (GInt (Z v'')))
               | v == v' && v' == v'' = Apply "ivec3" [v]
        toExpr (GIVec3 (GInt x) (GInt y) (GInt z)) = Apply "ivec3" [x, y, z]

        fromExpr v = GIVec3 (GInt (X v)) (GInt (Y v)) (GInt (Z v))

        typeName _ = "ivec3"

        size _ = 1

instance ShaderType GIVec4 where
        zero = GIVec4 zero zero zero zero

        toExpr (GIVec4 (GInt (X v)) (GInt (Y v1)) (GInt (Z v2)) (GInt (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 = Apply "ivec4" [v]
        toExpr (GIVec4 (GInt x) (GInt y) (GInt z) (GInt w)) =
                Apply "ivec4" [x, y, z, w]

        fromExpr v = GIVec4 (GInt (X v)) (GInt (Y v)) (GInt (Z v)) (GInt (W v))

        typeName _ = "ivec4"

        size _ = 1

instance ShaderType GBVec2 where
        zero = GBVec2 zero zero

        toExpr (GBVec2 (GBool (X v)) (GBool (Y v'))) | v == v' =
                Apply "bvec2" [v]

        toExpr (GBVec2 (GBool x) (GBool y)) = Apply "bvec2" [x, y]

        fromExpr v = GBVec2 (GBool (X v)) (GBool (Y v))

        typeName _ = "bvec2"

        size _ = 1

instance ShaderType GBVec3 where
        zero = GBVec3 zero zero zero

        toExpr (GBVec3 (GBool (X v)) (GBool (Y v')) (GBool (Z v'')))
               | v == v' && v' == v'' = Apply "bvec3" [v]
        toExpr (GBVec3 (GBool x) (GBool y) (GBool z)) = Apply "bvec3" [x, y, z]

        fromExpr v = GBVec3 (GBool (X v)) (GBool (Y v)) (GBool (Z v))

        typeName _ = "bvec3"

        size _ = 1

instance ShaderType GBVec4 where
        zero = GBVec4 zero zero zero zero

        toExpr (GBVec4 (GBool (X v))
                       (GBool (Y v1))
                       (GBool (Z v2))
                       (GBool (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 = Apply "bvec4" [v]
        toExpr (GBVec4 (GBool x) (GBool y) (GBool z) (GBool w)) =
                Apply "bvec4" [x, y, z, w]

        fromExpr v = GBVec4 (GBool (X v)) (GBool (Y v))
                            (GBool (Z v)) (GBool (W v))

        typeName _ = "bvec4"

        size _ = 1

instance ShaderType GMat2 where
        zero = GMat2 zero zero

        toExpr (GMat2 (GVec2 (GFloat (X (X m))) (GFloat (X (Y m1))))
                      (GVec2 (GFloat (Y (X m2))) (GFloat (Y (Y m3)))))
               | m == m1 && m1 == m2 && m2 == m3 = Apply "mat2" [m]
        toExpr (GMat2 (GVec2 (GFloat xx) (GFloat xy))
                      (GVec2 (GFloat yx) (GFloat yy)))
               = Apply "mat2" [xx, yx, xy, yy]

        fromExpr m = GMat2 (GVec2 (GFloat (X (X m))) (GFloat (Y (X m))))
                           (GVec2 (GFloat (Y (X m))) (GFloat (Y (Y m))))

        typeName _ = "mat2"

        size _ = 2

instance ShaderType GMat3 where
        zero = GMat3 zero zero zero

        toExpr (GMat3 (GVec3 (GFloat (X (X m)))
                             (GFloat (X (Y m1)))
                             (GFloat (X (Z m2))))
                      (GVec3 (GFloat (Y (X m3)))
                             (GFloat (Y (Y m4)))
                             (GFloat (Y (Z m5))))
                      (GVec3 (GFloat (Z (X m6)))
                             (GFloat (Z (Y m7)))
                             (GFloat (Z (Z m8)))))
               | m == m1 && m1 == m2 && m2 == m3 && m3 == m4 &&
                 m4 == m5 && m5 == m6 && m6 == m7 && m7 == m8 =
                         Apply "mat3" [m]
        toExpr (GMat3 (GVec3 (GFloat xx) (GFloat xy) (GFloat xz))
                      (GVec3 (GFloat yx) (GFloat yy) (GFloat yz))
                      (GVec3 (GFloat zx) (GFloat zy) (GFloat zz)))
               = Apply "mat3" [xx, yx, zx, xy, yy, zy, xz, yz, zz]

        fromExpr m = GMat3 (GVec3 (GFloat (X (X m)))
                                  (GFloat (X (Y m)))
                                  (GFloat (X (Z m))))
                           (GVec3 (GFloat (Y (X m)))
                                  (GFloat (Y (Y m)))
                                  (GFloat (Y (Z m))))
                           (GVec3 (GFloat (Z (X m)))
                                  (GFloat (Z (Y m)))
                                  (GFloat (Z (Z m))))

        typeName _ = "mat3"

        size _ = 3

instance ShaderType GMat4 where
        zero = GMat4 zero zero zero zero

        toExpr (GMat4 (GVec4 (GFloat (X (X m)))
                             (GFloat (X (Y m1)))
                             (GFloat (X (Z m2)))
                             (GFloat (X (W m3))))
                      (GVec4 (GFloat (Y (X m4)))
                             (GFloat (Y (Y m5)))
                             (GFloat (Y (Z m6)))
                             (GFloat (Y (W m7))))
                      (GVec4 (GFloat (Z (X m8)))
                             (GFloat (Z (Y m9)))
                             (GFloat (Z (Z m10)))
                             (GFloat (Z (W m11))))
                      (GVec4 (GFloat (W (X m12)))
                             (GFloat (W (Y m13)))
                             (GFloat (W (Z m14)))
                             (GFloat (W (W m15)))))
               | m == m1 && m1 == m2 && m2 == m3 && m3 == m4 &&
                 m4 == m5 && m5 == m6 && m6 == m7 && m7 == m8 &&
                 m8 == m9 && m9 == m10 && m10 == m11 && m11 == m12 &&
                 m12 == m13 && m13 == m14 && m14 == m15 = Apply "mat4" [m]
        toExpr (GMat4 (GVec4 (GFloat xx) (GFloat xy) (GFloat xz) (GFloat xw))
                      (GVec4 (GFloat yx) (GFloat yy) (GFloat yz) (GFloat yw))
                      (GVec4 (GFloat zx) (GFloat zy) (GFloat zz) (GFloat zw))
                      (GVec4 (GFloat wx) (GFloat wy) (GFloat wz) (GFloat ww)))
               = Apply "mat4" [ xx, yx, zx, wx
                              , xy, yy, zy, wy
                              , xz, yz, zz, wz
                              , xw, yw, zw, ww ]

        fromExpr m = GMat4 (GVec4 (GFloat (X (X m)))
                                  (GFloat (X (Y m)))
                                  (GFloat (X (Z m)))
                                  (GFloat (X (W m))))
                           (GVec4 (GFloat (Y (X m)))
                                  (GFloat (Y (Y m)))
                                  (GFloat (Y (Z m)))
                                  (GFloat (Y (W m))))
                           (GVec4 (GFloat (Z (X m)))
                                  (GFloat (Z (Y m)))
                                  (GFloat (Z (Z m)))
                                  (GFloat (Z (W m))))
                           (GVec4 (GFloat (W (X m)))
                                  (GFloat (W (Y m)))
                                  (GFloat (W (Z m)))
                                  (GFloat (W (W m))))

        typeName _ = "mat4"

        size _ = 4

instance Hashable Expr where
        hashWithSalt s e = case e of
                                Empty -> hash2 s 0 (0 :: Int)
                                Read str -> hash2 s 1 str
                                Op1 str exp -> hash2 s 2 (str, exp)
                                Op2 str exp exp' -> hash2 3 s (str, exp, exp')
                                Apply str exps -> hash2 4 s (str, exps)
                                X exp -> hash2 5 s exp
                                Y exp -> hash2 6 s exp
                                Z exp -> hash2 7 s exp
                                W exp -> hash2 8 s exp
                                Literal str -> hash2 s 9 str
                                Action actHash -> hash2 s 10 actHash
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

hash2 :: Hashable a => Int -> Int -> a -> Int
hash2 s i x = s `hashWithSalt` i `hashWithSalt` x
