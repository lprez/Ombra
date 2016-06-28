{-# LANGUAGE DataKinds, MultiParamTypeClasses, FunctionalDependencies,
             KindSignatures, TypeOperators, TypeFamilies, GADTs,
             FlexibleInstances, UndecidableInstances, 
             ConstraintKinds, FlexibleContexts #-}
module Graphics.Rendering.Ombra.Shader.Language.Functions where

import Graphics.Rendering.Ombra.Shader.Language.Types

import GHC.Exts (Constraint)
import GHC.TypeLits
import Text.Printf
import Prelude (String, (.), ($), error, Eq)
import qualified Prelude

-- TODO: memoized versions of the functions

class Base a b | a -> b
instance Base Int Int
instance Base IVec2 Int
instance Base IVec3 Int
instance Base IVec4 Int
instance Base Float Float
instance Base Vec2 Float
instance Base Vec3 Float
instance Base Vec4 Float
instance Base Mat2 Float
instance Base Mat3 Float
instance Base Mat4 Float

class (Base a aBase, Base b bBase) =>
      Arithmetic aBase bBase a b result | a b -> result
                                        , b -> aBase bBase
                                        , a -> aBase bBase
                                        , result -> aBase bBase

instance Arithmetic Float Float Float Float Float
instance Arithmetic Float Float Vec2 Vec2 Vec2
instance Arithmetic Float Float Vec3 Vec3 Vec3
instance Arithmetic Float Float Vec4 Vec4 Vec4
instance Arithmetic Float Float Vec2 Float Vec2
instance Arithmetic Float Float Vec3 Float Vec3
instance Arithmetic Float Float Vec4 Float Vec4
instance Arithmetic Float Float Float Vec2 Vec2
instance Arithmetic Float Float Float Vec3 Vec3
instance Arithmetic Float Float Float Vec4 Vec4
instance Arithmetic Float Float Mat2 Mat2 Mat2
instance Arithmetic Float Float Mat3 Mat3 Mat3
instance Arithmetic Float Float Mat4 Mat4 Mat4
instance Arithmetic Float Float Mat2 Float Mat2
instance Arithmetic Float Float Mat3 Float Mat3
instance Arithmetic Float Float Mat4 Float Mat4
instance Arithmetic Float Float Float Mat2 Mat2
instance Arithmetic Float Float Float Mat3 Mat3
instance Arithmetic Float Float Float Mat4 Mat4

instance Arithmetic Int Int Int Int Int
instance Arithmetic Int Int IVec2 IVec2 IVec2
instance Arithmetic Int Int IVec3 IVec3 IVec3
instance Arithmetic Int Int IVec4 IVec4 IVec4
instance Arithmetic Int Int IVec2 Int IVec2
instance Arithmetic Int Int IVec3 Int IVec3
instance Arithmetic Int Int IVec4 Int IVec4
instance Arithmetic Int Int Int IVec2 IVec2
instance Arithmetic Int Int Int IVec3 IVec3
instance Arithmetic Int Int Int IVec4 IVec4

-- | Types that can be multiplied.
class (Base a aBase, Base b bBase) =>
      Mul aBase bBase a b result | a b -> result
                                 , b -> aBase bBase
                                 , a -> aBase bBase
                                 , result -> aBase bBase
instance Mul Float Float Mat2 Vec2 Vec2
instance Mul Float Float Mat3 Vec3 Vec3
instance Mul Float Float Mat4 Vec4 Vec4
instance Mul Float Float Vec2 Mat2 Vec2
instance Mul Float Float Vec3 Mat3 Vec3
instance Mul Float Float Vec4 Mat4 Vec4
instance {-# OVERLAPPABLE #-} 
         ( Arithmetic aBase bBase a b result
         , Base a aBase, Base b bBase) =>
         Mul aBase bBase a b result

class (ShaderType a, Base a Float) => FloatVec a
instance FloatVec Vec2
instance FloatVec Vec3
instance FloatVec Vec4

-- | Floats or vectors.
class ShaderType a => GenType a
instance {-# OVERLAPS #-} GenType Float
instance {-# OVERLAPPABLE #-} (FloatVec a, ShaderType a) => GenType a

type family GenTypeFloatConstr a b where
        GenTypeFloatConstr a Float = GenType a
        GenTypeFloatConstr a a = GenType a

type GenTypeFloat a b = (GenTypeFloatConstr a b, ShaderType a, ShaderType b)

infixl 7 *
(*) :: (Mul aBase bBase a b c, ShaderType a, ShaderType b, ShaderType c)
    => a -> b -> c
(*) = op2 "*"

infixl 7 /
(/) :: (Arithmetic aBase bBase a b c, ShaderType a, ShaderType b, ShaderType c)
    => a -> b -> c
(/) = op2 "/"

infixl 6 +
(+) :: (Arithmetic aBase bBase a b c, ShaderType a, ShaderType b, ShaderType c)
    => a -> b -> c
(+) = op2 "+"

infixl 6 -
(-) :: (Arithmetic aBase bBase a b c, ShaderType a, ShaderType b, ShaderType c)
    => a -> b -> c
(-) = op2 "-"

infixr 8 ^
(^) :: (ShaderType a, GenType a) => a -> a -> a
(^) = fun2 "pow"

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
(&&) = op2 "&&"

infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) = op2 "||"

infix 4 ==
(==) :: ShaderType a => a -> a -> Bool
(==) = op2 "=="

infix 4 /=
(/=) :: ShaderType a => a -> a -> Bool
(/=) = op2 "!="

infix 4 >=
(>=) :: ShaderType a => a -> a -> Bool
(>=) = op2 ">="

infix 4 <=
(<=) :: ShaderType a => a -> a -> Bool
(<=) = op2 "<="

infix 4 <
(<) :: ShaderType a => a -> a -> Bool
(<) = op2 "<"

infix 4 >
(>) :: ShaderType a => a -> a -> Bool
(>) = op2 ">"

class ShaderType a => VecOrd a
instance VecOrd Vec2
instance VecOrd Vec3
instance VecOrd Vec4
instance VecOrd IVec2
instance VecOrd IVec3
instance VecOrd IVec4

class ShaderType a => VecEq a
instance VecEq Vec2
instance VecEq Vec3
instance VecEq Vec4
instance VecEq IVec2
instance VecEq IVec3
instance VecEq IVec4
instance VecEq BVec2
instance VecEq BVec3
instance VecEq BVec4

lessThan :: VecOrd a => a -> a -> Bool
lessThan = fun2 "lessThan"

lessThanEqual :: VecOrd a => a -> a -> Bool
lessThanEqual = fun2 "lessThanEqual"

greaterThan :: VecOrd a => a -> a -> Bool
greaterThan = fun2 "greaterThan"

greaterThanEqual :: VecOrd a => a -> a -> Bool
greaterThanEqual = fun2 "greaterThanEqual"

equal :: VecEq a => a -> a -> Bool
equal = fun2 "equal"

notEqual :: VecEq a => a -> a -> Bool
notEqual = fun2 "notEqual"

class ShaderType a => BoolVector a
instance BoolVector BVec2
instance BoolVector BVec3
instance BoolVector BVec4

anyB :: BoolVector a => a -> Bool
anyB = fun1 "any"

allB :: BoolVector a => a -> Bool
allB = fun1 "all"

notB :: BoolVector a => a -> Bool
notB = fun1 "not"

negate :: GenType a => a -> a
negate = op1 "-"

not :: GenType a => a -> a
not = op1 "!"

class (ShaderType a, Base a a) => Num a where
        fromInteger :: Prelude.Integer -> a

instance Num Float where
        fromInteger = fromRational . Prelude.fromInteger

instance Num Int where
        fromInteger = Int . Literal
                          . (printf "%d" :: Prelude.Integer -> String)
                          . Prelude.fromInteger

fromRational :: Prelude.Rational -> Float
fromRational = Float . Literal
                     . (printf "%f" :: Prelude.Float -> String)
                     . Prelude.fromRational

radians :: GenType a => a -> a
radians = fun1 "radians"

degrees :: GenType a => a -> a
degrees = fun1 "degrees"

sin :: GenType a => a -> a
sin = fun1 "sin"

cos :: GenType a => a -> a
cos = fun1 "cos"

tan :: GenType a => a -> a
tan = fun1 "tan"

asin :: GenType a => a -> a
asin = fun1 "asin"

acos :: GenType a => a -> a
acos = fun1 "acos"

atan :: GenType a => a -> a
atan = fun1 "atan"

atan2 :: GenType a => a -> a -> a
atan2 = fun2 "atan"

exp :: GenType a => a -> a
exp = fun1 "exp"

log :: GenType a => a -> a
log = fun1 "log"

exp2 :: GenType a => a -> a
exp2 = fun1 "exp2"

log2 :: GenType a => a -> a
log2 = fun1 "log2"

sqrt :: GenType a => a -> a
sqrt = fun1 "sqrt"

inversesqrt :: GenType a => a -> a
inversesqrt = fun1 "inversesqrt"

abs :: GenType a => a -> a
abs = fun1 "abs"

sign :: GenType a => a -> a
sign = fun1 "sign"

floor :: GenType a => a -> a
floor = fun1 "floor"

ceil :: GenType a => a -> a
ceil = fun1 "ceil"

fract :: GenType a => a -> a
fract = fun1 "fract"

mod :: GenTypeFloat a b => a -> b -> a
mod = fun2 "mod"

min :: GenTypeFloat a b => a -> b -> a
min = fun2 "min"

max :: GenTypeFloat a b => a -> b -> a
max = fun2 "max"

clamp :: GenTypeFloat a b => a -> b -> b -> a
clamp = fun3 "clamp"

mix :: GenTypeFloat a b => a -> a -> b -> a
mix = fun3 "mix"

step :: GenTypeFloat a b => b -> a -> a
step = fun2 "step"

smoothstep :: GenTypeFloat a b => b -> b -> a -> a
smoothstep = fun3 "smoothstep"

length :: GenType a => a -> Float
length = fun1 "length"

arrayLength :: (ShaderType t, KnownNat n) => Array n t -> Int
arrayLength = fun1 "length"

(!) :: (ShaderType t, KnownNat n) => Array n t -> Int -> t
arr ! i = fromExpr $ ArrayIndex (toExpr arr) (toExpr i)

distance :: GenType a => a -> a -> Float
distance = fun2 "distance"

dot :: GenType a => a -> a -> Float
dot = fun2 "dot"

cross :: Vec3 -> Vec3 -> Vec3
cross = fun2 "cross"

normalize :: GenType a => a -> a
normalize = fun1 "normalize"

faceforward :: GenType a => a -> a -> a -> a
faceforward = fun3 "faceforward"

reflect :: GenType a => a -> a -> a
reflect = fun2 "reflect"

refract :: GenType a => a -> a -> Float -> a
refract = fun3 "refract"

class ShaderType a => Matrix a
instance Matrix Mat2
instance Matrix Mat3
instance Matrix Mat4

-- TODO: unsafe
matrixCompMult :: (Matrix a, Matrix b, Matrix c) => a -> b -> c
matrixCompMult = fun2 "matrixCompMult"

-- | Avoid evaluating the expression of the argument more than one time.
-- Conditionals and loops imply it.
store :: ShaderType a => a -> a
store x = fromExpr . Action $ Store (typeName x) (toExpr x)

true :: Bool
true = Bool $ Literal "true"

false :: Bool
false = Bool $ Literal "false"

-- | Rebound if. You don't need to use this function, with -XRebindableSyntax.
ifThenElse :: ShaderType a => Bool -> a -> a -> a
ifThenElse b t f = fromExpr . Action $ If (toExpr b) (typeName t)
                                          (toExpr t) (toExpr f)

loop :: ShaderType a 
     => Int -- ^ Maximum number of iterations (should be as low as possible, must be an integer literal)
     -> a -- ^ Initial value
     -> (Int -> a -> (a, Bool)) -- ^ Iteration -> Old value -> (Next, Stop)
     -> a
loop (Int (Literal iters)) iv f =
        fromExpr . Action $
                For (Prelude.read iters :: Prelude.Int)
                    (typeName iv)
                    (toExpr iv)
                    (\ie ve -> let (next, stop) = f (fromExpr ie) (fromExpr ve)
                               in (toExpr next, toExpr stop))
loop _ _ _ = error "loop: iteration number is not a literal."

texture2D :: Sampler2D -> Vec2 -> Vec4
texture2D = fun2 "texture2D"

texture2DBias :: Sampler2D -> Vec2 -> Float -> Vec4
texture2DBias = fun3 "texture2DBias"

texture2DProj :: Sampler2D -> Vec3 -> Vec4
texture2DProj = fun2 "texture2DProj"

texture2DProjBias :: Sampler2D -> Vec3 -> Float -> Vec4
texture2DProjBias = fun3 "texture2DProj"

texture2DProj4 :: Sampler2D -> Vec4 -> Vec4
texture2DProj4 = fun2 "texture2DProj"

texture2DProjBias4 :: Sampler2D -> Vec4 -> Float -> Vec4
texture2DProjBias4 = fun3 "texture2DProj"

texture2DLod :: Sampler2D -> Vec2 -> Float -> Vec4
texture2DLod = fun3 "texture2DLod"

texture2DProjLod :: Sampler2D -> Vec3 -> Float -> Vec4
texture2DProjLod = fun3 "texture2DProjLod"

texture2DProjLod4 :: Sampler2D -> Vec4 -> Float -> Vec4
texture2DProjLod4 = fun3 "texture3DProjLod"

textureCube :: SamplerCube -> Vec3 -> Vec4
textureCube = fun2 "textureCube"

textureCubeBias :: SamplerCube -> Vec3 -> Float -> Vec4
textureCubeBias = fun3 "textureCube"

textureCubeLod :: SamplerCube -> Vec3 -> Float -> Vec4
textureCubeLod = fun3 "textureCubeLod"

-- | The position of the vertex (only works in the vertex shader).
position :: Vec4
position = fromExpr $ Read "gl_Position"

-- | The data of the fragment (only works in the fragment shader).
fragData :: Array 16 Vec4
fragData = fromExpr $ Read "gl_FragData"

-- | The coordinates of the fragment (only works in the fragment shader).
fragCoord :: Vec4
fragCoord = fromExpr $ Read "gl_FragCoord"

-- | If the fragment belongs to a front-facing primitive (only works in the
-- fragment shader).
fragFrontFacing :: Bool
fragFrontFacing = fromExpr $ Read "gl_FrontFacing"

class ShaderType t => ToInt t
instance ToInt Float
instance ToInt Bool
instance ToInt Int

int :: ToInt t => t -> Int
int = fun1 "int"

class ShaderType t => ToBool t
instance ToBool Float
instance ToBool Bool
instance ToBool Int

bool :: ToBool t => t -> Bool
bool = fun1 "bool"

class ShaderType t => ToFloat t
instance ToFloat Float
instance ToFloat Bool
instance ToFloat Int

float :: ToFloat t => t -> Float
float = fun1 "float"

class ToVec2 t where
        vec2 :: t -> Vec2

instance {-# OVERLAPPING #-} ToVec2 Float where
        vec2 = fun1 "vec2"

instance {-# OVERLAPPABLE #-}
         (Components Vec2 <= n, ToCompList t n) => ToVec2 t where
        vec2 = funCompList "vec2"

class ToVec3 t where
        vec3 :: t -> Vec3

instance {-# OVERLAPPING #-} ToVec3 Float where
        vec3 = fun1 "vec3"

instance {-# OVERLAPPABLE #-}
         (Components Vec3 <= n, ToCompList t n) => ToVec3 t where
        vec3 = funCompList "vec3"

class ToVec4 t where
        vec4 :: t -> Vec4

instance {-# OVERLAPPING #-} ToVec4 Float where
        vec4 = fun1 "vec4"

instance {-# OVERLAPPABLE #-}
         (Components Vec4 <= n, ToCompList t n) => ToVec4 t where
        vec4 = funCompList "vec4"

class ToIVec2 t where
        ivec2 :: t -> IVec2

instance {-# OVERLAPPING #-} ToIVec2 Float where
        ivec2 = fun1 "ivec2"

instance {-# OVERLAPPABLE #-}
         (Components IVec2 <= n, ToCompList t n) => ToIVec2 t where
        ivec2 = funCompList "ivec2"

class ToIVec3 t where
        ivec3 :: t -> IVec3

instance {-# OVERLAPPING #-} ToIVec3 Float where
        ivec3 = fun1 "ivec3"

instance {-# OVERLAPPABLE #-}
         (Components IVec3 <= n, ToCompList t n) => ToIVec3 t where
        ivec3 = funCompList "ivec3"

class ToIVec4 t where
        ivec4 :: t -> IVec4

instance {-# OVERLAPPING #-} ToIVec4 Float where
        ivec4 = fun1 "ivec4"

instance {-# OVERLAPPABLE #-}
         (Components IVec4 <= n, ToCompList t n) => ToIVec4 t where
        ivec4 = funCompList "ivec4"

class ToBVec2 t where
        bvec2 :: t -> BVec2

instance {-# OVERLAPPING #-} ToBVec2 Float where
        bvec2 = fun1 "bvec2"

instance {-# OVERLAPPABLE #-}
         (Components BVec2 <= n, ToCompList t n) => ToBVec2 t where
        bvec2 = funCompList "bvec2"

class ToBVec3 t where
        bvec3 :: t -> BVec3

instance {-# OVERLAPPING #-} ToBVec3 Float where
        bvec3 = fun1 "bvec3"

instance {-# OVERLAPPABLE #-}
         (Components BVec3 <= n, ToCompList t n) => ToBVec3 t where
        bvec3 = funCompList "bvec3"

class ToBVec4 t where
        bvec4 :: t -> BVec4

instance {-# OVERLAPPING #-} ToBVec4 Float where
        bvec4 = fun1 "bvec4"

instance {-# OVERLAPPABLE #-}
         (Components BVec4 <= n, ToCompList t n) => ToBVec4 t where
        bvec4 = funCompList "bvec4"

class ToMat2 t where
        mat2 :: t -> Mat2

instance {-# OVERLAPPING #-} ToMat2 Float where
        mat2 = fun1 "mat2"

instance {-# OVERLAPPABLE #-}
         (Components Mat2 <= n, ToCompList t n) => ToMat2 t where
        mat2 = funCompList "mat2"

class ToMat3 t where
        mat3 :: t -> Mat3

instance {-# OVERLAPPING #-} ToMat3 Float where
        mat3 = fun1 "mat3"

instance {-# OVERLAPPABLE #-}
         (Components Mat3 <= n, ToCompList t n) => ToMat3 t where
        mat3 = funCompList "mat3"

class ToMat4 t where
        mat4 :: t -> Mat4

instance {-# OVERLAPPING #-} ToMat4 Float where
        mat4 = fun1 "mat4"

instance {-# OVERLAPPABLE #-}
         (Components Mat4 <= n, ToCompList t n) => ToMat4 t where
        mat4 = funCompList "mat4"

-- | Useful type for constructing vectors and matrices from scalars, vectors and
-- matrices.
data CompList (count :: Nat) where
        CL :: (1 <= Components t, ShaderType t) => t -> CompList (Components t)
        CLAppend :: CompList x -> CompList y -> CompList (x + y)

class ToCompList x (n :: Nat) | x -> n where
        toCompList :: x -> CompList n

instance {-# OVERLAPPING #-} ToCompList (CompList n) n where
        toCompList = Prelude.id

instance {-# OVERLAPPABLE #-}
         (1 <= n, ShaderType t, n ~ (Components t)) => ToCompList t n where
        toCompList = CL

-- | You can call \*vec\* and mat\* with a single scalar or with a 'CompList'
-- containing enough components. This function helps you create 'CompList's.
--
-- Examples:
--
-- > vec2 0
-- > mat2 $ Vec2 2 4 # Vec2 1 3
-- > vec4 $ mat2 (0 # 1 # vec2 2) # 9  -- 9 is discarded
-- > mat4 $ 5 # vec2 5 # Vec3 1 2 3 # Mat2 (vec2 0) (Vec2 1 2) # mat3 0
-- > vec4 $ 1 # vec2 0 -- Not enough components, fails with "Couldn't match type
-- >                   -- ‘'Prelude.False’ with 'Prelude.True’" (because
-- >                   -- Components Vec4 <=? 3 ~ False).
(#) :: (ToCompList x xn, ToCompList y yn) => x -> y -> CompList (xn + yn)
x # y = CLAppend (toCompList x) (toCompList y)

infixr 5 #

type family Components (t :: *) :: Nat where
        Components Int = 1
        Components Float = 1
        Components Bool = 1
        Components Vec2 = 2
        Components IVec2 = 2
        Components BVec2 = 2
        Components Vec3 = 3
        Components IVec3 = 3
        Components BVec3 = 3
        Components Vec4 = 4
        Components IVec4 = 4
        Components BVec4 = 4
        Components Mat2 = 4
        Components Mat3 = 9
        Components Mat4 = 16
        Components x = 0

op1 :: (ShaderType a, ShaderType b) => String -> a -> b
op1 name = fromExpr . Op1 name . toExpr

op2 :: (ShaderType a, ShaderType b, ShaderType c) => String -> a -> b -> c
op2 name x y = fromExpr $ Op2 name (toExpr x) (toExpr y)

fun1 :: (ShaderType a, ShaderType b) => String -> a -> b
fun1 name x = fromExpr $ Apply name [toExpr x]

fun2 :: (ShaderType a, ShaderType b, ShaderType c) => String -> a -> b -> c
fun2 name x y = fromExpr $ Apply name [toExpr x, toExpr y]

fun3 :: (ShaderType a, ShaderType b, ShaderType c, ShaderType d)
     => String -> a -> b -> c -> d
fun3 name x y z = fromExpr $ Apply name [toExpr x, toExpr y, toExpr z]

funCompList :: (ToCompList cl n, ShaderType r) => String -> cl -> r
funCompList name = fromExpr . Apply name . toExprList . toCompList
        where toExprList :: CompList n -> [Expr]
              toExprList (CL x) = [toExpr x]
              toExprList (CLAppend c1 c2) =
                      toExprList c1 Prelude.++ toExprList c2
