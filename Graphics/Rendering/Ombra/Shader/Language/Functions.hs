{-# LANGUAGE DataKinds, MultiParamTypeClasses, FunctionalDependencies,
             KindSignatures, TypeOperators, TypeFamilies, GADTs,
             FlexibleInstances, UndecidableInstances, 
             ConstraintKinds, FlexibleContexts #-}
module Graphics.Rendering.Ombra.Shader.Language.Functions where

import Graphics.Rendering.Ombra.Shader.Language.Types

import GHC.TypeLits
import Text.Printf
import Prelude (String, (.), ($), error, Int, Integer, Float)
import qualified Prelude

-- TODO: memoized versions of the functions

class Base a b | a -> b
instance Base GInt GInt
instance Base GIVec2 GInt
instance Base GIVec3 GInt
instance Base GIVec4 GInt
instance Base GFloat GFloat
instance Base GVec2 GFloat
instance Base GVec3 GFloat
instance Base GVec4 GFloat
instance Base GMat2 GFloat
instance Base GMat3 GFloat
instance Base GMat4 GFloat

class (Base a aBase, Base b bBase) =>
      Arithmetic aBase bBase a b result | a b -> result
                                        , b -> aBase bBase
                                        , a -> aBase bBase
                                        , result -> aBase bBase

instance Arithmetic GFloat GFloat GFloat GFloat GFloat
instance Arithmetic GFloat GFloat GVec2 GVec2 GVec2
instance Arithmetic GFloat GFloat GVec3 GVec3 GVec3
instance Arithmetic GFloat GFloat GVec4 GVec4 GVec4
instance Arithmetic GFloat GFloat GVec2 GFloat GVec2
instance Arithmetic GFloat GFloat GVec3 GFloat GVec3
instance Arithmetic GFloat GFloat GVec4 GFloat GVec4
instance Arithmetic GFloat GFloat GFloat GVec2 GVec2
instance Arithmetic GFloat GFloat GFloat GVec3 GVec3
instance Arithmetic GFloat GFloat GFloat GVec4 GVec4
instance Arithmetic GFloat GFloat GMat2 GMat2 GMat2
instance Arithmetic GFloat GFloat GMat3 GMat3 GMat3
instance Arithmetic GFloat GFloat GMat4 GMat4 GMat4
instance Arithmetic GFloat GFloat GMat2 GFloat GMat2
instance Arithmetic GFloat GFloat GMat3 GFloat GMat3
instance Arithmetic GFloat GFloat GMat4 GFloat GMat4
instance Arithmetic GFloat GFloat GFloat GMat2 GMat2
instance Arithmetic GFloat GFloat GFloat GMat3 GMat3
instance Arithmetic GFloat GFloat GFloat GMat4 GMat4

instance Arithmetic GInt GInt GInt GInt GInt
instance Arithmetic GInt GInt GIVec2 GIVec2 GIVec2
instance Arithmetic GInt GInt GIVec3 GIVec3 GIVec3
instance Arithmetic GInt GInt GIVec4 GIVec4 GIVec4
instance Arithmetic GInt GInt GIVec2 GInt GIVec2
instance Arithmetic GInt GInt GIVec3 GInt GIVec3
instance Arithmetic GInt GInt GIVec4 GInt GIVec4
instance Arithmetic GInt GInt GInt GIVec2 GIVec2
instance Arithmetic GInt GInt GInt GIVec3 GIVec3
instance Arithmetic GInt GInt GInt GIVec4 GIVec4

-- | Types that can be multiplied.
class (Base a aBase, Base b bBase) =>
      Mul aBase bBase a b result | a b -> result
                                 , b -> aBase bBase
                                 , a -> aBase bBase
                                 , result -> aBase bBase
instance Mul GFloat GFloat GMat2 GVec2 GVec2
instance Mul GFloat GFloat GMat3 GVec3 GVec3
instance Mul GFloat GFloat GMat4 GVec4 GVec4
instance Mul GFloat GFloat GVec2 GMat2 GVec2
instance Mul GFloat GFloat GVec3 GMat3 GVec3
instance Mul GFloat GFloat GVec4 GMat4 GVec4
instance {-# OVERLAPPABLE #-} 
         ( Arithmetic aBase bBase a b result
         , Base a aBase, Base b bBase) =>
         Mul aBase bBase a b result

class (ShaderType a, Base a GFloat) => GFloatVec a
instance GFloatVec GVec2
instance GFloatVec GVec3
instance GFloatVec GVec4

-- | GFloats or float vectors.
class ShaderType a => GenType a
instance {-# OVERLAPS #-} GenType GFloat
instance {-# OVERLAPPABLE #-} (GFloatVec a, ShaderType a) => GenType a

type family GenTypeGFloatConstr a b where
        GenTypeGFloatConstr a GFloat = GenType a
        GenTypeGFloatConstr a a = GenType a

-- | __@a@__ must be a 'GenType', while __@b@__ can either be the same as
-- __@a@__, or a 'GFloat'.
type GenTypeGFloat a b = (GenTypeGFloatConstr a b, ShaderType a, ShaderType b)

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
(&&) :: GBool -> GBool -> GBool
(&&) = op2 "&&"

infixr 2 ||
(||) :: GBool -> GBool -> GBool
(||) = op2 "||"

infix 4 ==
(==) :: ShaderType a => a -> a -> GBool
(==) = op2 "=="

infix 4 /=
(/=) :: ShaderType a => a -> a -> GBool
(/=) = op2 "!="

infix 4 >=
(>=) :: ShaderType a => a -> a -> GBool
(>=) = op2 ">="

infix 4 <=
(<=) :: ShaderType a => a -> a -> GBool
(<=) = op2 "<="

infix 4 <
(<) :: ShaderType a => a -> a -> GBool
(<) = op2 "<"

infix 4 >
(>) :: ShaderType a => a -> a -> GBool
(>) = op2 ">"

class ShaderType a => VecOrd a
instance VecOrd GVec2
instance VecOrd GVec3
instance VecOrd GVec4
instance VecOrd GIVec2
instance VecOrd GIVec3
instance VecOrd GIVec4

class ShaderType a => VecEq a
instance VecEq GVec2
instance VecEq GVec3
instance VecEq GVec4
instance VecEq GIVec2
instance VecEq GIVec3
instance VecEq GIVec4
instance VecEq GBVec2
instance VecEq GBVec3
instance VecEq GBVec4

lessThan :: VecOrd a => a -> a -> GBool
lessThan = fun2 "lessThan"

lessThanEqual :: VecOrd a => a -> a -> GBool
lessThanEqual = fun2 "lessThanEqual"

greaterThan :: VecOrd a => a -> a -> GBool
greaterThan = fun2 "greaterThan"

greaterThanEqual :: VecOrd a => a -> a -> GBool
greaterThanEqual = fun2 "greaterThanEqual"

equal :: VecEq a => a -> a -> GBool
equal = fun2 "equal"

notEqual :: VecEq a => a -> a -> GBool
notEqual = fun2 "notEqual"

class ShaderType a => GBoolVector a
instance GBoolVector GBVec2
instance GBoolVector GBVec3
instance GBoolVector GBVec4

anyBV :: GBoolVector a => a -> GBool
anyBV = fun1 "any"

allBV :: GBoolVector a => a -> GBool
allBV = fun1 "all"

notBV :: GBoolVector a => a -> GBool
notBV = fun1 "not"

negate :: GenType a => a -> a
negate = op1 "-"

negateM :: GMatrix a => a -> a
negateM = op1 "-"

negateI :: GInt -> GInt
negateI = op1 "-"

not :: GBool -> GBool
not = op1 "!"

class (ShaderType a, Base a a) => Num a where
        fromInteger :: Integer -> a

instance Num GFloat where
        fromInteger = fromRational . Prelude.fromInteger

instance Num GInt where
        fromInteger = GInt . Literal
                           . (printf "%d" :: Integer -> String)
                           . Prelude.fromInteger

fromRational :: Prelude.Rational -> GFloat
fromRational = GFloat . Literal
                      . (printf "%f" :: Float -> String)
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

-- XXX: ???
absI :: GInt -> GInt
absI = fun1 "abs"

sign :: GenType a => a -> a
sign = fun1 "sign"

signI :: GInt -> GInt
signI = fun1 "sign"

floor :: GenType a => a -> a
floor = fun1 "floor"

ceil :: GenType a => a -> a
ceil = fun1 "ceil"

fract :: GenType a => a -> a
fract = fun1 "fract"

mod :: GenTypeGFloat a b => a -> b -> a
mod = fun2 "mod"

min :: GenTypeGFloat a b => a -> b -> a
min = fun2 "min"

max :: GenTypeGFloat a b => a -> b -> a
max = fun2 "max"

clamp :: GenTypeGFloat a b => a -> b -> b -> a
clamp = fun3 "clamp"

-- | Linear interpolation between two values.
--
-- @mix x y t = x*(1-t) + y*t@
mix :: GenTypeGFloat a b => a -> a -> b -> a
mix = fun3 "mix"

-- | @step e x@ returns 0 if x < e, 1 otherwise.
step :: GenTypeGFloat a b => b -> a -> a
step = fun2 "step"

{-
smoothstep :: GenTypeGFloat a b => b -> b -> a -> a
smoothstep = fun3 "smoothstep"
-}

length :: GenType a => a -> GFloat
length = fun1 "length"

arrayLength :: (ShaderType t, KnownNat n) => GArray n t -> GInt
arrayLength = fun1 "length"

-- | Access an array element at a given index.
(!) :: (ShaderType t, KnownNat n) => GArray n t -> GInt -> t
arr ! i = fromExpr $ ArrayIndex (toExpr arr) (toExpr i)

distance :: GenType a => a -> a -> GFloat
distance = fun2 "distance"

dot :: GenType a => a -> a -> GFloat
dot = fun2 "dot"

cross :: GVec3 -> GVec3 -> GVec3
cross = fun2 "cross"

normalize :: GenType a => a -> a
normalize = fun1 "normalize"

faceforward :: GenType a => a -> a -> a -> a
faceforward = fun3 "faceforward"

reflect :: GenType a => a -> a -> a
reflect = fun2 "reflect"

refract :: GenType a => a -> a -> GFloat -> a
refract = fun3 "refract"

class ShaderType a => GMatrix a
instance GMatrix GMat2
instance GMatrix GMat3
instance GMatrix GMat4

-- | Component-wise multiplication of matrices.
matrixCompMult :: GMatrix a => a -> a -> a
matrixCompMult = fun2 "matrixCompMult"

-- | Avoid evaluating the expression of the argument more than one time.
-- Conditionals and loops imply it.
store :: ShaderType a => a -> a
store x = fromExpr . Action $ Store (typeName x) (toExpr x)

true :: GBool
true = GBool $ Literal "true"

false :: GBool
false = GBool $ Literal "false"

-- | Rebound if. You don't need to use this function, with -XRebindableSyntax.
ifThenElse :: ShaderType a => GBool -> a -> a -> a
ifThenElse b t f = fromExpr . Action $ If (toExpr b) (typeName t)
                                          (toExpr t) (toExpr f)

-- | This function implements raw GLSL loops. The same effect can be achieved
-- using Haskell list functions, but that may result in a large compiled GLSL
-- source, which in turn causes an out of memory error.
loop :: ShaderType a 
     => Int -- ^ Maximum number of iterations (should be as low as possible)
     -> a -- ^ Initial value
     -> (GInt -> a -> (a, GBool)) -- ^ Iteration -> Old value -> (Next, Stop)
     -> a
loop iters iv f =
        fromExpr . Action $
                For iters
                    (typeName iv)
                    (toExpr iv)
                    (\ie ve -> let (next, stop) = f (fromExpr ie) (fromExpr ve)
                               in (toExpr next, toExpr stop))

-- | Texture lookup function.
texture2D :: GSampler2D -> GVec2 -> GVec4
texture2D = fun2 "texture2D"

{-
texture2DBias :: GSampler2D -> GVec2 -> GFloat -> GVec4
texture2DBias = fun3 "texture2DBias"

texture2DProj :: GSampler2D -> GVec3 -> GVec4
texture2DProj = fun2 "texture2DProj"

texture2DProjBias :: GSampler2D -> GVec3 -> GFloat -> GVec4
texture2DProjBias = fun3 "texture2DProj"

texture2DProj4 :: GSampler2D -> GVec4 -> GVec4
texture2DProj4 = fun2 "texture2DProj"

texture2DProjBias4 :: GSampler2D -> GVec4 -> GFloat -> GVec4
texture2DProjBias4 = fun3 "texture2DProj"

texture2DLod :: GSampler2D -> GVec2 -> GFloat -> GVec4
texture2DLod = fun3 "texture2DLod"

texture2DProjLod :: GSampler2D -> GVec3 -> GFloat -> GVec4
texture2DProjLod = fun3 "texture2DProjLod"

texture2DProjLod4 :: GSampler2D -> GVec4 -> GFloat -> GVec4
texture2DProjLod4 = fun3 "texture3DProjLod"

textureCube :: GSamplerCube -> GVec3 -> GVec4
textureCube = fun2 "textureCube"

textureCubeBias :: GSamplerCube -> GVec3 -> GFloat -> GVec4
textureCubeBias = fun3 "textureCube"

textureCubeLod :: GSamplerCube -> GVec3 -> GFloat -> GVec4
textureCubeLod = fun3 "textureCubeLod"
-}

-- | Partial derivative of the argument with respect to the window X coordinate.
dFdx :: GenType a => a -> a
dFdx = fun1 "dFdx"

-- | Partial derivative of the argument with respect to the window Y coordinate.
dFdy :: GenType a => a -> a
dFdy = fun1 "dFdy"

-- | Sum of the absolute values of 'dFdx' and 'dFdy'.
fwidth :: GenType a => a -> a
fwidth = fun1 "fwidth"

-- | The position of the vertex (only works in the vertex shader).
position :: GVec4
position = fromExpr $ Read "gl_Position"

-- | The data of the fragment (only works in the fragment shader).
fragData :: GArray 16 GVec4
fragData = fromExpr $ Read "gl_FragData"

-- | The coordinates of the fragment (only works in the fragment shader).
fragCoord :: GVec4
fragCoord = fromExpr $ Read "gl_FragCoord"

-- | If the fragment belongs to a front-facing primitive (only works in the
-- fragment shader).
fragFrontFacing :: GBool
fragFrontFacing = fromExpr $ Read "gl_FrontFacing"

class ShaderType t => ToGInt t
instance ToGInt GFloat
instance ToGInt GBool
instance ToGInt GInt

int :: ToGInt t => t -> GInt
int = fun1 "int"

class ShaderType t => ToGBool t
instance ToGBool GFloat
instance ToGBool GBool
instance ToGBool GInt

bool :: ToGBool t => t -> GBool
bool = fun1 "bool"

class ShaderType t => ToGFloat t
instance ToGFloat GFloat
instance ToGFloat GBool
instance ToGFloat GInt

float :: ToGFloat t => t -> GFloat
float = fun1 "float"

class ToGVec2 t where
        vec2 :: t -> GVec2

instance {-# OVERLAPPING #-} ToGVec2 GFloat where
        vec2 = fun1 "vec2"

instance {-# OVERLAPPABLE #-}
         (Components GVec2 <= n, ToCompList t n) => ToGVec2 t where
        vec2 = funCompList "vec2"

class ToGVec3 t where
        vec3 :: t -> GVec3

instance {-# OVERLAPPING #-} ToGVec3 GFloat where
        vec3 = fun1 "vec3"

instance {-# OVERLAPPABLE #-}
         (Components GVec3 <= n, ToCompList t n) => ToGVec3 t where
        vec3 = funCompList "vec3"

class ToGVec4 t where
        vec4 :: t -> GVec4

instance {-# OVERLAPPING #-} ToGVec4 GFloat where
        vec4 = fun1 "vec4"

instance {-# OVERLAPPABLE #-}
         (Components GVec4 <= n, ToCompList t n) => ToGVec4 t where
        vec4 = funCompList "vec4"

class ToGIVec2 t where
        ivec2 :: t -> GIVec2

instance {-# OVERLAPPING #-} ToGIVec2 GFloat where
        ivec2 = fun1 "ivec2"

instance {-# OVERLAPPABLE #-}
         (Components GIVec2 <= n, ToCompList t n) => ToGIVec2 t where
        ivec2 = funCompList "ivec2"

class ToGIVec3 t where
        ivec3 :: t -> GIVec3

instance {-# OVERLAPPING #-} ToGIVec3 GFloat where
        ivec3 = fun1 "ivec3"

instance {-# OVERLAPPABLE #-}
         (Components GIVec3 <= n, ToCompList t n) => ToGIVec3 t where
        ivec3 = funCompList "ivec3"

class ToGIVec4 t where
        ivec4 :: t -> GIVec4

instance {-# OVERLAPPING #-} ToGIVec4 GFloat where
        ivec4 = fun1 "ivec4"

instance {-# OVERLAPPABLE #-}
         (Components GIVec4 <= n, ToCompList t n) => ToGIVec4 t where
        ivec4 = funCompList "ivec4"

class ToGBVec2 t where
        bvec2 :: t -> GBVec2

instance {-# OVERLAPPING #-} ToGBVec2 GFloat where
        bvec2 = fun1 "bvec2"

instance {-# OVERLAPPABLE #-}
         (Components GBVec2 <= n, ToCompList t n) => ToGBVec2 t where
        bvec2 = funCompList "bvec2"

class ToGBVec3 t where
        bvec3 :: t -> GBVec3

instance {-# OVERLAPPING #-} ToGBVec3 GFloat where
        bvec3 = fun1 "bvec3"

instance {-# OVERLAPPABLE #-}
         (Components GBVec3 <= n, ToCompList t n) => ToGBVec3 t where
        bvec3 = funCompList "bvec3"

class ToGBVec4 t where
        bvec4 :: t -> GBVec4

instance {-# OVERLAPPING #-} ToGBVec4 GFloat where
        bvec4 = fun1 "bvec4"

instance {-# OVERLAPPABLE #-}
         (Components GBVec4 <= n, ToCompList t n) => ToGBVec4 t where
        bvec4 = funCompList "bvec4"

class ToGMat2 t where
        mat2 :: t -> GMat2

instance {-# OVERLAPPING #-} ToGMat2 GFloat where
        mat2 = fun1 "mat2"

instance {-# OVERLAPPABLE #-}
         (Components GMat2 <= n, ToCompList t n) => ToGMat2 t where
        mat2 = funCompList "mat2"

class ToGMat3 t where
        mat3 :: t -> GMat3

instance {-# OVERLAPPING #-} ToGMat3 GFloat where
        mat3 = fun1 "mat3"

instance {-# OVERLAPPABLE #-}
         (Components GMat3 <= n, ToCompList t n) => ToGMat3 t where
        mat3 = funCompList "mat3"

class ToGMat4 t where
        mat4 :: t -> GMat4

instance {-# OVERLAPPING #-} ToGMat4 GFloat where
        mat4 = fun1 "mat4"

instance {-# OVERLAPPABLE #-}
         (Components GMat4 <= n, ToCompList t n) => ToGMat4 t where
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
-- > mat2 $ GVec2 2 4 # GVec2 1 3
-- > vec4 $ mat2 (0 # 1 # vec2 2) # 9  -- 9 is discarded
-- > mat4 $ 5 # vec2 5 # GVec3 1 2 3 # GMat2 (vec2 0) (GVec2 1 2) # mat3 0
-- > vec4 $ 1 # vec2 0 -- Not enough components, fails with "Couldn't match type
-- >                   -- ‘'Prelude.False’ with 'Prelude.True’" (because
-- >                   -- Components GVec4 <=? 3 ~ False).
(#) :: (ToCompList x xn, ToCompList y yn) => x -> y -> CompList (xn + yn)
x # y = CLAppend (toCompList x) (toCompList y)

infixr 5 #

type family Components (t :: *) :: Nat where
        Components GInt = 1
        Components GFloat = 1
        Components GBool = 1
        Components GVec2 = 2
        Components GIVec2 = 2
        Components GBVec2 = 2
        Components GVec3 = 3
        Components GIVec3 = 3
        Components GBVec3 = 3
        Components GVec4 = 4
        Components GIVec4 = 4
        Components GBVec4 = 4
        Components GMat2 = 4
        Components GMat3 = 9
        Components GMat4 = 16
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
