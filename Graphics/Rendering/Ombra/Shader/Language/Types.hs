{-# LANGUAGE MultiParamTypeClasses, DataKinds, KindSignatures, TypeOperators,
             ScopedTypeVariables, DeriveGeneric, TypeFamilies, FlexibleInstances
#-}

module Graphics.Rendering.Ombra.Shader.Language.Types where

import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import Data.MemoTrie
import Data.Hashable

-- | An expression.
data Expr = Empty | Read String String | Op1 String String Expr
          | Op2 String String Expr Expr | Apply String String [Expr]
          | X Expr | Y Expr | Z Expr | W Expr
          | Literal String String | Action Action Int | Dummy Int
          | ArrayIndex String Expr Expr | ContextVar String Int ContextVarType
          | HashDummy Int | Uniform String Int | Input String Int
          | Attribute String Int | ArgDummy Int
          deriving (Eq, Generic)

subExprs :: Expr -> [Expr]
subExprs (Op1 _ _ e) = [e]
subExprs (Op2 _ _ e1 e2) = [e1, e2]
subExprs (Apply _ _ es) = es
subExprs (X e) = [e]
subExprs (Y e) = [e]
subExprs (Z e) = [e]
subExprs (W e) = [e]
subExprs (ArrayIndex _ e1 e2) = [e1, e2]
subExprs _ = []

replaceSubExprs :: Expr -> [Expr] -> Expr
replaceSubExprs (Op1 t f _) [e] = Op1 t f e
replaceSubExprs (Op2 t f _ _) [e1, e2] = Op2 t f e1 e2
replaceSubExprs (Apply t f _) es = Apply t f es
replaceSubExprs (X _) [e] = X e
replaceSubExprs (Y _) [e] = Y e
replaceSubExprs (Z _) [e] = Z e
replaceSubExprs (W _) [e] = W e
replaceSubExprs (ArrayIndex t _ _) [e1, e2] = ArrayIndex t e1 e2
replaceSubExprs x _ = x

exprType :: Expr -> Maybe String
exprType (Read t _) = Just t
exprType (Op1 t _ _) = Just t
exprType (Op2 t _ _ _) = Just t
exprType (Apply t _ _) = Just t
exprType (X e) = vecElemType e
exprType (Y e) = vecElemType e
exprType (Z e) = vecElemType e
exprType (W e) = vecElemType e
exprType (Literal t _) = Just t
exprType (Action a n) = Just $ actionType a n
exprType (ArrayIndex t _ _) = Just t
exprType (ContextVar t _ _) = Just t
exprType (Uniform t _) = Just t
exprType (Input t _) = Just t
exprType (Attribute t _) = Just t
exprType _ = Nothing

vecElemType :: Expr -> Maybe String
vecElemType e = case exprType e of
                     Just ('v' : 'e' : 'c' : _) -> Just "float"
                     Just ('i': 'v' : 'e' : 'c' : _) -> Just "int"
                     Just ('b': 'v' : 'e' : 'c' : _) -> Just "bool"
                     _ -> Nothing

actionType :: Action -> Int -> String
actionType (Store t _) _ = t
actionType (If _ t _ _) _ = t
actionType (For _ is _) n = fst $ is !! n

reconstructExprFun :: (a -> Int -> Expr)
                   -> ((Expr -> Expr) -> b -> b)
                   -> b
                   -> (a -> b)
reconstructExprFun (!) mapExprs b = \args -> mapExprs (iter args) b
        where iter args (ArgDummy n) = args ! n
              iter args expr =
                      replaceSubExprs expr . map (iter args) $ subExprs expr

reconstructForBody :: ([Expr], Expr) -> ForBody
reconstructForBody (es, e) = let ef = reconstructExprFun (!!) id e
                                 efs = reconstructExprFun (!!) map es
                             in ForBody $ \x y -> (efs (x : y), ef (x : y))

-- | Expressions that are transformed into statements.
data Action = Store String Expr | If Expr String Expr Expr
            | For Expr [(String, Expr)] ForBody
            deriving Generic

newtype ForBody = ForBody (Expr -> [Expr] -> ([Expr], Expr))

data ContextVarType = LoopIteration | LoopValue Int deriving (Eq, Generic)

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
        typeName (_ :: GArray n t) =
                typeName (zero :: t) ++
                "[" ++ show (natVal (Proxy :: Proxy n)) ++ "]"
        size (GArray _ :: GArray n t) =
                size (zero :: t) * fromInteger (natVal (Proxy :: Proxy n))

instance ShaderType GBool where
        zero = GBool $ Literal "bool" "false"

        toExpr (GBool e) = e

        fromExpr = GBool

        typeName _ = "bool"

        size _ = 1

instance ShaderType GInt where
        zero = GInt $ Literal "int" "0"

        toExpr (GInt e) = e

        fromExpr = GInt

        typeName _ = "int"

        size _ = 1

instance ShaderType GFloat where
        zero = GFloat $ Literal "float" "0.0"

        toExpr (GFloat e) = e

        fromExpr = GFloat

        typeName _ = "float"

        size _ = 1

instance ShaderType GSampler2D where
        zero = GSampler2D $ Literal "sampler2D" "0"

        toExpr (GSampler2D e) = e

        fromExpr = GSampler2D

        typeName _ = "sampler2D"

        size _ = 1

{-

instance ShaderType GSamplerCube where
        zero = GSamplerCube $ Literal "0"

        toExpr (GSamplerCube e) = e

        fromExpr = GSamplerCube

        typeName _ = "samplerCube"

        size _ = 1

-}

instance ShaderType GVec2 where
        zero = GVec2 zero zero

        toExpr (GVec2 (GFloat (X v)) (GFloat (Y v'))) | v == v' =
                case exprType v of
                     Just "vec2" -> v
                     _ -> Apply "vec2" "vec2" [v]

        toExpr (GVec2 (GFloat x) (GFloat y)) = Apply "vec2" "vec2" [x, y]

        fromExpr v = GVec2 (GFloat (X v)) (GFloat (Y v))

        typeName _ = "vec2"

        size _ = 1

instance ShaderType GVec3 where
        zero = GVec3 zero zero zero

        toExpr (GVec3 (GFloat (X v)) (GFloat (Y v')) (GFloat (Z v'')))
               | v == v' && v' == v'' =
                case exprType v of
                     Just "vec3" -> v
                     _ -> Apply "vec3" "vec3" [v]
        toExpr (GVec3 (GFloat x) (GFloat y) (GFloat z)) =
                Apply "vec3" "vec3" [x, y, z]

        fromExpr v = GVec3 (GFloat (X v)) (GFloat (Y v)) (GFloat (Z v))

        typeName _ = "vec3"

        size _ = 1

instance ShaderType GVec4 where
        zero = GVec4 zero zero zero zero

        toExpr (GVec4 (GFloat (X v))
                      (GFloat (Y v1))
                      (GFloat (Z v2))
                      (GFloat (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 =
                       case exprType v of
                            Just "vec4" -> v
                            _ -> Apply "vec4" "vec4" [v]
        toExpr (GVec4 (GFloat x) (GFloat y) (GFloat z) (GFloat w)) =
                Apply "vec4" "vec4" [x, y, z, w]

        fromExpr v = GVec4 (GFloat (X v)) (GFloat (Y v)) (GFloat (Z v)) (GFloat (W v))

        typeName _ = "vec4"

        size _ = 1

instance ShaderType GIVec2 where
        zero = GIVec2 zero zero

        toExpr (GIVec2 (GInt (X v)) (GInt (Y v'))) | v == v' =
                case exprType v of
                     Just "ivec2" -> v
                     _ -> Apply "ivec2" "ivec2" [v]
        toExpr (GIVec2 (GInt x) (GInt y)) = Apply "ivec2" "ivec2" [x, y]

        fromExpr v = GIVec2 (GInt (X v)) (GInt (Y v))

        typeName _ = "ivec2"

        size _ = 1

instance ShaderType GIVec3 where
        zero = GIVec3 zero zero zero

        toExpr (GIVec3 (GInt (X v)) (GInt (Y v')) (GInt (Z v'')))
               | v == v' && v' == v'' =
                case exprType v of
                     Just "ivec3" -> v
                     _ -> Apply "ivec3" "ivec3" [v]
        toExpr (GIVec3 (GInt x) (GInt y) (GInt z)) =
                Apply "ivec3" "ivec3" [x, y, z]

        fromExpr v = GIVec3 (GInt (X v)) (GInt (Y v)) (GInt (Z v))

        typeName _ = "ivec3"

        size _ = 1

instance ShaderType GIVec4 where
        zero = GIVec4 zero zero zero zero

        toExpr (GIVec4 (GInt (X v)) (GInt (Y v1)) (GInt (Z v2)) (GInt (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 =
                case exprType v of
                     Just "ivec4" -> v
                     _ -> Apply "ivec4" "ivec4" [v]
        toExpr (GIVec4 (GInt x) (GInt y) (GInt z) (GInt w)) =
                Apply "ivec4" "ivec4" [x, y, z, w]

        fromExpr v = GIVec4 (GInt (X v)) (GInt (Y v)) (GInt (Z v)) (GInt (W v))

        typeName _ = "ivec4"

        size _ = 1

instance ShaderType GBVec2 where
        zero = GBVec2 zero zero

        toExpr (GBVec2 (GBool (X v)) (GBool (Y v'))) | v == v' =
                case exprType v of
                     Just "bvec2" -> v
                     _ -> Apply "bvec2" "bvec2" [v]

        toExpr (GBVec2 (GBool x) (GBool y)) = Apply "bvec2" "bvec2" [x, y]

        fromExpr v = GBVec2 (GBool (X v)) (GBool (Y v))

        typeName _ = "bvec2"

        size _ = 1

instance ShaderType GBVec3 where
        zero = GBVec3 zero zero zero

        toExpr (GBVec3 (GBool (X v)) (GBool (Y v')) (GBool (Z v'')))
               | v == v' && v' == v'' =
                case exprType v of
                     Just "bvec3" -> v
                     _ -> Apply "bvec3" "bvec3" [v]
        toExpr (GBVec3 (GBool x) (GBool y) (GBool z)) =
                Apply "bvec3" "bvec3" [x, y, z]

        fromExpr v = GBVec3 (GBool (X v)) (GBool (Y v)) (GBool (Z v))

        typeName _ = "bvec3"

        size _ = 1

instance ShaderType GBVec4 where
        zero = GBVec4 zero zero zero zero

        toExpr (GBVec4 (GBool (X v))
                       (GBool (Y v1))
                       (GBool (Z v2))
                       (GBool (W v3)))
               | v == v1 && v1 == v2 && v2 == v3 =
                case exprType v of
                     Just "bvec4" -> v
                     _ -> Apply "bvec4" "bvec4" [v]
        toExpr (GBVec4 (GBool x) (GBool y) (GBool z) (GBool w)) =
                Apply "bvec4" "bvec4" [x, y, z, w]

        fromExpr v = GBVec4 (GBool (X v)) (GBool (Y v))
                            (GBool (Z v)) (GBool (W v))

        typeName _ = "bvec4"

        size _ = 1

instance ShaderType GMat2 where
        zero = GMat2 zero zero

        toExpr (GMat2 (GVec2 (GFloat (X (X m))) (GFloat (X (Y m1))))
                      (GVec2 (GFloat (Y (X m2))) (GFloat (Y (Y m3)))))
               | m == m1 && m1 == m2 && m2 == m3 =
                case exprType m of
                     Just "mat2" -> m
                     _ -> Apply "mat2" "mat2" [m]
        toExpr (GMat2 (GVec2 (GFloat xx) (GFloat xy))
                      (GVec2 (GFloat yx) (GFloat yy)))
               = Apply "mat2" "mat2" [xx, yx, xy, yy]

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
                         case exprType m of
                              Just "mat3" -> m
                              _ -> Apply "mat3" "mat3" [m]
        toExpr (GMat3 (GVec3 (GFloat xx) (GFloat xy) (GFloat xz))
                      (GVec3 (GFloat yx) (GFloat yy) (GFloat yz))
                      (GVec3 (GFloat zx) (GFloat zy) (GFloat zz)))
               = Apply "mat3" "mat3" [xx, yx, zx, xy, yy, zy, xz, yz, zz]

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
                 m12 == m13 && m13 == m14 && m14 == m15 =
                         case exprType m of
                              Just "mat4" -> m
                              _ -> Apply "mat4" "mat4" [m]
        toExpr (GMat4 (GVec4 (GFloat xx) (GFloat xy) (GFloat xz) (GFloat xw))
                      (GVec4 (GFloat yx) (GFloat yy) (GFloat yz) (GFloat yw))
                      (GVec4 (GFloat zx) (GFloat zy) (GFloat zz) (GFloat zw))
                      (GVec4 (GFloat wx) (GFloat wy) (GFloat wz) (GFloat ww)))
               = Apply "mat4" "mat4" [ xx, yx, zx, wx
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
                                Read _ str -> hash2 s 1 str
                                Op1 _ str exp -> hash2 s 2 (str, exp)
                                Op2 _ str exp exp' -> hash2 3 s (str, exp, exp')
                                Apply _ str exps -> hash2 4 s (str, exps)
                                X exp -> hash2 5 s exp
                                Y exp -> hash2 6 s exp
                                Z exp -> hash2 7 s exp
                                W exp -> hash2 8 s exp
                                Literal _ str -> hash2 s 9 str
                                Action actHash i -> hash2 s 10 (actHash, i)
                                Dummy i -> hash2 s 11 i
                                ContextVar _ i LoopIteration -> hash2 s 12 i
                                ContextVar _ i (LoopValue n) -> hash2 s 13 (i, n)
                                ArrayIndex _ arr i -> hash2 s 14 (arr, i)
                                Uniform _ i -> hash2 s 15 i
                                Input _ i -> hash2 s 16 i
                                Attribute _ i -> hash2 s 17 i
                                HashDummy h -> h -- TODO: hashWithSalt?
                                ArgDummy n -> hash2 s 18 n

instance HasTrie Action where
        newtype (Action :->: a) = ActionTrie { unActionTrie
                                                :: Reg Action :->: a }
        trie = trieGeneric ActionTrie
        untrie = untrieGeneric unActionTrie
        enumerate = enumerateGeneric unActionTrie

instance HasTrie ContextVarType where
        newtype (ContextVarType :->: a) =
                ContextVarTypeTrie { unContextVarTypeTrie
                                        :: Reg ContextVarType :->: a }
        trie = trieGeneric ContextVarTypeTrie
        untrie = untrieGeneric unContextVarTypeTrie
        enumerate = enumerateGeneric unContextVarTypeTrie

instance HasTrie ForBody where
        newtype (ForBody :->: b) = ForBodyTrie (([Expr], Expr) :->: b)
        trie f = ForBodyTrie . trie $ f . reconstructForBody 
        untrie (ForBodyTrie t) = untrie t . (\(ForBody f) -> applyArgs f)
                where applyArgs f = f (ArgDummy 0) [ArgDummy n | n <- [1 ..]]
        enumerate (ForBodyTrie t) =
                map (\(a, b) -> (reconstructForBody a, b)) $ enumerate t

instance HasTrie Expr where
        newtype (Expr :->: a) = ExprTrie { unExprTrie :: Reg Expr :->: a }
        trie = trieGeneric ExprTrie
        untrie = untrieGeneric unExprTrie
        enumerate = enumerateGeneric unExprTrie

instance Hashable Action where
        hashWithSalt s (Store t e) = hash2 s 0 (t, e)
        hashWithSalt s (If eb tt et ef) = hash2 s 1 (eb, tt, et, ef)
        hashWithSalt s (For iters ivs (ForBody eFun)) =
                let dummies n = zipWith (const Dummy) ivs [n ..]
                    baseHash = hash (iters, ivs, eFun (Dummy 0) (dummies 1))
                in hash2 s 2 ( baseHash
                             , eFun (Dummy baseHash) (dummies $ baseHash + 1))

instance Eq Action where
        a == a' = hash a == hash a'

hash2 :: Hashable a => Int -> Int -> a -> Int
hash2 s i x = s `hashWithSalt` i `hashWithSalt` x
