{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}

module Graphics.Rendering.Ombra.Draw.State where

-- import Data.Ord
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Lazy as H
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Semigroup ((<>))
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Culling
import Graphics.Rendering.Ombra.Blend (transparency)
import qualified Graphics.Rendering.Ombra.Blend.Draw as Blend
import qualified Graphics.Rendering.Ombra.Blend.Types as Blend
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Geometry.Types (geometryHash)
import Graphics.Rendering.Ombra.OutBuffer
import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Shader.Language.Types (GVec4)
import qualified Graphics.Rendering.Ombra.Stencil.Draw as Stencil
import qualified Graphics.Rendering.Ombra.Stencil.Types as Stencil
import Graphics.Rendering.Ombra.Texture
import Graphics.Rendering.Ombra.Internal.SM

data Target bo o where
        BufferTarget :: BufferPair bo -> Target bo o
        DefaultTarget :: Target o o
        NoTarget :: (forall bo o. Target bo o -> Ordering) -> Target bo o

data DrawState o where
        DrawState :: ( GeometryVertex g
                     , ShaderInput g
                     , ElementType e
                     , ShaderInput v
                     , FragmentShaderOutput o'
                     )
                  => { stateVertexShader :: Maybe (VertexShader g (GVec4, v))
                     , stateFragmentShader :: Maybe (FragmentShader v o')
                     , stateGeometry :: Maybe (Geometry e g)
                     , stateUniforms :: HashMap UniformID UniformValue
                     , stateTextures :: HashSet Texture
                     , stateTarget :: Target o' o
                     , stateBlendEnabled :: Bool
                     , stateBlendMode :: Blend.Mode
                     , stateStencilEnabled :: Bool
                     , stateStencilMode :: Stencil.Mode
                     , stateCullEnabled :: Bool
                     , stateCullFace :: CullFace
                     , stateDepthTest :: Bool
                     , stateDepthMask :: Bool
                     , stateColorMask :: (Bool, Bool, Bool, Bool)
                     , stateHashShaders :: Maybe Int
                     }
                  -> DrawState o

instance GLES => Eq (DrawState o) where
        s == s' = compare s s' == EQ

-- TODO: hashare tutto e implementare eq e ord sulla base dell'hash

instance GLES => Ord (DrawState o) where
        compare s s' =    compareStateGeometry s s'
                        -- si potrebbe aggiungere un intero agli stati che
                        -- normalmente è 0 ma viene incrementato nelle unioni
                        -- uguali
                        -- potrebbe anche risolvere il problema degli uniform
                        -- esterni
                        -- XXX
                       <> compare (map hash . H.elems $ stateUniforms s)
                                  (map hash . H.elems $ stateUniforms s')
                       <> compare (hash $ stateTextures s)
                                  (hash $ stateTextures s')
                       <> compareStateTarget s s'
                       <> compare (stateBlendEnabled s) (stateBlendEnabled s')
                       <> compare (stateBlendMode s) (stateBlendMode s')
                       <> compare (stateStencilEnabled s)
                                  (stateStencilEnabled s')
                       <> compare (stateStencilMode s) (stateStencilMode s')
                       <> compare (stateCullEnabled s) (stateCullEnabled s')
                       <> compare (stateCullFace s) (stateCullFace s')
                       <> compare (stateDepthTest s) (stateDepthTest s')
                       <> compare (stateDepthMask s) (stateDepthMask s')
                       <> compare (stateColorMask s) (stateColorMask s')
                       <> compare (stateHashShaders s) (stateHashShaders s')

data DrawDiff = DrawDiff { changedShader :: Bool
                         , changedGeometry :: Bool
                         , changedTarget :: Bool
                         , changedUniforms :: HashMap UniformID UniformValue
                         , addedTextures :: HashSet Texture
                         , removedTextures :: HashSet Texture
                         , changedBlendEnabled :: Bool
                         , changedBlendConstantColor :: Bool
                         , changedBlendEquation :: Bool
                         , changedBlendFunction :: Bool
                         , changedStencilEnabled :: Bool
                         , changedStencilFunction :: Bool
                         , changedStencilOperation :: Bool
                         , changedCullEnabled :: Bool
                         , changedCullFace :: Bool
                         , changedDepthTest :: Bool
                         , changedDepthMask :: Bool
                         , changedColorMask :: Bool
                         }

diff :: GLES => DrawState o -> DrawState o -> DrawDiff
diff s s' = DrawDiff { changedShader = stateHashShaders s /= stateHashShaders s'
                     , changedGeometry = not $ sameStateGeometry s s'
                     , changedTarget = not $ sameStateTarget s s'
                     , changedUniforms =
                             if stateHashShaders s /= stateHashShaders s'
                             then stateUniforms s'
                             else H.differenceWith (\u' u -> if u == u'
                                                             then Nothing
                                                             else Just u'
                                                   )
                                                   (stateUniforms s')
                                                   (stateUniforms s)
                     , addedTextures =
                             HS.difference (stateTextures s') (stateTextures s)
                     , removedTextures =
                             HS.difference (stateTextures s) (stateTextures s')
                     , changedBlendEnabled =
                             stateBlendEnabled s /= stateBlendEnabled s'
                     , changedBlendConstantColor =
                                Blend.constantColor (stateBlendMode s)
                             /= Blend.constantColor (stateBlendMode s')
                     , changedBlendEquation =
                                Blend.equation (stateBlendMode s)
                             /= Blend.equation (stateBlendMode s')
                     , changedBlendFunction =
                                Blend.function (stateBlendMode s)
                             /= Blend.function (stateBlendMode s')
                     , changedStencilEnabled =
                             stateStencilEnabled s /= stateStencilEnabled s'
                     , changedStencilFunction =
                                Stencil.sideFunction (stateStencilMode s)
                             /= Stencil.sideFunction (stateStencilMode s')
                     , changedStencilOperation =
                                Stencil.sideOperation (stateStencilMode s)
                             /= Stencil.sideOperation (stateStencilMode s')
                     , changedCullEnabled =
                             stateCullEnabled s /= stateCullEnabled s'
                     , changedCullFace =
                             stateCullFace s /= stateCullFace s'
                     , changedDepthTest =
                             stateDepthTest s /= stateDepthTest s'
                     , changedDepthMask =
                             stateDepthMask s /= stateDepthMask s'
                     , changedColorMask =
                             stateColorMask s /= stateColorMask s'
                     }

sameStateTarget :: GLES => DrawState o -> DrawState o -> Bool
sameStateTarget s s' = compareStateTarget s s' == EQ

compareStateTarget :: GLES => DrawState o -> DrawState o -> Ordering
compareStateTarget (DrawState {stateTarget = t})
                   (DrawState {stateTarget = t'}) = compareTarget t t'

compareTarget :: GLES => Target bo o -> Target bo' o' -> Ordering
compareTarget DefaultTarget DefaultTarget = EQ
compareTarget DefaultTarget _ = LT
compareTarget _ DefaultTarget = GT
compareTarget (NoTarget f) t = f t
compareTarget t (NoTarget f) = case f t of
                                    LT -> GT
                                    GT -> LT
                                    EQ -> EQ
compareTarget (BufferTarget (BufferPair g d)) (BufferTarget (BufferPair g' d')) =
        compare (textures g) (textures g') <> compare (textures d) (textures d')

sameStateGeometry :: DrawState o -> DrawState o -> Bool
sameStateGeometry s s' = compareStateGeometry s s' == EQ

compareStateGeometry :: DrawState o -> DrawState o -> Ordering
compareStateGeometry (DrawState {stateGeometry = Nothing})
                     (DrawState {stateGeometry = Nothing}) = EQ
compareStateGeometry (DrawState {stateGeometry = Nothing}) _ = LT
compareStateGeometry (DrawState {stateGeometry = Just g})
                     (DrawState {stateGeometry = Just g'}) =
             compare (geometryHash g) (geometryHash g')
compareStateGeometry (DrawState {stateGeometry = Just _}) _ = GT

instance (GLES, FragmentShaderOutput o) => State (DrawState o) where
        type Cost (DrawState o) = Int
        cost s s' = sum [ changeCost changedShader 100
                        , changeCost changedGeometry 5
                        , changeCost changedTarget 500
                        , changeCost changedBlendEnabled 10
                        , changeCost changedBlendConstantColor 10
                        , changeCost changedBlendEquation 10
                        , changeCost changedBlendFunction 10
                        , changeCost changedStencilEnabled 10
                        , changeCost changedStencilFunction 10
                        , changeCost changedStencilOperation 10
                        , changeCost changedCullEnabled 10
                        , changeCost changedCullFace 10
                        , changeCost changedDepthTest 10
                        , changeCost changedDepthMask 10
                        , changeCost changedColorMask 10
                        , HS.size (addedTextures dif) * 10
                        , HS.size (removedTextures dif) * 10
                        , H.size (changedUniforms dif) * 1
                        ]
                where dif = diff s s'
                      changeCost f n = if f dif then n else 0
        initial = DrawState { stateVertexShader =
                                Nothing :: Maybe (VertexShader GVec4 (GVec4, ()))
                            , stateFragmentShader = Nothing
                            , stateGeometry =
                                Nothing :: Maybe (Geometry Triangle GVec4)
                            , stateUniforms = H.empty
                            , stateTextures = HS.empty
                            , stateTarget = DefaultTarget
                            , stateBlendEnabled = False
                            , stateBlendMode = transparency
                            , stateStencilEnabled = False
                            , stateStencilMode = initialStencilMode
                            , stateCullEnabled = False
                            , stateCullFace = CullBack
                            , stateDepthTest = True
                            , stateDepthMask = True
                            , stateColorMask = (True, True, True, True)
                            , stateHashShaders = Nothing
                            }
        symmetric _ = True
        short _ c n = c < (quot n 10) ^ 3 + 1

mkState :: ( GeometryVertex g
           , ShaderInput g
           , ElementType e
           , ShaderInput v
           , FragmentShaderOutput o'
           , GLES
           )
        => VertexShader g (GVec4, v)
        -> FragmentShader v o'
        -> Geometry e g
        -> Target o' o
        -> Maybe (Blend.Mode)
        -> Maybe (Stencil.Mode)
        -> Maybe CullFace
        -> Bool
        -> Bool
        -> (Bool, Bool, Bool, Bool)
        -> DrawState o
mkState vs fs geom buffers blend stencil cull depthTest depthMask colorMask =
        DrawState { stateVertexShader = Just vs
                  , stateFragmentShader = Just fs
                  , stateGeometry = Just geom
                  , stateUniforms = H.fromList $ unisv ++ unisf
                  , stateTextures = HS.fromList $ texsv ++ texsf
                  , stateTarget = buffers
                  , stateBlendEnabled = isJust blend
                  , stateBlendMode = fromMaybe transparency blend
                  , stateStencilEnabled = isJust stencil
                  , stateStencilMode = fromMaybe initialStencilMode stencil
                  , stateCullEnabled = isJust cull
                  , stateCullFace = fromMaybe CullBack cull
                  , stateDepthTest = depthTest
                  , stateDepthMask = depthMask
                  , stateColorMask = colorMask
                  , stateHashShaders = Just $ hash (vs, fs)
                  }
        where (uid, unisv, texsv) = uniformList vs 0
              (_, unisf, texsf) = uniformList fs uid

setTarget :: GLES
          => (forall bo. Target bo o')
          -> DrawState o
          -> DrawState o'
setTarget t' (DrawState vs fs g us ts t be bm se sm ce cf dt dm cm hs) =
        DrawState vs fs g us ts t' be bm se sm ce cf dt dm cm hs

noTarget :: GLES => Target bo o -> Target bo' o'
noTarget (NoTarget f) = NoTarget f
noTarget t = NoTarget $ compareTarget t

initialStencilMode :: Stencil.Mode
initialStencilMode =
        Stencil.Mode (Stencil.FrontBack (Stencil.Function Stencil.Always 0 255))
                     (Stencil.FrontBack (Stencil.Operation Stencil.Keep
                                                           Stencil.Keep
                                                           Stencil.Keep))

data ApDrawState f o = ApDrawState (DrawState o) (f (DrawState o))

instance GLES => Eq (ApDrawState f o) where
        (ApDrawState s _) == (ApDrawState s' _) = s == s'

instance GLES => Ord (ApDrawState f o) where
        compare (ApDrawState s _) (ApDrawState s' _) = compare s s'

instance (GLES, FragmentShaderOutput o) => State (ApDrawState f o) where
        type Cost (ApDrawState f o) = Int
        cost (ApDrawState s _) (ApDrawState s' _) = cost s s'
        initial = ApDrawState initial $ error "ApDrawState initial"
        symmetric (ApDrawState s _) = symmetric s
        short (ApDrawState s _, ApDrawState s' _) = short (s, s')
