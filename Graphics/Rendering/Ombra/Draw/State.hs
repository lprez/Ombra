{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}

module Graphics.Rendering.Ombra.Draw.State where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Lazy as H
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Semigroup ((<>))
import Graphics.Rendering.Ombra.Backend (GLES, GLEnum, GLInt, GLUInt)
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
import Graphics.Rendering.Ombra.Vector (Vec4)

data Target bo o where
        BufferTarget :: BufferPair bo -> Target bo o
        DefaultTarget :: Target o o
        NoTarget :: (forall bo o. Target bo o -> Ordering) -> Target bo o

data DrawChange where
        ChangeTarget :: Maybe Int -> DrawChange
        ClearBuffers :: (Bool, Bool, Bool) -> DrawChange
        ChangeShaders :: Maybe Int -> DrawChange
        ChangeTextures :: HashSet Texture -> HashSet Texture -> DrawChange
        ChangeBlendEnabled :: Bool -> DrawChange
        ChangeBlendConstantColor :: Maybe Vec4 -> DrawChange
        ChangeBlendEquation :: (GLEnum, GLEnum) -> DrawChange
        ChangeBlendFunction :: (GLEnum, GLEnum, GLEnum, GLEnum) -> DrawChange
        ChangeStencilEnabled :: Bool -> DrawChange
        ChangeStencilFunction :: Stencil.Side (GLEnum, GLInt, GLUInt)
                              -> DrawChange
        ChangeStencilOperation :: Stencil.Side (GLEnum, GLEnum, GLEnum)
                               -> DrawChange
        ChangeCullEnabled :: Bool -> DrawChange
        ChangeCullFace :: CullFace -> DrawChange
        ChangeDepthTest :: Bool -> DrawChange
        ChangeDepthMask :: Bool -> DrawChange
        ChangeColorMask :: (Bool, Bool, Bool, Bool) -> DrawChange
        ChangeGeometry :: Maybe Int -> DrawChange
        ChangeUniforms :: HashMap UniformID UniformValue -> DrawChange
        deriving (Eq, Ord)

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

instance GLES => Ord (DrawState o) where
        compare s s' =    compareStateTarget s s'
                       <> compare (stateHashShaders s) (stateHashShaders s')
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
                       <> compare (hash $ stateTextures s)
                                  (hash $ stateTextures s')
                       <> compareStateGeometry s s'
                       <> compare (map hash . H.elems $ stateUniforms s)
                                  (map hash . H.elems $ stateUniforms s')

rootChanges :: GLES => DrawState o -> NonEmpty DrawChange
rootChanges s@(DrawState { stateTarget = target, stateGeometry = geometry }) =
           rootTarget target
        :| [ ChangeShaders . stateHashShaders $ s
           , ChangeTextures (stateTextures $ s) HS.empty
           , ChangeBlendEnabled . stateBlendEnabled $ s
           , ChangeBlendConstantColor . Blend.constantColor $ blendMode
           , ChangeBlendEquation . Blend.equation $ blendMode
           , ChangeBlendFunction . Blend.function $ blendMode
           , ChangeStencilEnabled . stateStencilEnabled $ s
           , ChangeStencilFunction . fmap Stencil.function $
                   Stencil.sideFunction stencilMode
           , ChangeStencilOperation . fmap Stencil.operation $
                   Stencil.sideOperation stencilMode
           , ChangeCullEnabled . stateCullEnabled $ s
           , ChangeCullFace . stateCullFace $ s
           , ChangeDepthTest . stateDepthTest $ s
           , ChangeDepthMask . stateDepthMask $ s
           , ChangeColorMask . stateColorMask $ s
           , ChangeGeometry . fmap hash $ geometry
           ]
        where blendMode = stateBlendMode s
              stencilMode = stateStencilMode s

diffChange :: GLES => DrawChange -> DrawChange -> Maybe DrawChange
diffChange (ChangeTextures s _) (ChangeTextures s' _) =
        Just $ changedTextures s s'
diffChange c c' = if c == c' then Nothing else Just c'

-- | Minimize the number of changes.
diffState :: GLES
          => Bool              -- ^ Filter every type of change.
          -> DrawState o
          -> DrawState o
          -> DrawChange
          -> [DrawChange]
diffState _ s s' (ChangeTextures _ _) = [changedTextures (stateTextures s)
                                                         (stateTextures s')
                                        ]
diffState _ s s' (ChangeUniforms _) = [changedUniforms s s']
diffState False _ _ c = [c]
diffState True s s' (ChangeTarget _) | sameStateTarget s s' = []
diffState True s s' (ChangeShaders _) | stateHashShaders s ==
                                        stateHashShaders s' = []
diffState True s s' (ChangeBlendEnabled _) | stateBlendEnabled s ==
                                             stateBlendEnabled s' = []
diffState True s s' (ChangeBlendConstantColor _)
        | Blend.constantColor (stateBlendMode s) ==
          Blend.constantColor (stateBlendMode s') = []
diffState True s s' (ChangeBlendEquation _)
        | Blend.equation (stateBlendMode s) ==
          Blend.equation (stateBlendMode s') = []
diffState True s s' (ChangeBlendFunction _)
        | Blend.function (stateBlendMode s) ==
          Blend.function (stateBlendMode s') = []
diffState True s s' (ChangeStencilEnabled _) | stateStencilEnabled s ==
                                               stateStencilEnabled s' = []
diffState True s s' (ChangeStencilFunction _)
        | Stencil.sideFunction (stateStencilMode s) ==
          Stencil.sideFunction (stateStencilMode s') = []
diffState True s s' (ChangeStencilOperation _)
        | Stencil.sideOperation (stateStencilMode s) ==
          Stencil.sideOperation (stateStencilMode s') = []
diffState True s s' (ChangeCullEnabled _) | stateCullEnabled s ==
                                            stateCullEnabled s' = []
diffState True s s' (ChangeCullFace _) | stateCullFace s ==
                                         stateCullFace s' = []
diffState True s s' (ChangeDepthTest _) | stateDepthTest s ==
                                          stateDepthTest s' = []
diffState True s s' (ChangeDepthMask _) | stateDepthMask s ==
                                          stateDepthMask s' = []
diffState True s s' (ChangeColorMask _) | stateColorMask s ==
                                          stateColorMask s' = []
diffState True s s' (ChangeGeometry _) | sameStateGeometry s s' = []
diffState True _ _ c = [c]

rootTarget :: GLES => Target bo o -> DrawChange
rootTarget (BufferTarget (BufferPair g d)) =
        ChangeTarget . Just $ hash (textures g, textures d)
rootTarget _ = ChangeTarget Nothing

changedUniforms :: GLES => DrawState o -> DrawState o -> DrawChange
changedUniforms s s' | stateHashShaders s /= stateHashShaders s' =
        ChangeUniforms $ stateUniforms s'
changedUniforms s s' =
        ChangeUniforms $ H.differenceWith (\u' u -> if u == u'
                                                    then Nothing
                                                    else Just u'
                                          )
                                          (stateUniforms s')
                                          (stateUniforms s)

changedTextures :: GLES => HashSet Texture -> HashSet Texture -> DrawChange
changedTextures s s' = ChangeTextures (HS.difference s' s) (HS.difference s s')

sameStateTarget :: GLES => DrawState o -> DrawState o -> Bool
sameStateTarget s s' = compareStateTarget s s' == EQ

compareStateTarget :: GLES => DrawState o -> DrawState o -> Ordering
compareStateTarget (DrawState {stateTarget = t})
                   (DrawState {stateTarget = t'}) = compareTarget t t'

-- | TODO: generate ID from createBuffers and use it for comparison
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

initialState :: (GLES, FragmentShaderOutput o) => DrawState o
initialState = DrawState { stateVertexShader =
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
