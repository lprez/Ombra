{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Ombra.Draw.Mode (
        DrawMode,
        mode,
        bufferMode,
        blend,
        noBlend,
        stencil,
        noStencil,
        depthTest,
        depthMask,
        colorMask,
        preVertex,
        extVertex,
        preFragment,
        -- * DrawRoute
        -- ** Static
        DrawRoute,
        route,
        routeShader,
        drawRoute,
        -- ** Applicative
        ApDrawRoute,
        apRoute,
        pureRoute,
        apDrawRoute
) where

import Control.Arrow
import Data.Semigroup
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Blend.Draw as Blend
import qualified Graphics.Rendering.Ombra.Blend.Types as Blend
import Graphics.Rendering.Ombra.Culling
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Draw.State
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.OutBuffer hiding (bufferPair)
import Graphics.Rendering.Ombra.Internal.SM hiding (route)
import qualified Graphics.Rendering.Ombra.Internal.SM as SM
import Graphics.Rendering.Ombra.Shader
import qualified Graphics.Rendering.Ombra.Stencil.Draw as Stencil
import qualified Graphics.Rendering.Ombra.Stencil.Types as Stencil

data DrawMode vi vo fi o where
        DrawMode :: FragmentShaderOutput fo
                 => { vertexShader :: VertexShader vi (GVec4, vo)
                    , fragmentShader :: FragmentShader fi fo
                    , blendMode :: Maybe Blend.Mode
                    , stencilMode :: Maybe Stencil.Mode
                    , cullFace :: Maybe CullFace
                    , depthTestEnabled :: Bool
                    , depthMaskEnabled :: Bool
                    , colorMaskEnabled :: (Bool, Bool, Bool, Bool)
                    , bufferPair :: Target fo o
                    }
                 -> DrawMode vi vo fi o

-- | Create a 'DrawMode' that draws directly to the screen (unless it's used
-- inside of 'drawBuffers').
mode :: FragmentShaderOutput o
     => VertexShader vi (GVec4, vo)
     -> FragmentShader fi o
     -> DrawMode vi vo fi o
mode vs fs = DrawMode { vertexShader = vs
                      , fragmentShader = fs
                      , blendMode = Nothing
                      , stencilMode = Nothing
                      , cullFace = Nothing
                      , depthTestEnabled = True
                      , depthMaskEnabled = True
                      , colorMaskEnabled = (True, True, True, True)
                      , bufferPair = DefaultTarget
                      }

-- | Create a 'DrawMode' that draws to a specified 'BufferPair'.
bufferMode :: FragmentShaderOutput fo
           => VertexShader vi (GVec4, vo)
           -> FragmentShader fi fo
           -> BufferPair fo
           -> DrawMode vi vo fi o
bufferMode vs fs buffer = DrawMode { vertexShader = vs
                                   , fragmentShader = fs
                                   , blendMode = Nothing
                                   , stencilMode = Nothing
                                   , cullFace = Nothing
                                   , depthTestEnabled = True
                                   , depthMaskEnabled = True
                                   , colorMaskEnabled = (True, True, True, True)
                                   , bufferPair = BufferTarget buffer
                                   }

-- | Enable blending and set the blend mode.
blend :: (Maybe Blend.Mode -> Blend.Mode)
      -> DrawMode vi vo fi o
      -> DrawMode vi vo fi o
blend f mode = mode { blendMode = Just . f . blendMode $ mode }

-- | Disable blending.
noBlend :: DrawMode vi vo fi o -> DrawMode vi vo fi o
noBlend mode = mode { blendMode = Nothing }

-- | Enable stencil testing and set the stencil test mode.
stencil :: (Maybe Stencil.Mode -> Stencil.Mode)
        -> DrawMode vi vo fi o
        -> DrawMode vi vo fi o
stencil f mode = mode { stencilMode = Just . f . stencilMode $ mode }

-- | Disable stencil testing.
noStencil :: DrawMode vi vo fi o -> DrawMode vi vo fi o
noStencil mode = mode { stencilMode = Nothing }

-- | Enable culling and set the cull face.
cull :: CullFace -> DrawMode vi vo fi o -> DrawMode vi vo fi o
cull sm mode = mode { cullFace = Just sm }

-- | Disable culling.
noCull :: DrawMode vi vo fi o -> DrawMode vi vo fi o
noCull mode = mode { cullFace = Nothing }

-- | Enable/disable depth testing.
depthTest :: Bool -> DrawMode vi vo fi o -> DrawMode vi vo fi o
depthTest e mode = mode { depthTestEnabled = e }

-- | Enable/disable writing to the depth buffer.
depthMask :: Bool -> DrawMode vi vo fi o -> DrawMode vi vo fi o
depthMask e mode = mode { depthMaskEnabled = e }

-- | Enable/disable writing to one or more color channels.
colorMask :: ((Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool)) 
          -> DrawMode vi vo fi o
          -> DrawMode vi vo fi o
colorMask f mode = mode { colorMaskEnabled = f . colorMaskEnabled $ mode }

preVertex :: VertexShader vi' vi
          -> DrawMode vi vo fi o
          -> DrawMode vi' vo fi o
preVertex vs mode = mode { vertexShader = vs >>> vertexShader mode }

-- | This can be used to pass additional information from the vertex shader to
-- the fragment shader.
extVertex :: VertexShader vi' (vi, a)
          -> VertexShader (vo, a) vo'
          -> DrawMode vi vo fi fo
          -> DrawMode vi' vo' fi fo
extVertex inp out mode = mode { vertexShader =     inp
                                               >>> vertexShader mode *** arr id
                                               >>> (\((v, o), a) -> (v, (o, a)))
                                               ^>> second out
                              }

preFragment :: FragmentShader fi' fi
            -> DrawMode vi vo fi o
            -> DrawMode vi vo fi' o
preFragment fs (DrawMode vs oldFs bm sm cf dt dm cm bp) =
        DrawMode vs (fs >>> oldFs) bm sm cf dt dm cm bp

modeToState :: ( GLES
               , GeometryVertex g
               , ElementType e
               , ShaderInput g
               , ShaderInput v
               , FragmentShaderOutput o
               )
            => Geometry e g
            -> DrawMode g v v o
            -> DrawState o
modeToState geom mode@(DrawMode { vertexShader = vs
                                , fragmentShader = fs
                                , bufferPair = b 
                                }) = mkState vs fs geom b
                                             (blendMode mode)
                                             (stencilMode mode)
                                             (cullFace mode)
                                             (depthTestEnabled mode)
                                             (depthMaskEnabled mode)
                                             (colorMaskEnabled mode)

-- | A DrawRoute is an optimized container for drawing operations.
newtype DrawRoute o = DrawRoute (Partial (DrawState o))
        deriving (Semigroup, Monoid)

-- | A 'DrawRoute' whose modes are wrapped in a functor. This is useful when
-- combined with FRP frameworks, because it allows you to modify the modes
-- without having to rebuild the route, at the expense of reusing a route that
-- might not be as efficient as a fresh one. The quality of the route does not
-- degrade if the equality relationships between the sample properties are
-- always mantained in the effective properties. For instance, if two parts of
-- the route have the same sample blend mode, but at some point one of the
-- routes changes the effective blend mode and the other doesn't, the old route
-- might not be as good as before for drawing the scene again. Note that this
-- doesn't matter much for the properties that can be cheaply modified like
-- uniforms.
newtype ApDrawRoute f o = ApDrawRoute (Partial (ApDrawState f o))
        deriving (Semigroup, Monoid)

-- | Create a route with only one element.
route :: ( GLES
         , GeometryVertex g
         , ElementType e
         , ShaderInput g
         , ShaderInput v
         , FragmentShaderOutput o
         )
      => Geometry e g
      -> DrawMode g v v o
      -> DrawRoute o
route geom = DrawRoute . job . modeToState geom

-- | Combination of 'route', 'preVertex' and 'preFragment'.
routeShader :: ( GLES
               , GeometryVertex g
               , ElementType e
               , ShaderInput g
               , ShaderInput v
               , FragmentShaderOutput o
               )
            => Geometry e g
            -> VertexShader g vi
            -> FragmentShader v fi
            -> DrawMode vi v fi o
            -> DrawRoute o
routeShader geom vs fs = route geom . preVertex vs . preFragment fs

-- | Applicative 'route'. The effective properties are the ones that are
-- actually used when drawing, while the sample properties are used to build the
-- route.
apRoute :: ( GLES
           , GeometryVertex g
           , ElementType e
           , ShaderInput g
           , ShaderInput v
           , FragmentShaderOutput o
           , Applicative f
           )
        => Geometry e g         -- ^ Sample Geometry
        -> DrawMode g v v o     -- ^ Sample DrawMode
        -> f (Geometry e g)     -- ^ Effective Geometry
        -> f (DrawMode g v v o) -- ^ Effective DrawMode
        -> ApDrawRoute f o
apRoute geom mode fgeom fmode =
        ApDrawRoute . job $ ApDrawState (modeToState geom mode)
                                        (modeToState <$> fgeom <*> fmode)
pureRoute :: ( GLES
             , GeometryVertex g
             , ElementType e
             , ShaderInput g
             , ShaderInput v
             , FragmentShaderOutput o
             , Applicative f
             )
          => Geometry e g       -- ^ Sample and effective Geometry
          -> DrawMode g v v o   -- ^ Sample and effective DrawMode
          -> ApDrawRoute f o
pureRoute geom mode = apRoute geom mode (pure geom) (pure mode)

drawRoute :: (GLES, FragmentShaderOutput o, MonadDraw o m)
          => DrawRoute o
          -> m ()
drawRoute (DrawRoute p) = let Route s _ = SM.route p in foldDraw s

apDrawRoute :: (GLES, FragmentShaderOutput o, MonadDraw o m, Applicative f)
            => ApDrawRoute f o
            -> f (m ())
apDrawRoute (ApDrawRoute p) = let Route s _ = SM.route p
                                  fstate (ApDrawState _ fs) = fs
                              in foldDraw <$> traverse fstate s
