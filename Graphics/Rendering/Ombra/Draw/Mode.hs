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
        modeToState,
) where

import Control.Arrow
import Data.Functor.Identity
import Data.Semigroup
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Blend.Draw as Blend
import qualified Graphics.Rendering.Ombra.Blend.Types as Blend
import Graphics.Rendering.Ombra.Culling
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Draw.State
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.OutBuffer hiding (bufferPair)
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

-- |
-- @
-- preVertex vs' ('mode' vs fs) = 'mode' (vs' >>> vs) fs
-- @
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

-- |
-- @
-- preFragment fs' ('mode' vs fs) = 'mode' vs (fs' >>> fs)
-- @
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
