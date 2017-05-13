{-# LANGUAGE GADTs, DataKinds, PolyKinds, ConstraintKinds #-}

module Graphics.Rendering.Ombra.Object.Internal (
        MonadObject,
        MonadDrawingMode(..),
        drawObject
) where

import Data.Proxy (Proxy(..))
import Graphics.Rendering.Ombra.Blend as Blend
import Graphics.Rendering.Ombra.Geometry.Internal
import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Object.Types
import Graphics.Rendering.Ombra.Screen
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.ShaderVar (varBuild)
import Graphics.Rendering.Ombra.Stencil as Stencil
import Graphics.Rendering.Ombra.Texture.Internal

type MonadObject m = ( MonadProgram m
                     , MonadTexture m
                     , MonadScreen m
                     , MonadGeometry m
                     , MonadDrawingMode m
                     )

class MonadDrawingMode m where
        withBlendMode :: Maybe Blend.Mode -> m a -> m a
        withStencilMode :: Maybe Stencil.Mode -> m a -> m a
        withDepthTest :: Bool -> m a -> m a
        withDepthMask :: Bool -> m a -> m a
        withColorMask :: (Bool, Bool, Bool, Bool) -> m a -> m a
        withCulling :: Maybe CullFace -> m a -> m a

withGlobal :: (MonadProgram m, MonadTexture m, MonadScreen m)
           => Global g
           -> m ()
           -> m ()
withGlobal (Single g c) act =
        setUniformValue (Proxy :: Proxy 'S)  (g undefined) c >> act
withGlobal (Mirror g c) act =
           setUniformValue (Proxy :: Proxy 'M) (varBuild (const undefined) g) c
        >> act
withGlobal (WithTexture t gf) act =
        withActiveTexture t () $ flip withGlobal act . gf
withGlobal (WithTextureSize t gf) act =
        textureSize t >>= flip withGlobal act . gf
withGlobal (WithFramebufferSize gf) act =
        currentViewport >>= flip withGlobal act . gf

withObjProp :: MonadDrawingMode m => ObjProp -> m a -> m a
withObjProp (Blend m) a = withBlendMode m a
withObjProp (Stencil m) a = withStencilMode m a
withObjProp (DepthTest d) a = withDepthTest d a
withObjProp (DepthMask m) a = withDepthMask m a
withObjProp (ColorMask m) a = withColorMask m a
withObjProp (Cull face) a = withCulling face a

drawObject :: MonadObject m
           => Object gs is
           -> m ()
drawObject (g :~> o) = withGlobal g $ drawObject o
drawObject (Mesh g) = drawGeometry g
drawObject NoMesh = return ()
drawObject (Prop p o) = withObjProp p $ drawObject o
drawObject (Append o o') = drawObject o >> drawObject o'
