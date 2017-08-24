module Graphics.Rendering.Ombra.Culling.Draw (
        MonadCulling(..)
) where

import Graphics.Rendering.Ombra.Internal.GL
import Graphics.Rendering.Ombra.Culling.Types

class (GLES, MonadGL m) => MonadCulling m where
        withCulling :: Maybe CullFace -> m a -> m a
