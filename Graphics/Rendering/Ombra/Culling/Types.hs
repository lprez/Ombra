module Graphics.Rendering.Ombra.Culling.Types (
        CullFace(..)
) where

-- Side(s) to be culled.
data CullFace = CullFront | CullBack | CullFrontBack deriving Eq
