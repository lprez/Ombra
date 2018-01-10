module Graphics.Rendering.Ombra.Culling (
        CullFace(..)
) where

-- Side(s) to be culled.
data CullFace = CullFront | CullBack | CullFrontBack deriving (Eq, Ord)
