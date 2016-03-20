{-# LANGUAGE TypeOperators, DataKinds, ConstraintKinds, MultiParamTypeClasses,
             TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Graphics.Rendering.Ombra.Generic (
        -- * Objects
        Object((:~>)),
        MemberGlobal((~~>)),
        RemoveGlobal((*~>)),
        nothing,
        geom,
        modifyGeometry,

        -- * Groups
        Group,
        group,
        (~~),
        unsafeJoin,
        emptyGroup,
        globalGroup,

        -- * Layers
        Layer,
        layer,
        combineLayers,
        -- ** Sublayers
        subLayer,
        depthSubLayer,
        subRenderLayer,
        -- ** Render layers
        RenderLayer,
        renderColor,
        renderDepth,
        renderColorDepth,
        renderColorInspect,
        renderDepthInspect,
        renderColorDepthInspect,
        renderBuffers,

        -- * Shaders
        Program,
        program,
        Global((:=)),
        (-=),
        globalTexture,
        globalTexSize,
        globalFramebufferSize,

        -- * Geometries
        Geometry,
        AttrList(..),
        mkGeometry,
        extend,
        remove,

        -- * Textures
        Texture,
        mkTexture,
        -- ** Colors
        Color(..),
        colorTex,

        module Data.Vect.Float,
        module Graphics.Rendering.Ombra.Color
) where

import Control.Applicative
import Data.Typeable
import Data.Type.Equality
import Data.Vect.Float
import Data.Word (Word8)
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Types hiding (program)
import Graphics.Rendering.Ombra.Internal.GL (GLES, ActiveTexture)
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.ShaderVar
import Graphics.Rendering.Ombra.Texture
import Unsafe.Coerce

-- | An empty group.
emptyGroup :: Group is gs
emptyGroup = Empty

-- | Set a global uniform for a 'Group'.
globalGroup :: Global g -> Group gs is -> Group (g ': gs) is
globalGroup = Global

-- | An empty object.
nothing :: Object '[] '[]
nothing = NoMesh

-- | An object with a specified 'Geometry'.
geom :: Geometry i -> Object '[] i
geom = Mesh

class MemberGlobal g gs where
        -- | Modify the global of an 'Object'.
        (~~>) :: (Uniform 'S g)
              => (Draw (CPU 'S g) -> Global g)  -- ^ Changing function
              -> Object gs is
              -> Object gs is

instance {-# OVERLAPPING #-} MemberGlobal g (g ': gs) where
        f ~~> (g := c :~> o) = f c :~> o

instance {-# OVERLAPPABLE #-} ((g == g1) ~ False, MemberGlobal g gs) =>
         MemberGlobal g (g1 ': gs) where
        f ~~> (g :~> o) = g :~> (f ~~> o)

infixr 2 ~~>

class RemoveGlobal g gs gs' where
        -- | Remove a global from an 'Object'.
        (*~>) :: (a -> g) -> Object gs is -> Object gs' is

instance {-# OVERLAPPING #-} RemoveGlobal g (g ': gs) gs where
        _ *~> (_ :~> o) = o

instance {-# OVERLAPPABLE #-} ((g == g1) ~ False, RemoveGlobal g gs gs') =>
         RemoveGlobal g (g1 ': gs) (g1 ': gs') where
        r *~> (g :~> o) = g :~> (r *~> o)

infixr 2 *~>

-- | Modify the geometry of an 'Object'.
modifyGeometry :: (Empty is ~ False)
               => (Geometry is -> Geometry is')
               -> Object gs is -> Object gs is'
modifyGeometry fg (g :~> o) = g :~> modifyGeometry fg o
modifyGeometry fg (Mesh g) = Mesh $ fg g

-- | Create a 'Global' from a pure value. The first argument is ignored,
-- it just provides the type (you can use the constructor of the GPU type).
(-=) :: (ShaderVar g, Uniform 'S g) => (a -> g) -> CPU 'S g -> Global g
g -= c = g := return c

infixr 4 -=

-- | Create a 'Global' of CPU type 'ActiveTexture' using a 'Texture'.
globalTexture :: (Uniform 'S g, CPU 'S g ~ ActiveTexture, ShaderVar g, GLES)
              => (a -> g) -> Texture -> Global g
globalTexture g c = g := textureUniform c

-- | Create a 'Global' using the size of a 'Texture'.
globalTexSize :: (ShaderVar g, Uniform 'S g, GLES)
              => (a -> g) -> Texture
              -> ((Int, Int) -> CPU 'S g) -> Global g
globalTexSize g t fc = g := (fc <$> textureSize t)

-- | Create a 'Global' using the size of the framebuffer.
globalFramebufferSize :: (ShaderVar g, Uniform 'S g) => (a -> g)
                      -> (Vec2 -> CPU 'S g) -> Global g
globalFramebufferSize g fc = g := (fc . tupleToVec <$>
                                            (viewportSize <$> drawGet))
        where tupleToVec (x, y) = Vec2 (fromIntegral x) (fromIntegral y)

-- | Create a 'Group' from a list of 'Object's.
group :: (Set is, Set gs) => [Object is gs] -> Group is gs
group = foldr (\obj grp -> grp ~~ Object obj) emptyGroup

-- | Join two groups.
(~~) :: (Equal gs gs', Equal is is')
     => Group gs is -> Group gs' is'
     -> Group (Union gs gs') (Union is is')
(~~) = Append

-- | Join two groups, even if they don't provide the same variables.
unsafeJoin :: Group gs is -> Group gs' is'
           -> Group (Union gs gs') (Union is is')
unsafeJoin = Append

-- | Associate a group with a program.
layer :: (Subset progAttr grpAttr, Subset progUni grpUni)
      => Program progUni progAttr -> Group grpUni grpAttr -> Layer
layer = Layer

-- | Combine some layers.
combineLayers :: [Layer] -> Layer
combineLayers = MultiLayer

-- | Generate a 1x1 texture.
colorTex :: GLES => Color -> Texture
colorTex c = mkTexture 1 1 [ c ]

-- | Use a 'Layer' as a 'Texture' on another. Based on 'renderColor'.
subLayer :: Int                         -- ^ Texture width.
         -> Int                         -- ^ Texture height.
         -> Layer                       -- ^ Layer to draw on a 'Texture'.
         -> (Texture -> [Layer])        -- ^ Layers using the texture.
         -> Layer
subLayer w h l = subRenderLayer . renderColor w h l

-- | Use a 'Layer' as a depth 'Texture' on another. Based on 'renderDepth'.
depthSubLayer :: Int                         -- ^ Texture width.
              -> Int                         -- ^ Texture height.
              -> Layer                       -- ^ Layer to draw on a
                                             -- depth 'Texture'.
              -> (Texture -> [Layer])        -- ^ Layers using the texture.
              -> Layer
depthSubLayer w h l = subRenderLayer . renderDepth w h l

-- TODO: buffersSubLayer

-- | Generalized version of 'subLayer' and 'depthSubLayer'.
subRenderLayer :: RenderLayer [Layer] -> Layer
subRenderLayer = SubLayer

-- | Render a 'Layer' in a 'Texture'.
renderColor :: Int                         -- ^ Texture width.
            -> Int                         -- ^ Texture height.
            -> Layer                       -- ^ Layer to draw on a 'Texture'.
            -> (Texture -> a)              -- ^ Function using the texture.
            -> RenderLayer a
renderColor w h l f = RenderLayer False [ColorLayer, DepthLayer] w h 0 0 0 0
                                  False False l $ \[t, _] _ _ -> f t

-- | Render a 'Layer' in a depth 'Texture'
renderDepth :: Int              -- ^ Texture width.
            -> Int              -- ^ Texture height.
            -> Layer            -- ^ Layer to draw on a depth 'Texture'.
            -> (Texture -> a)   -- ^ Function using the texture.
            -> RenderLayer a
renderDepth w h l f =
        RenderLayer False [DepthLayer] w h 0 0 0 0 False False l $
                \[t] _ _ -> f t

-- | Combination of 'renderColor' and 'renderDepth'.
renderColorDepth :: Int                         -- ^ Texture width.
                 -> Int                         -- ^ Texture height.
                 -> Layer                       -- ^ Layer to draw on a 'Texture'
                 -> (Texture -> Texture -> a)   -- ^ Color, depth.
                 -> RenderLayer a
renderColorDepth w h l f =
        RenderLayer False [ColorLayer, DepthLayer] w h 0 0 0 0 False False l $
                    \[ct, dt] _ _ -> f ct dt

-- | Render a 'Layer' in a 'Texture', reading the content of the texture.
renderColorInspect
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Color] -> a)    -- ^ Function using the texture.
        -> RenderLayer a
renderColorInspect w h l rx ry rw rh f =
        RenderLayer False [ColorLayer, DepthLayer] w h rx ry
                    rw rh True False l $
                    \[t, _] (Just c) _ -> f t c

-- | Render a 'Layer' in a depth 'Texture', reading the content of the texture.
-- Not supported on WebGL.
renderDepthInspect
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a depth 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Word8] -> a)    -- ^ Layers using the texture.
        -> RenderLayer a
renderDepthInspect w h l rx ry rw rh f =
        RenderLayer False [DepthLayer] w h rx ry rw rh False True l $
                    \[t] _ (Just d) -> f t d

-- | Combination of 'renderColorInspect' and 'renderDepthInspect'. Not supported
-- on WebGL.
renderColorDepthInspect
        :: Int         -- ^ Texture width.
        -> Int         -- ^ Texture height.
        -> Layer       -- ^ Layer to draw on a 'Texture'
        -> Int         -- ^ First pixel to read X
        -> Int         -- ^ First pixel to read Y
        -> Int         -- ^ Width of the rectangle to read
        -> Int         -- ^ Height of the rectangle to read
        -> (Texture -> Texture -> [Color] -> [Word8] -> a)  -- ^ Layers using
                                                            -- the texture.
        -> RenderLayer a
renderColorDepthInspect w h l rx ry rw rh f =
        RenderLayer False [ColorLayer, DepthLayer] w h rx ry rw rh True True l $
                    \[ct, dt] (Just c) (Just d) -> f ct dt c d

-- | Render a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) in some 'Texture's.
renderBuffers :: Int                           -- ^ Textures width.
              -> Int                           -- ^ Textures height.
              -> Int                           -- ^ Number of colors.
              -> Layer                         -- ^ Layer to draw.
              -> ([Texture] -> a)              -- ^ Function using the texture.
              -> RenderLayer a
renderBuffers w h n l f =
        RenderLayer True (map BufferLayer [0 .. n - 1]) w h
                    0 0 0 0 False False l $ \ts _ _ -> f ts

-- TODO: renderBuffersDepth
