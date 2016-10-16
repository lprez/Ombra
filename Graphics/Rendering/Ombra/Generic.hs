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
        groupEmpty,
        groupGlobal,
        depthTest,
        depthMask,
        colorMask,
        Variables,
        -- ** Blending
        blend,
        noBlend,
        Blend.transparency,
        Blend.additive,
        -- ** Stencil test
        stencil,
        noStencil,
        -- ** Culling
        CullFace(..),
        cull,
        noCull,

        -- * Layers
        Buffer(..),
        Layer,
        layer,
        over,
        clear,
        -- ** Sublayers
        subLayer,
        depthSubLayer,
        subRenderLayer,
        -- ** Render layers
        RenderLayer,
        renderColor,
        renderDepth,
        renderColorDepth,
        renderColorStencil,
        renderColorInspect,
        renderDepthInspect,
        renderColorDepthInspect,
        renderColorStencilInspect,
        renderBuffers,
        renderBuffersDepth,
        renderBuffersStencil,

        -- * Shaders
        Compatible,
        Program,
        program,
        Global,
        (-=),
        globalTexture,
        globalTexSize,
        globalFramebufferSize,
        CPUMirror,
        globalMirror,
        globalMirror',

        -- * Geometries
        Geometry,
        AttrList(..),
        mkGeometry,
        extend,
        remove,

        -- * Textures
        Texture,
        ActiveTexture,
        mkTexture,
        mkTextureFloat,
        Filter(..),
        setFilter,
        -- ** Colors
        Color(..),
        colorTex,

        GLES,
        module Data.Vect.Float,
        module Graphics.Rendering.Ombra.Color
) where

import Control.Applicative
import Data.Typeable
import Data.Type.Equality
import Data.Vect.Float
import Data.Word (Word8)
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Blend as Blend
import qualified Graphics.Rendering.Ombra.Stencil as Stencil
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Types hiding (program, depthTest,
                                              depthMask, colorMask)
import Graphics.Rendering.Ombra.Internal.GL (GLES, ActiveTexture)
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.ShaderVar
import Graphics.Rendering.Ombra.Texture
import Unsafe.Coerce

-- | An empty group.
groupEmpty :: Group gs is
groupEmpty = Empty

-- | Set a global uniform for a 'Group'.
groupGlobal :: Global g -> Group gs is -> Group (g ': gs) is
groupGlobal = Global

-- | Enable blending and set the blending mode for a 'Group' of objects.
blend :: Blend.Mode -> Group gs is -> Group gs is
blend m = Blend $ Just m
-- TODO: should search and modify existing Blend

-- | Disable blending for a 'Group'.
noBlend :: Group gs is -> Group gs is
noBlend = Blend Nothing
-- TODO: should search and modify existing Blend

-- | Enable stencil testing and set the stencil mode for a 'Group' of objects.
stencil :: Stencil.Mode -> Group gs is -> Group gs is
stencil m = Stencil $ Just m
-- TODO: should search and modify existing Stencil

-- | Disable stencil testing on a 'Group' of objects.
noStencil :: Group gs is -> Group gs is
noStencil = Stencil Nothing
-- TODO: should search and modify existing Stencil

-- | Enable/disable depth testing for a 'Group'.
depthTest :: Bool -> Group gs is -> Group gs is
depthTest = DepthTest
-- TODO: should search and modify existing DepthTest

-- | Enable/disable writing into the depth buffer for a 'Group'.
depthMask :: Bool -> Group gs is -> Group gs is
depthMask = DepthMask
-- TODO: should search and modify existing DepthMask

-- | Enable/disable writing into the four channels of the color buffer for a
-- 'Group'.
colorMask :: (Bool, Bool, Bool, Bool) -> Group gs is -> Group gs is
colorMask = ColorMask
-- TODO: should search and modify existing DepthMask

-- | Enable face culling.
cull :: CullFace -> Group gs is -> Group gs is
cull m = Cull $ Just m
-- TODO: should search and modify existing Cull

-- | Disable face culling.
noCull :: Group gs is -> Group gs is
noCull = Cull Nothing
-- TODO: should search and modify existing Cull

-- | An empty object.
nothing :: Object '[] '[]
nothing = NoMesh

-- | An object with a specified 'Geometry'.
geom :: Geometry i -> Object '[] i
geom = Mesh

class MemberGlobal g gs where
        -- | Modify the global of an 'Object'. This doesn't work with mirror
        -- globals.
        (~~>) :: (Uniform 'S g)
              => (Draw (CPU 'S g) -> Global g)  -- ^ Changing function
              -> Object gs is
              -> Object gs is

instance {-# OVERLAPPING #-} MemberGlobal g (g ': gs) where
        f ~~> (g := c :~> o) = f c :~> o
        f ~~> (glob :~> o) = glob :~> o

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
-- You can use this to set the value of a shader uniform.
(-=) :: (ShaderVar g, Uniform 'S g) => (a -> g) -> CPU 'S g -> Global g
g -= c = g := return c

infixr 4 -=

-- TODO: polymorphic -= instead of globalTexture
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

tupleToVec :: (Int, Int) -> Vec2
tupleToVec (x, y) = Vec2 (fromIntegral x) (fromIntegral y)

-- | Like '-=' but for mirror types.
globalMirror :: (ShaderVar g, Uniform 'M g) => Proxy g -> CPU 'M g -> Global g
globalMirror g c = Mirror g $ return c

-- | Extended version of 'globalMirror'.
globalMirror' :: (GLES, ShaderVar g, Uniform 'M g)
              => Proxy g
              -> [Texture]      -- ^ Textures to make active. Remember that
                                -- the CPU version of 'Sampler2D' is
                                -- 'ActiveTexture', not 'Texture'.
              -> ([(ActiveTexture, (Int, Int))] -> Vec2 -> CPU 'M g)
                                -- ^ Function that, given a list of active
                                -- textures (the same passed in the second
                                -- argument) and their size, and the
                                -- framebuffer value, build the CPU value of
                                -- the global.
              -> Global g
globalMirror' g ts f = Mirror g $ f <$> mapM ( \t -> (,) <$> textureUniform t
                                                         <*> textureSize t) ts
                                    <*> (tupleToVec . viewportSize <$> drawGet)

-- | Valid variables.
type Variables v = Set v

-- | Create a 'Group' from a list of 'Object's.
group :: (Variables gs, Variables is) => [Object gs is] -> Group gs is
group = foldr (\obj grp -> grp ~~ Object obj) groupEmpty

type EqualJoin x y v = EqualOrErr x y (Text "Can't join groups with " :<>:
                                       Text "different " :<>: v :<>:
                                       Text "." :$$:
                                       Text "    Left group " :<>: v :<>:
                                       Text ": " :<>: ShowType x :$$:
                                       Text "    Right group " :<>: v :<>:
                                       Text ": " :<>: ShowType y)


-- | Join two groups.
(~~) :: (EqualJoin gs gs' (Text "globals"), EqualJoin is is' (Text "inputs"))
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

infixl 1 `over`
-- | Draw the first Layer over the second one. The first Layer will use the same
-- buffers (color, depth, stencil) of the second one.
over :: Layer -> Layer -> Layer
over = OverLayer

-- | Clear some buffers before drawing a Layer.
clear :: [Buffer] -> Layer -> Layer
clear = ClearLayer

-- | Generate a 1x1 texture.
colorTex :: GLES => Color -> Texture
colorTex c = mkTexture 1 1 [ c ]

-- | Use a 'Layer' as a 'Texture' on another.
--
-- > subLayer w h l = subRenderLayer . renderColor w h l
subLayer :: Int                         -- ^ Texture width.
         -> Int                         -- ^ Texture height.
         -> Layer                       -- ^ Layer to draw on a 'Texture'.
         -> (Texture -> Layer)          -- ^ Layers using the texture.
         -> Layer
subLayer w h l = subRenderLayer . renderColor w h l

-- | Use a 'Layer' as a depth 'Texture' on another.
--
-- > depthSubLayer w h l = subRenderLayer . renderDepth w h l
depthSubLayer :: Int                         -- ^ Texture width.
              -> Int                         -- ^ Texture height.
              -> Layer                       -- ^ Layer to draw on a
                                             -- depth 'Texture'.
              -> (Texture -> Layer)          -- ^ Layers using the texture.
              -> Layer
depthSubLayer w h l = subRenderLayer . renderDepth w h l

-- TODO: buffersSubLayer

-- | Generalized version of 'subLayer' and 'depthSubLayer'.
subRenderLayer :: RenderLayer Layer -> Layer
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

-- | 'renderColor' with an additional stencil buffer.
renderColorStencil :: Int                         -- ^ Texture width.
                   -> Int                         -- ^ Texture height.
                   -> Layer                       -- ^ Layer to draw on a 'Texture'
                   -> (Texture -> a)              -- ^ Color.
                   -> RenderLayer a
renderColorStencil w h l f =
        RenderLayer False [ColorLayer, DepthStencilLayer] w h 0 0 0 0
                    False False l $
                    \[ct, _] _ _ -> f ct

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

-- | 'renderColorInspect' with an additional stencil buffer.
renderColorStencilInspect
        :: Int                          -- ^ Texture width.
        -> Int                          -- ^ Texture height.
        -> Layer                        -- ^ Layer to draw on a 'Texture'.
        -> Int                          -- ^ First pixel to read X
        -> Int                          -- ^ First pixel to read Y
        -> Int                          -- ^ Width of the rectangle to read
        -> Int                          -- ^ Height of the rectangle to read
        -> (Texture -> [Color] -> a)    -- ^ Function using the texture.
        -> RenderLayer a
renderColorStencilInspect w h l rx ry rw rh f =
        RenderLayer False [ColorLayer, DepthStencilLayer] w h rx ry
                    rw rh True False l $
                    \[t, _] (Just c) _ -> f t c

-- | Render a 'Layer' with multiple floating point colors
-- (use 'Fragment2', 'Fragment3', etc.) in some 'Texture's.
renderBuffers :: Int                           -- ^ Textures width.
              -> Int                           -- ^ Textures height.
              -> Int                           -- ^ Number of colors.
              -> Layer                         -- ^ Layer to draw.
              -> ([Texture] -> a)              -- ^ Function using the textures.
              -> RenderLayer a
renderBuffers w h n l f =
        RenderLayer True (DepthLayer : map BufferLayer [0 .. n - 1]) w h
                    0 0 0 0 False False l $ \(_ : ts) _ _ -> f ts

-- | Combination of 'renderBuffers' and 'renderDepth'.
renderBuffersDepth :: Int                           -- ^ Textures width.
                   -> Int                           -- ^ Textures height.
                   -> Int                           -- ^ Number of colors.
                   -> Layer                         -- ^ Layer to draw.
                   -> ([Texture] -> Texture -> a)   -- ^ Function using the
                                                    -- buffers textures and
                                                    -- the depth texture.
                   -> RenderLayer a
renderBuffersDepth w h n l f =
        RenderLayer True (DepthLayer : map BufferLayer [0 .. n - 1]) w h
                    0 0 0 0 False False l $ \(dt : ts) _ _ -> f ts dt

-- | 'renderBuffers' with an additional stencil buffer.
renderBuffersStencil :: Int                      -- ^ Textures width.
                     -> Int                      -- ^ Textures height.
                     -> Int                      -- ^ Number of colors.
                     -> Layer                    -- ^ Layer to draw.
                     -> ([Texture] -> a)         -- ^ Function using the texture.
                     -> RenderLayer a
renderBuffersStencil w h n l f =
        RenderLayer True (DepthStencilLayer : map BufferLayer [0 .. n - 1]) w h
                    0 0 0 0 False False l $ \(_ : ts) _ _ -> f ts
