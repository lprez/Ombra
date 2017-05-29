{-# LANGUAGE TypeOperators, DataKinds, ConstraintKinds, MultiParamTypeClasses,
             TypeFamilies, FlexibleContexts, FlexibleInstances #-}

-- |
-- Module:      Graphics.Rendering.Ombra.Object
-- License:     BSD3
-- Maintainer:  ziocroc@gmail.com
-- Stability:   experimental
-- Portability: GHC only

module Graphics.Rendering.Ombra.Object (
        -- * Creating and modifying objects
        Object((:~>)),
        nothing,
        -- merge,
        geom,
        modifyGeometry,

        -- * Object properties
        depthTest,
        depthMask,
        colorMask,
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

        -- * Globals
        Global,
        (-=),
        withTexture,
        withTexSize,
        withFramebufferSize,
        ActiveTexture,
        -- ** Mirror globals
        mirror,
        CPUMirror,
        -- ** Modifying globals
        MemberGlobal((~~>)),
        RemoveGlobal((*~>)),
) where

import Data.Typeable
import Data.Type.Equality
import Data.Word (Word8)
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Blend as Blend
import qualified Graphics.Rendering.Ombra.Stencil as Stencil
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Object.Types
import Graphics.Rendering.Ombra.Internal.GL (ActiveTexture)
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.CPU hiding (mirror)
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.ShaderVar
import Graphics.Rendering.Ombra.Shader.Stages
import Graphics.Rendering.Ombra.Texture

-- | Enable blending and set the blending mode for an 'Object'.
blend :: Blend.Mode -> Object gs is -> Object gs is
blend m = Prop . Blend $ Just m

-- | Disable blending for a 'Object'.
noBlend :: Object gs is -> Object gs is
noBlend = Prop $ Blend Nothing

-- | Enable stencil testing and set the stencil mode for an 'Object'.
stencil :: Stencil.Mode -> Object gs is -> Object gs is
stencil m = Prop . Stencil $ Just m

-- | Disable stencil testing on a 'Object' of objects.
noStencil :: Object gs is -> Object gs is
noStencil = Prop $ Stencil Nothing

-- | Enable/disable depth testing for a 'Object'.
depthTest :: Bool -> Object gs is -> Object gs is
depthTest d = Prop $ DepthTest d

-- | Enable/disable writing into the depth buffer for a 'Object'.
depthMask :: Bool -> Object gs is -> Object gs is
depthMask m = Prop $ DepthMask m

-- | Enable/disable writing into the four channels of the color buffer for a
-- 'Object'.
colorMask :: (Bool, Bool, Bool, Bool) -> Object gs is -> Object gs is
colorMask m = Prop $ ColorMask m

-- | Enable face culling.
cull :: CullFace -> Object gs is -> Object gs is
cull m = Prop . Cull $ Just m

-- | Disable face culling.
noCull :: Object gs is -> Object gs is
noCull = Prop $ Cull Nothing

-- | An empty object.
nothing :: Object '[] '[]
nothing = NoMesh

-- | An object with a specified 'Geometry'.
geom :: Geometry (i ': is) -> Object '[] (i ': is)
geom = Mesh

-- TODO: Either (CPU 'S g) (CPU 'M g) ???
class MemberGlobal g gs where
        -- | Modify the global of an 'Object'. This doesn't work with mirror
        -- globals.
        (~~>) :: (Uniform 'S g)
              => (CPU 'S g -> Global g)  -- ^ Changing function
              -> Object gs is
              -> Object gs is

instance {-# OVERLAPPING #-} MemberGlobal g (g ': gs) where
        f ~~> (g :~> o) = globalApply f g :~> o
        f ~~> (Prop p o) = Prop p $ f ~~> o
        f ~~> (Append o o') = Append (f ~~> o) (f ~~> o')
        f ~~> NoMesh = NoMesh

instance {-# OVERLAPPABLE #-} ((g == g1) ~ False, MemberGlobal g gs) =>
         MemberGlobal g (g1 ': gs) where
        f ~~> (g :~> o) = g :~> (f ~~> o)
        f ~~> (Prop p o) = Prop p $ f ~~> o
        f ~~> (Append o o') = Append (f ~~> o) (f ~~> o')
        f ~~> NoMesh = NoMesh
        
globalApply :: (Uniform 'S g)
            => (CPU 'S g -> Global g)
            -> Global g
            -> Global g
globalApply f (Single g c) = f c
globalApply f (WithTexture t g) = WithTexture t $ globalApply f . g
globalApply f (WithTextureSize t g) = WithTextureSize t $ globalApply f . g
globalApply f (WithFramebufferSize g) = WithFramebufferSize $ globalApply f . g
globalApply f g = g

infixr 2 ~~>

class RemoveGlobal g gs' where
        -- | Remove a global from an 'Object'.
        (*~>) :: (a -> g) -> Object gs' is -> Object (Remove g gs') is

instance {-# OVERLAPPING #-} Remove g (g : gs') ~ gs' =>
        RemoveGlobal g (g ': gs') where
        _ *~> (_ :~> o) = o
        r *~> (Prop p o) = Prop p $ r *~> o
        r *~> (Append o o') = Append (r *~> o) (r *~> o')
        r *~> NoMesh = NoMesh

instance {-# OVERLAPPABLE #-} ( Remove g (g' ': gs') ~ (g' ': Remove g gs')
                              , RemoveGlobal g gs'
                              ) => RemoveGlobal g (g' ': gs') where
        r *~> (g :~> o) = g :~> (r *~> o)
        r *~> (Prop p o) = Prop p $ r *~> o
        r *~> (Append o o') = Append (r *~> o) (r *~> o')
        r *~> NoMesh = NoMesh

infixr 2 *~>

-- | Modify the geometry of an 'Object'.
modifyGeometry :: (Geometry (i ': is) -> Geometry (i' : is'))
               -> Object gs (i ': is) -> Object gs (i' : is')
modifyGeometry fg (g :~> o) = g :~> modifyGeometry fg o
modifyGeometry fg (Mesh g) = Mesh $ fg g
modifyGeometry fg (Prop p o) = Prop p $ modifyGeometry fg o
modifyGeometry fg (Append o o') = Append (modifyGeometry fg o)
                                         (modifyGeometry fg o')
modifyGeometry fg NoMesh = NoMesh

-- | Create a 'Global' from a pure value. The first argument is ignored,
-- it just provides the type (you can use the constructor of the GPU type).
-- You can use this to set the value of a shader uniform.
(-=) :: (ShaderVar g, Uniform 'S g) => (a -> g) -> CPU 'S g -> Global g
(-=) = Single

infixr 4 -=

-- | Create a 'Global' activating a 'Texture'. Note that the corresponding CPU
-- type of 'Sampler2D' is 'ActiveTexture', not Texture.
withTexture :: Texture -> (ActiveTexture -> Global g) -> Global g
withTexture = WithTexture

-- | Create a 'Global' using the size of a 'Texture'.
withTexSize :: Texture -> ((Int, Int) -> Global g) -> Global g
withTexSize = WithTextureSize

-- | Create a 'Global' using the size of the framebuffer.
withFramebufferSize :: ((Int, Int) -> Global g) -> Global g
withFramebufferSize = WithFramebufferSize

-- TODO: remove Proxy?
-- | Like '-=' but for mirror types.
mirror :: (ShaderVar g, Uniform 'M g) => Proxy g -> CPU 'M g -> Global g
mirror = Mirror

{-
type EqualMerge x y v = EqualOrErr x y (Text "Can't merge groups with " :<>:
                                        Text "different " :<>: v :<>:
                                        Text "." :$$:
                                        Text "    Left group " :<>: v :<>:
                                        Text ": " :<>: ShowType x :$$:
                                        Text "    Right group " :<>: v :<>:
                                        Text ": " :<>: ShowType y)


-- | Merge two objects. This is more generic than 'mappend'.
merge :: (EqualMerge gs gs' (Text "globals"), EqualMerge is is' (Text "inputs"))
      => Object gs is -> Object gs' is'
      -> Object (Union gs gs') (Union is is')
merge = Append
-}

{-
-- | Merge two objects, even if they don't provide the same variables.
unsafeMerge :: Object gs is -> Object gs' is'
            -> Object (Union gs gs') (Union is is')
unsafeMerge = Append
-}
