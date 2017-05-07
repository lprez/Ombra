{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies #-}

{-| Simplified 2D graphics system. -}
module Graphics.Rendering.Ombra.D2 (
        -- * 2D Objects
        Object2D,
        IsObject2D,
        rect,
        image,
        sprite,
        depth,
        poly,
        -- * Transformations
        trans,
        rot,
        scale,
        scaleV,
        scaleTex,
        scaleTexAR,
        transform,
        -- * Layers
        view,
        viewScreen,
        viewVP,
        layerS,
        -- * Transformation matrices
        transMat3,
        rotMat3,
        scaleMat3,
        screenMat3,
        -- * Uniforms
        Uniforms2D,
        Image(..),
        Depth(..),
        Transform2(..),
        View2(..),
) where

import Graphics.Rendering.Ombra.Backend hiding (Texture, Program)
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Layer
import Graphics.Rendering.Ombra.Object
import Graphics.Rendering.Ombra.Shapes
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.Default2D (Image(..), Depth(..), Transform2(..), View2(..))
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Texture
import Graphics.Rendering.Ombra.Transformation
import Graphics.Rendering.Ombra.Vector

type Uniforms2D = '[Image, Depth, Transform2]

-- | A simple 2D Object, without the 'View2' matrix.
type Object2D = Object Uniforms2D Geometry2D

-- | 2D objects compatible with the standard 2D shader program.
type IsObject2D gs is = ( Subset Geometry2D is, Subset (View2 ': Uniforms2D) gs
                        , ShaderVars is, ShaderVars gs )

-- | A rectangle with a specified 'Texture'.
rect :: GLES => Texture -> Object2D
rect = flip poly rectGeometry

-- | A 2D object with a specified 'Geometry'.
poly :: GLES => Texture -> Geometry (i ': is) -> Object Uniforms2D (i ': is)
poly t g = withTexture t (Image -=) :~>
           Depth -= 0 :~>
           Transform2 -= idmtx :~>
           geom g

-- | A rectangle with the aspect ratio adapted to its texture.
image :: GLES => Texture -> Object2D
image t = scaleTexAR t $ rect t

-- | Set the depth of a 2D 'Object'.
depth :: (MemberGlobal Depth gs, GLES)
      => Float -> Object gs is -> Object gs is
depth d obj = const (Depth -= d) ~~> obj

-- | A rectangle with the size and aspect ratio adapted to the screen, assuming
-- that you're using 'viewScreen' or 'screenMat3'.
sprite :: GLES => Texture -> Object2D
sprite t = scaleTex t $ rect t

-- | Create a group of objects with a view matrix.
view :: (ShaderVars gs, ShaderVars is, GLES)
     => Mat3 -> [Object gs is] -> Object (View2 ': gs) is
view m = viewVP $ const m

-- | Create a group of objects with a view matrix and 'screenMat3'.
viewScreen :: (ShaderVars gs, ShaderVars is, GLES)
           => Mat3 -> [Object gs is] -> Object (View2 ': gs) is
viewScreen m = viewVP $ \s -> screenMat3 s .*. m

-- | Create a group of objects with a view matrix, depending on the size of the
-- framebuffer.
viewVP :: (ShaderVars gs, ShaderVars is, GLES)
       => (Vec2 -> Mat3) -> [Object gs is] -> Object (View2 ': gs) is
viewVP mf os = withFramebufferSize (\s -> View2 -= mf (tupleToVec s))
               :~> mconcat os

-- | A 'Layer' with the standard 2D program.
layerS :: IsObject2D gs is => Object gs is -> Layer' s t ()
layerS = layer defaultProgram2D

-- | Translate a 2D 'Object'.
trans :: (MemberGlobal Transform2 gs, GLES)
      => Vec2 -> Object gs is -> Object gs is
trans v = transform $ transMat3 v

-- | Rotate a 2D 'Object'.
rot :: (MemberGlobal Transform2 gs, GLES)
    => Float -> Object gs is -> Object gs is
rot a = transform $ rotMat3 a

-- | Scale a 2D 'Object'.
scale :: (MemberGlobal Transform2 gs, GLES)
      => Float -> Object gs is -> Object gs is
scale f = transform $ scaleMat3 (Vec2 f f)

-- | Scale a 2D 'Object' in two dimensions.
scaleV :: (MemberGlobal Transform2 gs, GLES)
       => Vec2 -> Object gs is -> Object gs is
scaleV v = transform $ scaleMat3 v

-- | Scale an 'Object' so that it has the same size as the 'Texture', assuming
-- 'viewScreen' or 'screenMat3'.
scaleTex :: (MemberGlobal Transform2 gs, GLES)
           => Texture -> Object gs is -> Object gs is
scaleTex t = transform' t $ scaleMat3

-- | Scale an 'Object' so that it has the same aspect ratio as the 'Texture'
-- 
-- > scaleV $ Vec2 1 (texture height / texture width).
scaleTexAR :: (MemberGlobal Transform2 gs, GLES)
           => Texture -> Object gs is -> Object gs is
scaleTexAR t = transform' t $ (\(Vec2 w h) -> scaleMat3 $ Vec2 1 (h / w))

-- | Transform a 2D 'Object'.
transform :: (MemberGlobal Transform2 gs, GLES)
          => Mat3 -> Object gs is -> Object gs is
transform m' o = (\m -> Transform2 -= m .*. m') ~~> o

-- | Transform a 2D 'Object'.
transform' :: (MemberGlobal Transform2 gs, GLES)
           => Texture
           -> (Vec2 -> Mat3)
           -> Object gs is
           -> Object gs is
transform' t m' o = (\m -> withTexSize t $
                        \s -> Transform2 -= m .*. m' (tupleToVec s)) ~~> o

-- | Convert the screen coordinates to GL coordinates.
screenMat3 :: Vec2      -- ^ Viewport size.
           -> Mat3
screenMat3 (Vec2 w h) = Mat3 (Vec3 (2 / w)          0           0 )
                             (Vec3    0         (- 2 / h)       0 )
                             (Vec3  (- 1)           1           1 )

tupleToVec :: (Int, Int) -> Vec2
tupleToVec (w, h) = Vec2 (fromIntegral w) (fromIntegral h)
