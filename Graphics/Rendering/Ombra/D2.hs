{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies #-}

{-| Simplified 2D graphics system. -}
module Graphics.Rendering.Ombra.D2 (
        module Graphics.Rendering.Ombra.Generic,
        module Data.Vect.Float,
        -- * 2D Objects and Groups
        Object2D,
        IsObject2D,
        Group2D,
        IsGroup2D,
        rect,
        image,
        sprite,
        depth,
        -- ** Geometry
        Geometry2D,
        poly,
        mkGeometry2D,
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

import Control.Applicative
import Data.Vect.Float
import Graphics.Rendering.Ombra.Backend hiding (Texture, Image, Program)
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Generic
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Shapes
import Graphics.Rendering.Ombra.Types hiding (program)
import Graphics.Rendering.Ombra.Texture
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.Default2D (Image(..), Depth(..), Transform2(..), View2(..))
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Transformation

type Uniforms2D = '[Image, Depth, Transform2]

-- | A standard 2D object.
type Object2D = Object Uniforms2D Geometry2D

-- | A standard 2D object group.
type Group2D = Group (View2 ': Uniforms2D) Geometry2D

-- | 2D objects compatible with the standard 2D shader program.
type IsObject2D globals inputs = ( Subset Geometry2D inputs
                                 , Subset Uniforms2D globals
                                 , Set inputs, Set globals )

-- | 2D object groups compatible with the standard 2D shader program.
type IsGroup2D gs is = ( Subset Geometry2D is, Subset (View2 ': Uniforms2D) gs
                       , Set is, Set gs )

-- | A rectangle with a specified 'Texture'.
rect :: GLES => Texture -> Object2D
rect = flip poly . rectGeometry $ Vec2 1 1

-- | A 2D object with a specified 'Geometry'.
poly :: (IsObject2D Uniforms2D is, GLES)
     => Texture -> Geometry is -> Object Uniforms2D is
poly t g = globalTexture Image t :~>
           Depth -= 0 :~> Transform2 -= idmtx :~> geom g

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
view :: (Set gs, Set is, GLES)
     => Mat3 -> [Object gs is] -> Group (View2 ': gs) is
view m = viewVP $ const m

-- | Create a group of objects with a view matrix and 'screenMat3'.
viewScreen :: (Set gs, Set is, GLES)
           => Mat3 -> [Object gs is] -> Group (View2 ': gs) is
viewScreen m = viewVP $ \s -> screenMat3 s .*. m

-- | Create a group of objects with a view matrix, depending on the size of the
-- framebuffer.
viewVP :: (Set gs, Set is, GLES)
       => (Vec2 -> Mat3) -> [Object gs is] -> Group (View2 ': gs) is
viewVP mf = globalGroup (globalFramebufferSize View2 mf) . group

-- | A 'Layer' with the standard 2D program.
layerS :: IsGroup2D gs is => Group gs is -> Layer
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
scaleTex t = transformDraw $
                (\(w, h) -> scaleMat3 $ Vec2 w h) <$> textureSize t

-- | Scale an 'Object' so that it has the same aspect ratio as the 'Texture'
-- 
-- > scaleV $ Vec2 1 (texture height / texture width).
scaleTexAR :: (MemberGlobal Transform2 gs, GLES)
           => Texture -> Object gs is -> Object gs is
scaleTexAR t = transformDraw $
                (\(w, h) -> scaleMat3 $ Vec2 1 (h / w)) <$> textureSize t

-- | Transform a 2D 'Object'.
transform :: (MemberGlobal Transform2 gs, GLES)
          => Mat3 -> Object gs is -> Object gs is
transform m' o = (\m -> Transform2 := (.*. m') <$> m) ~~> o

-- | Transform a 2D 'Object'.
transformDraw :: (MemberGlobal Transform2 gs, GLES)
              => Draw Mat3 -> Object gs is -> Object gs is
transformDraw m' o = (\m -> Transform2 := (.*.) <$> m <*> m') ~~> o

-- | Convert the screen coordinates to GL coordinates.
screenMat3 :: Vec2      -- ^ Viewport size.
           -> Mat3
screenMat3 (Vec2 w h) = Mat3 (Vec3 (2 / w)          0           0 )
                             (Vec3    0         (- 2 / h)       0 )
                             (Vec3  (- 1)           1           1 )
