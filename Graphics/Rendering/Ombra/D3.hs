{-# LANGUAGE DataKinds, FlexibleContexts, ConstraintKinds, TypeOperators,
             TypeFamilies, MultiParamTypeClasses, FlexibleInstances,
             UndecidableInstances #-}

{-| Simplified 3D graphics system. -}
module Graphics.Rendering.Ombra.D3 (
        -- * 3D Objects
        Object3D,
        IsObject3D,
        cube,
        mesh,
        -- * Transformations
        trans,
        rotX,
        rotY,
        rotZ,
        rot,
        scale,
        scaleV,
        transform,
        -- * Layers
        view,
        viewPersp,
        viewOrtho,
        viewVP,
        layerS,
        -- * Matrices
        -- ** View matrices
        perspectiveMat4,
        perspectiveMat4Size,
        orthoMat4,
        cameraMat4,
        lookAtMat4,
        -- ** Transformation matrices
        transMat4,
        rotXMat4,
        rotYMat4,
        rotZMat4,
        rotMat4,
        scaleMat4,
        -- * Uniforms
        Uniforms3D,
        Texture2(..),
        Transform3(..),
        View3(..),
) where

import Data.Vect.Float
import Graphics.Rendering.Ombra.Backend hiding (Texture, Program)
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Layer
import Graphics.Rendering.Ombra.Object
import Graphics.Rendering.Ombra.Shapes
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Shader.Default3D (Texture2(..), Transform3(..), View3(..))
import Graphics.Rendering.Ombra.Shader.Program hiding (program)
import Graphics.Rendering.Ombra.Texture
import Graphics.Rendering.Ombra.Transformation

type Uniforms3D = '[Transform3, Texture2]

-- | A standard 3D object, without the 'View3' matrix.
type Object3D = Object Uniforms3D Geometry3D

-- | 3D objects compatible with the standard 3D shader program.
type IsObject3D gs is = ( Subset Geometry3D is, Subset (View3 ': Uniforms3D) gs
                        , ShaderVars is, ShaderVars gs )

-- | A cube with a specified 'Texture'.
cube :: GLES => Texture -> Object3D
cube = flip mesh cubeGeometry

-- | A 3D object with a specified 'Geometry'.
mesh :: GLES => Texture -> Geometry is -> Object Uniforms3D is
mesh t g = Transform3 -= idmtx :~> withTexture t (Texture2 -=) :~> geom g

-- | Create a group of objects with a view matrix.
view :: (GLES, ShaderVars gs, ShaderVars is)
     => Mat4 -> [Object gs is] -> Object (View3 ': gs) is
view m = viewVP $ const m

-- | Create a group of objects with a view matrix and perspective projection.
viewPersp :: (GLES, ShaderVars gs, ShaderVars is)
          => Float      -- ^ Near
          -> Float      -- ^ Far
          -> Float      -- ^ FOV
          -> Mat4       -- ^ View matrix
          -> [Object gs is]
          -> Object (View3 ': gs) is
viewPersp n f fov m = viewVP $ \s -> m .*. perspectiveMat4Size n f fov s

-- | Create a group of objects with a view matrix and orthographic projection.
viewOrtho :: (GLES, ShaderVars gs, ShaderVars is)
          => Float      -- ^ Near
          -> Float      -- ^ Far
          -> Float      -- ^ Left
          -> Float      -- ^ Right
          -> Float      -- ^ Bottom
          -> Float      -- ^ Top
          -> Mat4       -- ^ View matrix
          -> [Object gs is]
          -> Object (View3 ': gs) is
viewOrtho n f l r b t m = view $ m .*. orthoMat4 n f l r b t

-- | Create a group of objects with a view matrix, depending on the size of the
-- framebuffer.
viewVP :: (GLES, ShaderVars gs, ShaderVars is)
       => (Vec2 -> Mat4) -> [Object gs is] -> Object (View3 ': gs) is
viewVP mf o = withFramebufferSize (\s -> View3 -= mf (tupleToVec s))
              :~> mconcat o
        where tupleToVec (w, h) = Vec2 (fromIntegral w) (fromIntegral h)

-- | A 'Layer' with the standard 3D program.
layerS :: IsObject3D gs is => Object gs is -> Layer' s t ()
layerS = layer defaultProgram3D

-- | Translate a 3D Object.
trans :: (MemberGlobal Transform3 gs, GLES) => Vec3
      -> Object gs is -> Object gs is
trans v = transform $ transMat4 v

-- | Rotate a 3D 'Object' around the X axis.
rotX :: (MemberGlobal Transform3 gs, GLES) => Float
     -> Object gs is -> Object gs is
rotX a = transform $ rotXMat4 a

-- | Rotate a 3D 'Object' around the Y axis.
rotY :: (MemberGlobal Transform3 gs, GLES) => Float
     -> Object gs is -> Object gs is
rotY a = transform $ rotYMat4 a

-- | Rotate a 3D 'Object' around the Z axis.
rotZ :: (MemberGlobal Transform3 gs, GLES) => Float
     -> Object gs is -> Object gs is
rotZ a = transform $ rotZMat4 a

-- | Rotate a 3D 'Object' around a vector.
rot :: (MemberGlobal Transform3 gs, GLES) => Vec3
    -> Float
    -> Object gs is -> Object gs is
rot ax ag = transform $ rotMat4 ax ag

-- | Scale a 3D 'Object'.
scale :: (MemberGlobal Transform3 gs, GLES) => Float
      -> Object gs is -> Object gs is
scale f = transform $ scaleMat4 (Vec3 f f f)

-- | Scale a 3D 'Object' in three dimensions.
scaleV :: (MemberGlobal Transform3 gs, GLES) => Vec3
       -> Object gs is -> Object gs is
scaleV v = transform $ scaleMat4 v

-- | Transform a 3D 'Object'.
transform :: (MemberGlobal Transform3 gs, GLES) => Mat4
          -> Object gs is -> Object gs is
transform m' o = (\m -> Transform3 -= m .*. m') ~~> o

-- | 4x4 perspective projection matrix, using width and height instead of the
-- aspect ratio.
perspectiveMat4Size :: Float        -- ^ Near
                    -> Float        -- ^ Far
                    -> Float        -- ^ FOV
                    -> Vec2         -- ^ Viewport size
                    -> Mat4
perspectiveMat4Size n f fov (Vec2 w h) = perspectiveMat4 n f fov $ w / h
