module Graphics.Rendering.Ombra.Image (
        image,
        image1,
        draw
) where

import Data.Foldable
import Data.Functor.Identity

import Graphics.Rendering.Ombra.Internal.GL (gl)
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.GLSL
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Image.Types

image :: (ShaderInput i, GeometryVertex i, ShaderInput v)
      => VertexShader i (GVec4, v)
      -> FragmentShader v o
      -> Geometry i
      -> Image o
image vs fs g = image1 (const vs) (const fs) (Identity (g, (), ()))

image1 :: (ShaderInput i, GeometryVertex i, ShaderInput v, Foldable t)
       => (UniformSetter vu -> VertexShader i (GVec4, v))
       -> (UniformSetter fu -> FragmentShader v o)
       -> t (Geometry i, vu, fu)
       -> Image o
image1 vs fs g = Image g vs fs

draw :: (MonadDraw o m, FragmentShaderOutput o) => Image o -> m ()
draw (Image geometries vs fs) =
        let prg = program (vs undefined) (fs undefined)
            -- XXX
        in do setProgram prg
              for_ geometries $ \(geometry, vu, fu) ->
                do let unis = uniformList (vs $ return vu) ++
                              uniformList (fs $ return fu)
                   for_ unis $ \(uid, (_, set)) -> getUniform uid >>= \eu ->
                           case eu of
                                Right (UniformLocation l) -> gl $ set l
                                Left _ -> return ()
                   drawGeometry geometry
draw (SeqImage i i') = draw i >> draw i'
