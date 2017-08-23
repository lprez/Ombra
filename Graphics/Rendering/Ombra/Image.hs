module Graphics.Rendering.Ombra.Image (
        image,
        image1,
        draw
) where

import Data.Foldable
import Data.Functor.Identity
import Data.Proxy

import Graphics.Rendering.Ombra.Internal.GL (gl)
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.GLSL
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.Types
import Graphics.Rendering.Ombra.Image.Types
import Graphics.Rendering.Ombra.Texture.Draw

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
                let (unisv, texsv) = uniformList (vs $ return vu)
                    (unisf, texsf) = uniformList (fs $ return fu)
                    unis = unisv ++ unisf
                    texs = texsv ++ texsf
                in withActiveTextures texs (const $ return ()) $ \samplers ->
                        withUniforms unis
                                     (zip texs samplers)
                                     (drawGeometry geometry)
                     
        where withUniforms unis texs a = (>> a) .
                for_ unis $ \(uid, uniformValue) ->
                        getUniform uid >>= \eu ->
                                case eu of
                                     Right (UniformLocation l) ->
                                        case uniformValue of
                                             UniformValue proxy value ->
                                                gl $ setUniform l proxy value
                                             UniformTexture tex ->
                                                let proxy = Proxy
                                                        :: Proxy GSampler2D
                                                    Just value = lookup tex texs
                                                in gl $ setUniform l proxy value
                                     Left _ -> return ()

draw (SeqImage i i') = draw i >> draw i'
