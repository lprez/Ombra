module Graphics.Rendering.Ombra.Stream (
        VertexStream,
        FragmentStream,
        vertexStream,
        rasterize,
        fragmentStream,
        draw
) where

import Graphics.Rendering.Ombra.Internal.GL (gl)
import Graphics.Rendering.Ombra.Geometry.Internal
import Graphics.Rendering.Ombra.Geometry.Types
import Graphics.Rendering.Ombra.Shader
import Graphics.Rendering.Ombra.Shader.GLSL
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Stream.Internal

vertexStream :: (ShaderInput i, GeometryVertex i)
             => VertexShader i v
             -> Geometry i
             -> VertexStream v
vertexStream = VertexStream

rasterize :: ShaderInput v
          => FragmentShader v f
          -> VertexStream (GVec4, v)
          -> FragmentStream f
rasterize = FragmentStream

fragmentStream :: (ShaderInput v, ShaderInput i, GeometryVertex i)
               => VertexShader i (GVec4, v)
               -> FragmentShader v f
               -> Geometry i
               -> FragmentStream f
fragmentStream vs fs = rasterize fs . vertexStream vs

draw :: (MonadGeometry m, MonadProgram m)
     => FragmentStream [GVec4]
     -> m ()
draw (FragmentStream fs (VertexStream vs geometry)) =
        let prg = program vs fs
        in do setProgram prg
              mapM_ (\(uid, (_, set)) -> getUniform uid >>= \eu ->
                        case eu of
                             Right (UniformLocation l) -> gl $ set l
                             Left _ -> return ()
                    ) (uniformList vs ++ uniformList fs)
              drawGeometry geometry
