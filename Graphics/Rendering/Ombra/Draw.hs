{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses #-}

module Graphics.Rendering.Ombra.Draw (
        Draw,
        DrawState,
        drawBegin,
        drawLayer,
        drawGroup,
        drawObject,
        drawEnd,
        removeGeometry,
        removeTexture,
        removeProgram,
        textureUniform,
        textureSize,
        setProgram,
        resizeViewport,
        runDraw,
        execDraw,
        evalDraw,
        drawInit,
        gl,
        renderLayer,
        layerToTexture,
        drawState
) where

import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Shapes
import Graphics.Rendering.Ombra.Types
import Graphics.Rendering.Ombra.Texture
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture, Program, UniformLocation)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.GLSL
import Graphics.Rendering.Ombra.Shader.Program

import Data.Bits ((.|.))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Typeable
import Data.Vect.Float
import Data.Word (Word, Word8)
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- | Create a 'DrawState'.
drawInit :: GLES
         => Int         -- ^ Viewport width
         -> Int         -- ^ Viewport height
         -> GL DrawState
drawInit w h =
        do enable gl_DEPTH_TEST
           enable gl_BLEND
           blendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
           clearColor 0.0 0.0 0.0 1.0
           depthFunc gl_LESS
           viewport 0 0 (fromIntegral w) (fromIntegral h)
           programs <- newGLResMap
           gpuBuffers <- newGLResMap
           gpuVAOs <- newDrawResMap
           uniforms <- newGLResMap
           textureImages <- newGLResMap
           return DrawState { currentProgram = Nothing
                            , loadedProgram = Nothing
                            , programs = programs
                            , gpuBuffers = gpuBuffers
                            , gpuVAOs = gpuVAOs
                            , uniforms = uniforms
                            , textureImages = textureImages
                            , activeTextures =
                                    V.replicate maxTexs Nothing
                            , viewportSize = (w, h) }
        where newGLResMap :: (Hashable i, Resource i r GL) => GL (ResMap i r)
              newGLResMap = newResMap
              
              newDrawResMap :: (Hashable i, Resource i r Draw)
                            => GL (ResMap i r)
              newDrawResMap = newResMap


maxTexs :: (Integral a, GLES) => a
maxTexs = 32 -- fromIntegral gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS -- XXX

-- | Run a 'Draw' action.
runDraw :: Draw a
        -> DrawState
        -> GL (a, DrawState)
runDraw (Draw a) = runStateT a

-- | Execute a 'Draw' action.
execDraw :: Draw a              -- ^ Action.
         -> DrawState           -- ^ State.
         -> GL DrawState
execDraw (Draw a) = execStateT a

-- | Evaluate a 'Draw' action.
evalDraw :: Draw a              -- ^ Action.
         -> DrawState           -- ^ State.
         -> GL a
evalDraw (Draw a) = evalStateT a

-- | Get the 'DrawState'.
drawState :: Draw DrawState
drawState = Draw get

-- | Viewport.
resizeViewport :: GLES
       => Int   -- ^ Width.
       -> Int   -- ^ Height.
       -> Draw ()
resizeViewport w h = do gl $ viewport 0 0 (fromIntegral w) (fromIntegral h)
                        Draw . modify $ \s -> s { viewportSize = (w, h) }

-- | Clear the buffers.
drawBegin :: GLES => Draw ()
drawBegin = do freeActiveTextures
               gl . clear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

drawEnd :: GLES => Draw ()
drawEnd = return ()

-- | Delete a 'Geometry' from the GPU (this is automatically done when the
-- 'Geometry' becomes unreachable).
removeGeometry :: GLES => Geometry is -> Draw ()
removeGeometry gi = let g = castGeometry gi in
        do removeDrawResource gl gpuBuffers g
           removeDrawResource id gpuVAOs g

-- | Delete a 'Texture' from the GPU (automatic).
removeTexture :: GLES => Texture -> Draw ()
removeTexture (TextureImage i) = removeDrawResource gl textureImages i
removeTexture (TextureLoaded l) = gl $ unloadResource
                                        (Nothing :: Maybe TextureImage) l

-- | Delete a 'Program' from the GPU (automatic).
removeProgram :: GLES => Program gs is -> Draw ()
removeProgram = removeDrawResource gl programs . castProgram

-- | Draw a 'Layer'.
drawLayer :: GLES => Layer -> Draw ()
drawLayer (Layer prg grp) = setProgram prg >> drawGroup grp
drawLayer (SubLayer rl) =
        do (layers, textures) <- renderLayer rl
           mapM_ drawLayer layers
           mapM_ removeTexture textures
drawLayer (MultiLayer layers) = mapM_ drawLayer layers

-- | Draw a 'Group'.
drawGroup :: GLES => Group gs is -> Draw ()
drawGroup Empty = return ()
drawGroup (Object o) = drawObject o
drawGroup (Global (g := c) o) = c >>= uniform g >> drawGroup o
drawGroup (Append g g') = drawGroup g >> drawGroup g'

-- | Draw an 'Object'.
drawObject :: GLES => Object gs is -> Draw ()
drawObject NoMesh = return ()
drawObject (Mesh g) = withRes_ (getGPUVAOGeometry $ castGeometry g)
                                 drawGPUVAOGeometry
drawObject ((g := c) :~> o) = c >>= uniform g >> drawObject o

uniform :: (GLES, Typeable g, UniformCPU c g) => (a -> g) -> c -> Draw ()
uniform g c = withRes_ (getUniform $ g undefined)
                       $ \(UniformLocation l) -> gl $ setUniform l
                                                                 (g undefined) c

-- | This helps you set the uniforms of type 'Graphics.Rendering.Ombra.Shader.Sampler2D'.
textureUniform :: GLES  => Texture -> Draw ActiveTexture
textureUniform tex = withRes (getTexture tex) (return $ ActiveTexture 0)
                                 $ \(LoadedTexture _ _ wtex) ->
                                        do at <- makeActive tex
                                           gl $ bindTexture gl_TEXTURE_2D wtex
                                           return at

-- | Get the dimensions of a 'Texture'.
textureSize :: (GLES, Num a) => Texture -> Draw (a, a)
textureSize tex = withRes (getTexture tex) (return (0, 0))
                          $ \(LoadedTexture w h _) -> return ( fromIntegral w
                                                             , fromIntegral h)

-- | Set the program.
setProgram :: GLES => Program g i -> Draw ()
setProgram p = do current <- currentProgram <$> Draw get
                  when (current /= Just (castProgram p)) $
                        withRes_ (getProgram $ castProgram p) $
                                \lp@(LoadedProgram glp _ _) -> do
                                   Draw . modify $ \s -> s {
                                           currentProgram = Just $ castProgram p,
                                           loadedProgram = Just lp
                                   }
                                   gl $ useProgram glp

withRes_ :: Draw (ResStatus a) -> (a -> Draw ()) -> Draw ()
withRes_ drs = withRes drs $ return ()

withRes :: Draw (ResStatus a) -> Draw b -> (a -> Draw b) -> Draw b
withRes drs u l = drs >>= \rs -> case rs of
                                        Loaded r -> l r
                                        _ -> u

getUniform :: (Typeable a, GLES) => a -> Draw (ResStatus UniformLocation)
getUniform g = do mprg <- loadedProgram <$> Draw get
                  case mprg of
                          Just prg ->
                                  getDrawResource gl uniforms
                                                  (prg, globalName g)
                          Nothing -> return $ Error "No loaded program."

getGPUVAOGeometry :: GLES => Geometry '[] -> Draw (ResStatus GPUVAOGeometry)
getGPUVAOGeometry = getDrawResource id gpuVAOs

getGPUBufferGeometry :: GLES => Geometry '[] -> Draw (ResStatus GPUBufferGeometry)
getGPUBufferGeometry = getDrawResource gl gpuBuffers

getGPUBufferGeometry' :: GLES
                      => Geometry '[]
                      -> (Either String GPUBufferGeometry -> GL ())
                      -> Draw (ResStatus GPUBufferGeometry)
getGPUBufferGeometry' = getDrawResource' gl gpuBuffers

getTexture :: GLES => Texture -> Draw (ResStatus LoadedTexture)
getTexture (TextureLoaded l) = return $ Loaded l
getTexture (TextureImage t) = getTextureImage t

getTextureImage :: GLES => TextureImage
                -> Draw (ResStatus LoadedTexture)
getTextureImage = getDrawResource gl textureImages

getProgram :: GLES
           => Program '[] '[] -> Draw (ResStatus LoadedProgram)
getProgram = getDrawResource gl programs

freeActiveTextures :: GLES => Draw ()
freeActiveTextures = Draw . modify $ \ds ->
        ds { activeTextures = V.replicate maxTexs Nothing }

-- pretty expensive
makeActive :: GLES => Texture -> Draw ActiveTexture
makeActive t = do ats <- activeTextures <$> Draw get
                  let at@(ActiveTexture atn) =
                        case V.elemIndex (Just t) ats of
                                Just n -> ActiveTexture $ fi n
                                Nothing ->
                                        case V.elemIndex Nothing ats of
                                             Just n -> ActiveTexture $ fi n
                                             -- TODO: Draw () error reporting
                                             Nothing -> ActiveTexture 0
                  gl . activeTexture $ gl_TEXTURE0 + fi atn
                  Draw . modify $ \ds ->
                          ds { activeTextures = ats V.// [(fi atn, Just t)] }
                  return at
        where fi :: (Integral a, Integral b) => a -> b
              fi = fromIntegral


-- | Realize a 'RenderLayer'. It returns the list of allocated 'Texture's so
-- that you can free them if you want.
renderLayer :: GLES => RenderLayer a -> Draw (a, [Texture])
renderLayer (RenderLayer stypes w' h' rx ry rw rh inspCol inspDepth layer f) =
        do (ts, mcol, mdepth) <- layerToTexture stypes w h layer
                                                (mayInspect inspCol)
                                                (mayInspect inspDepth)
           return (f ts mcol mdepth, ts)
        where w = fromIntegral w'
              h = fromIntegral h'

              mayInspect :: Bool
                         -> Either (Maybe [r])
                                   ([r] -> Draw (Maybe [r]), Int, Int, Int, Int)
              mayInspect True = Right (return . Just, rx, ry, rw, rh)
              mayInspect False = Left Nothing

-- | Draw a 'Layer' on some textures.
layerToTexture :: (GLES, Integral a)
               => [LayerType]                           -- ^ Textures contents.
               -> a                                     -- ^ Width
               -> a                                     -- ^ Height
               -> Layer                                 -- ^ Layer to draw
               -> Either b ( [Color] -> Draw b
                           , Int, Int, Int, Int)        -- ^ Color inspecting
                                                        -- function, start x,
                                                        -- start y, width,
                                                        -- height
               -> Either c ( [Word8] -> Draw c
                           , Int, Int, Int, Int)        -- ^ Depth inspecting,
                                                        -- function, etc.
               -> Draw ([Texture], b ,c)
layerToTexture stypes wp hp layer einspc einspd = do
        (ts, (colRes, depthRes)) <- renderToTexture (map arguments stypes) w h $
                        do drawLayer layer
                           colRes <- inspect einspc gl_RGBA wordsToColors 4
                           depthRes <- inspect einspd gl_DEPTH_COMPONENT id 1
                           return (colRes, depthRes)

        return (map (TextureLoaded . LoadedTexture w h) ts, colRes, depthRes)

        where (w, h) = (fromIntegral wp, fromIntegral hp)
              arguments stype =
                        case stype of
                              ColorLayer -> ( fromIntegral gl_RGBA
                                            , gl_RGBA
                                            , gl_UNSIGNED_BYTE
                                            , gl_COLOR_ATTACHMENT0 )
                              DepthLayer -> ( fromIntegral gl_DEPTH_COMPONENT
                                            , gl_DEPTH_COMPONENT
                                            , gl_UNSIGNED_SHORT
                                            , gl_DEPTH_ATTACHMENT )

              inspect :: Either c (a -> Draw c, Int, Int, Int, Int) -> GLEnum
                      -> ([Word8] -> a) -> Int -> Draw c
              inspect (Left r) _ _ s = return r
              inspect (Right (insp, x, y, rw, rh)) format trans s =
                        do arr <- liftIO . newByteArray $
                                        fromIntegral rw * fromIntegral rh * s
                           gl $ readPixels (fromIntegral x)
                                           (fromIntegral y)
                                           (fromIntegral rw)
                                           (fromIntegral rh)
                                           format gl_UNSIGNED_BYTE arr
                           liftIO (decodeBytes arr) >>= insp . trans
              wordsToColors (r : g : b : a : xs) = Color r g b a :
                                                   wordsToColors xs
              wordsToColors _ = []

renderToTexture :: GLES
                => [(GLInt, GLEnum, GLEnum, GLEnum)]
                -> GLSize -> GLSize -> Draw a -> Draw ([GL.Texture], a)
renderToTexture infos w h act = do
        fb <- gl createFramebuffer 
        gl $ bindFramebuffer gl_FRAMEBUFFER fb

        ts <- gl . flip mapM infos $
                \(internalFormat, format, pixelType, attachment) ->
                        do t <- emptyTexture
                           arr <- liftIO $ noArray
                           bindTexture gl_TEXTURE_2D t
                           texImage2D gl_TEXTURE_2D 0 internalFormat w 
                                      h 0 format pixelType arr
                           framebufferTexture2D gl_FRAMEBUFFER attachment
                                                gl_TEXTURE_2D t 0
                           return t

        (sw, sh) <- viewportSize <$> Draw get
        resizeViewport (fromIntegral w) (fromIntegral h)

        drawBegin
        ret <- act
        drawEnd

        resizeViewport sw sh
        gl $ deleteFramebuffer fb

        return (ts, ret)

getDrawResource :: (Resource i r m, Hashable i)
                => (m (ResStatus r) -> Draw (ResStatus r))
                 -> (DrawState -> ResMap i r)
                -> i
                -> Draw (ResStatus r)
getDrawResource lft mg i = getDrawResource' lft mg i $ const (return ())

getDrawResource' :: (Resource i r m, Hashable i)
                => (m (ResStatus r) -> Draw (ResStatus r))
                 -> (DrawState -> ResMap i r)
                 -> i
                 -> (Either String r -> m ())
                 -> Draw (ResStatus r)
getDrawResource' lft mg i f = do
        map <- mg <$> Draw get
        lft $ getResource' i map f

removeDrawResource :: (Resource i r m, Hashable i)
                   => (m () -> Draw ())
                   -> (DrawState -> ResMap i r)
                   -> i
                   -> Draw ()
removeDrawResource lft mg i = do
        s <- mg <$> Draw get
        lft $ removeResource i s

drawGPUVAOGeometry :: GLES => GPUVAOGeometry -> Draw ()
drawGPUVAOGeometry (GPUVAOGeometry _ ec vao) = currentProgram <$> Draw get >>=
        \mcp -> case mcp of
                     Just _ -> gl $ do bindVertexArray vao
                                       drawElements gl_TRIANGLES
                                                    (fromIntegral ec)
                                                    gl_UNSIGNED_SHORT
                                                    nullGLPtr
                                       bindVertexArray noVAO
                     Nothing -> return ()

instance GLES => Resource (LoadedProgram, String) UniformLocation GL where
        loadResource (LoadedProgram prg _ _, g) f =
                do loc <- getUniformLocation prg $ toGLString g
                   f . Right $ UniformLocation loc
        unloadResource _ _ = return ()

instance GLES => Resource (Geometry '[]) GPUVAOGeometry Draw where
        loadResource g f = (>> return ()) . getGPUBufferGeometry' g $
                \ge -> case ge of
                           Left err -> drawInGL . f $ Left err
                           Right buf -> loadResource buf $ drawInGL . f

                where drawInGL = flip evalDraw $
                                error "drawInGL: can't access draw state"
        unloadResource _ =
                gl . unloadResource (Nothing :: Maybe GPUBufferGeometry)

-- | Perform a 'GL' action in the 'Draw' monad.
gl :: GL a -> Draw a
gl = Draw . lift
