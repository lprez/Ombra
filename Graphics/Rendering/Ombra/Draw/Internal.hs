{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses, KindSignatures,
             GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Ombra.Draw.Internal (
        Draw,
        DrawState,
        drawState,
        drawInit,
        clearBuffers,
        drawLayer,
        drawObject,
        preloadGeometry,
        preloadTexture,
        preloadProgram,
        removeGeometry,
        removeTexture,
        removeProgram,
        textureSize,
        setProgram,
        resizeViewport,
        runDraw,
        execDraw,
        evalDraw,
        gl,
        drawGet
) where

import qualified Graphics.Rendering.Ombra.Blend.Internal as Blend
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Geometry.Internal
import Graphics.Rendering.Ombra.Layer.Internal hiding (clear)
import Graphics.Rendering.Ombra.Object.Internal
import Graphics.Rendering.Ombra.Texture.Internal
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture, Program, Buffer,
                                                    UniformLocation, cullFace,
                                                    depthMask, colorMask)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.GLSL
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.ShaderVar
import qualified Graphics.Rendering.Ombra.Stencil.Internal as Stencil

import Data.Hashable (Hashable)
import Data.Vect.Float
import Data.Word (Word8)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

-- | The state of the 'Draw' monad.
data DrawState = DrawState {
        currentProgram :: Maybe ProgramIndex,
        loadedProgram :: Maybe LoadedProgram,
        programs :: ResMap LoadedProgram,
        uniforms :: ResMap UniformLocation,
        elemBuffers :: ResMap LoadedBuffer,
        attributes :: ResMap LoadedAttribute,
        geometries :: ResMap LoadedGeometry,
        textureImages :: ResMap LoadedTexture,
        activeTextures :: Int,
        viewportSize :: (Int, Int),
        blendMode :: Maybe Blend.Mode,
        stencilMode :: Maybe Stencil.Mode,
        cullFace :: Maybe CullFace,
        depthTest :: Bool,
        depthMask :: Bool,
        colorMask :: (Bool, Bool, Bool, Bool)
}

newtype UniformLocation = UniformLocation GL.UniformLocation

-- | A state monad on top of 'GL'.
newtype Draw a = Draw { unDraw :: StateT DrawState GL a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance EmbedIO Draw where
        embedIO f (Draw a) = Draw get >>= Draw . lift . embedIO f . evalStateT a

-- | Create a 'DrawState'.
drawState :: GLES
          => Int         -- ^ Viewport width
          -> Int         -- ^ Viewport height
          -> IO DrawState
drawState w h = do programs <- newGLResMap
                   elemBuffers <- newGLResMap
                   attributes <- newGLResMap
                   geometries <- newDrawResMap
                   uniforms <- newGLResMap
                   textureImages <- newGLResMap
                   return DrawState { currentProgram = Nothing
                                    , loadedProgram = Nothing
                                    , programs = programs
                                    , elemBuffers = elemBuffers
                                    , attributes = attributes
                                    , geometries = geometries
                                    , uniforms = uniforms
                                    , textureImages = textureImages
                                    , activeTextures = 0
                                    , viewportSize = (w, h)
                                    , blendMode = Nothing
                                    , depthTest = True
                                    , depthMask = True
                                    , stencilMode = Nothing
                                    , cullFace = Nothing
                                    , colorMask = (True, True, True, True)
                                    }

        where newGLResMap :: IO (ResMap r)
              newGLResMap = newResMap
              
              newDrawResMap :: IO (ResMap r)
              newDrawResMap = newResMap

-- | Initialize the render engine.
drawInit :: GLES => Draw ()
drawInit = viewportSize <$> Draw get >>=
           \(w, h) -> gl $ do clearColor 0.0 0.0 0.0 1.0
                              enable gl_DEPTH_TEST
                              depthFunc gl_LESS
                              viewport 0 0 (fromIntegral w) (fromIntegral h)


{-
maxTexs :: (Integral a, GLES) => a
maxTexs = fromIntegral gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
-}

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

-- | Viewport.
resizeViewport :: GLES
               => Int   -- ^ Width.
               -> Int   -- ^ Height.
               -> Draw ()
resizeViewport w h = do gl $ viewport 0 0 (fromIntegral w) (fromIntegral h)
                        Draw . modify $ \s -> s { viewportSize = (w, h) }

clearBuffers :: GLES => [Buffer] -> Draw ()
clearBuffers = mapM_ $ gl . clear . buffer
        where buffer ColorBuffer = gl_COLOR_BUFFER_BIT
              buffer DepthBuffer = gl_DEPTH_BUFFER_BIT
              buffer StencilBuffer = gl_STENCIL_BUFFER_BIT

-- | Manually allocate a 'Geometry' in the GPU.
preloadGeometry :: GLES => Geometry is -> Draw ()
preloadGeometry g = () <$ getGeometry g

-- | Manually allocate a 'Texture' in the GPU.
preloadTexture :: GLES => Texture -> Draw ()
preloadTexture t = () <$ getTexture t

-- | Manually allocate a 'Program' in the GPU.
preloadProgram :: GLES => Program gs is -> Draw ()
preloadProgram p = () <$ getProgram p

-- | Manually delete a 'Geometry' from the GPU. Note that if you try to draw it, it will be allocated again.
removeGeometry :: GLES => Geometry is -> Draw ()
removeGeometry g = removeDrawResource id geometries g

-- | Manually delete a 'Texture' from the GPU.
removeTexture :: GLES => Texture -> Draw ()
removeTexture (TextureImage i) = removeDrawResource gl textureImages i
removeTexture (TextureLoaded l) = gl $ unloadResource
                                        (Nothing :: Maybe TextureImage) l

-- | Manually delete a 'Program' from the GPU.
removeProgram :: GLES => Program gs is -> Draw ()
removeProgram = removeDrawResource gl programs

-- | Draw a 'Layer'.
drawLayer :: GLES => Layer' Drawable t a -> Draw a
drawLayer = fmap fst . flip drawLayer' []

drawLayer' :: GLES
           => Layer' s t a
           -> [TTexture t]
           -> Draw (a, [TTexture t])
drawLayer' (Layer prg grp) ts = do setProgram prg
                                   drawObject grp
                                   return ((), ts)
drawLayer' (TextureLayer drawBufs stypes (w, h) (rx, ry, rw, rh)
                         inspCol inspDepth layer) tts0 =
        do (x, tts1, ts, mcol, mdepth) <-
                layerToTexture drawBufs stypes w h layer
                               (mayInspect inspCol) (mayInspect inspDepth) tts0
           let tts2 = map (TTexture . LoadedTexture gw gh) ts
           return ((x, tts2, mcol, mdepth), tts1 ++ tts2)
        where (gw, gh) = (fromIntegral w, fromIntegral h)
        
              mayInspect :: Bool
                         -> Either (Maybe [r])
                                   ([r] -> Draw (Maybe [r]), Int, Int, Int, Int)
              mayInspect True = Right (return . Just, rx, ry, rw, rh)
              mayInspect False = Left Nothing
drawLayer' (Permanent tt@(TTexture lt)) tts = 
        do let t = TextureLoaded lt
           gl $ unloader t (Nothing :: Maybe TextureImage) lt
           return (t, filter (/= tt) tts)
drawLayer' (WithTTextures ets f) tts =
        do drawLayer . f $ map (\(TTexture lt) -> TextureLoaded lt) ets
           return ((), tts)
drawLayer' (Free layer) tts =
        do (x, tts') <- drawLayer' layer []
           mapM_ (\(TTexture lt) -> removeTexture $ TextureLoaded lt) tts'
           return (x, tts)
drawLayer' (Clear bufs) tts = clearBuffers bufs >> return ((), tts)
drawLayer' (Cast layer) tts =
        do (x, tts') <- drawLayer' layer $ map castTTexture tts
           return (x, map castTTexture tts')
drawLayer' (Bind lx f) tts0 = drawLayer' lx tts0 >>=
                                \(x, tts1) -> drawLayer' (f x) tts1
drawLayer' (Return x) tts = return (x, tts)

-- | Draw an 'Object'.
drawObject :: GLES => Object gs is -> Draw ()
drawObject (g :~> o) = withGlobal g $ drawObject o
drawObject (Mesh g) = withRes_ (getGeometry g) drawGeometry
drawObject NoMesh = return ()
drawObject (Prop p o) = withObjProp p $ drawObject o
drawObject (Append o o') = drawObject o >> drawObject o'

withObjProp :: GLES => ObjProp -> Draw a -> Draw a
withObjProp (Blend m) a = stateReset blendMode setBlendMode m a
withObjProp (Stencil m) a = stateReset stencilMode setStencilMode m a
withObjProp (DepthTest d) a = stateReset depthTest setDepthTest d a
withObjProp (DepthMask d) a = stateReset depthMask setDepthMask d a
withObjProp (ColorMask d) a = stateReset colorMask setColorMask d a
withObjProp (Cull face) a = stateReset cullFace setCullFace face a

stateReset :: (DrawState -> a) -> (a -> Draw ()) -> a -> Draw b -> Draw b
stateReset getOld set new act = do old <- getOld <$> Draw get
                                   set new
                                   b <- act
                                   set old
                                   return b

withGlobal :: GLES => Global g -> Draw () -> Draw ()
withGlobal (Single g c) a = uniform single (g undefined) c >> a
withGlobal (Mirror g c) a = uniform mirror (varBuild (const undefined) g) c >> a
withGlobal (WithTexture t gf) a = withActiveTexture t $ flip withGlobal a . gf
withGlobal (WithTextureSize t gf) a = textureSize t >>= flip withGlobal a . gf
withGlobal (WithFramebufferSize gf) a = viewportSize <$> drawGet >>=
                                        flip withGlobal a . gf

        where tupleToVec (x, y) = Vec2 (fromIntegral x) (fromIntegral y)

uniform :: (GLES, ShaderVar g, Uniform s g)
        => proxy (s :: CPUSetterType *) -> g -> CPU s g -> Draw ()
uniform p g c = withUniforms p g c $
                        \n ug uc -> withRes_ (getUniform $ uniformName g n) $
                                \(UniformLocation l) -> gl $ setUniform l ug uc
                                                                

withActiveTexture :: GLES => Texture -> (ActiveTexture -> Draw ()) -> Draw ()
withActiveTexture tex f =
        withRes (getTexture tex) (return ()) $
                \(LoadedTexture _ _ wtex) -> makeActive tex $
                        \at -> do gl $ bindTexture gl_TEXTURE_2D wtex
                                  f at

makeActive :: GLES => Texture -> (ActiveTexture -> Draw a) -> Draw a
makeActive t f = do atn <- activeTextures <$> Draw get
                    Draw . modify $ \ds -> ds { activeTextures = atn + 1 }
                    gl . activeTexture $ gl_TEXTURE0 + fromIntegral atn
                    ret <- f . ActiveTexture . fromIntegral $ atn
                    Draw . modify $ \ds -> ds { activeTextures = atn }
                    return ret

-- | Get the dimensions of a 'Texture'.
textureSize :: (GLES, Num a) => Texture -> Draw (a, a)
textureSize tex = withRes (getTexture tex) (return (0, 0))
                          $ \(LoadedTexture w h _) -> return ( fromIntegral w
                                                             , fromIntegral h)

-- | Set the program.
setProgram :: GLES => Program g i -> Draw ()
setProgram p = do current <- currentProgram <$> Draw get
                  when (current /= Just (programIndex p)) $
                        withRes_ (getProgram p) $
                                \lp@(LoadedProgram glp _ _) -> do
                                   Draw . modify $ \s -> s {
                                           currentProgram =
                                                   Just $ programIndex p,
                                           loadedProgram = Just lp,
                                           activeTextures = 0
                                   }
                                   gl $ useProgram glp

withRes_ :: Draw (Either String a) -> (a -> Draw ()) -> Draw ()
withRes_ drs = withRes drs $ return ()

withRes :: Draw (Either String a) -> Draw b -> (a -> Draw b) -> Draw b
withRes drs u l = drs >>= \rs -> case rs of
                                      Right r -> l r
                                      _ -> u

getUniform :: GLES => String -> Draw (Either String UniformLocation)
getUniform name = do mprg <- loadedProgram <$> Draw get
                     case mprg of
                          Just prg -> getDrawResource gl uniforms (prg, name)
                          Nothing -> return $ Left "No loaded program."

getGeometry :: GLES => Geometry is -> Draw (Either String LoadedGeometry)
getGeometry = getDrawResource id geometries

getTexture :: GLES => Texture -> Draw (Either String LoadedTexture)
getTexture (TextureLoaded l) = return $ Right l
getTexture (TextureImage t) = getTextureImage t

getTextureImage :: GLES => TextureImage
                -> Draw (Either String LoadedTexture)
getTextureImage = getDrawResource gl textureImages

getProgram :: GLES => Program gs is -> Draw (Either String LoadedProgram)
getProgram = getDrawResource gl programs

-- | Draw a 'Layer' on some textures.
layerToTexture :: (GLES, Integral a)
               => Bool                                  -- ^ Draw buffers
               -> [LayerType]                           -- ^ Textures contents
               -> a                                     -- ^ Width
               -> a                                     -- ^ Height
               -> Layer' s t x                          -- ^ Layer to draw
               -> Either b ( [Color] -> Draw b
                           , Int, Int, Int, Int)        -- ^ Color inspecting
                                                        -- function, start x,
                                                        -- start y, width,
                                                        -- height
               -> Either c ( [Word8] -> Draw c
                           , Int, Int, Int, Int)        -- ^ Depth inspecting,
                                                        -- function, etc.
               -> [TTexture t]
               -> Draw (x, [TTexture t], [GL.Texture], b ,c)
layerToTexture drawBufs stypes wp hp layer einspc einspd tts = do
        (ts, (x, tts', colRes, depthRes)) <-
                renderToTexture drawBufs (map arguments stypes) w h $
                        do (x, tts') <- drawLayer' layer tts
                           colRes <- inspect einspc gl_RGBA wordsToColors 4
                           depthRes <- inspect einspd gl_DEPTH_COMPONENT id 1
                           return (x, tts', colRes, depthRes)

        return (x, tts', ts, colRes, depthRes)

        where (w, h) = (fromIntegral wp, fromIntegral hp)
              arguments stype =
                        case stype of
                              ColorLayer -> ( fromIntegral gl_RGBA
                                            , gl_RGBA
                                            , gl_UNSIGNED_BYTE
                                            , gl_COLOR_ATTACHMENT0
                                            , [ColorBuffer] )
                              DepthLayer -> ( fromIntegral gl_DEPTH_COMPONENT
                                            , gl_DEPTH_COMPONENT
                                            , gl_UNSIGNED_SHORT
                                            , gl_DEPTH_ATTACHMENT
                                            , [DepthBuffer] )
                              DepthStencilLayer -> ( fromIntegral
                                                        gl_DEPTH_STENCIL
                                                   , gl_DEPTH_STENCIL
                                                   , gl_UNSIGNED_INT_24_8
                                                   , gl_DEPTH_STENCIL_ATTACHMENT
                                                   , [ DepthBuffer
                                                     , StencilBuffer]
                                                   )
                              BufferLayer n -> ( fromIntegral gl_RGBA32F
                                               , gl_RGBA
                                               , gl_FLOAT
                                               , gl_COLOR_ATTACHMENT0 + 
                                                 fromIntegral n
                                               , [] )

              inspect :: Either c (a -> Draw c, Int, Int, Int, Int) -> GLEnum
                      -> ([Word8] -> a) -> Int -> Draw c
              inspect (Left r) _ _ _ = return r
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
                => Bool -> [(GLInt, GLEnum, GLEnum, GLEnum, [Buffer])]
                -> GLSize -> GLSize -> Draw a -> Draw ([GL.Texture], a)
renderToTexture drawBufs infos w h act = do
        fb <- gl createFramebuffer 
        gl $ bindFramebuffer gl_FRAMEBUFFER fb

        (ts, attchs, buffersToClear) <- fmap unzip3 . gl . flip mapM infos $
                \(internalFormat, format, pixelType, attachment, buffer) ->
                        do t <- emptyTexture Linear Linear
                           bindTexture gl_TEXTURE_2D t
                           if pixelType == gl_FLOAT
                           then liftIO noFloat32Array >>=
                                            texImage2DFloat gl_TEXTURE_2D 0
                                                            internalFormat w h
                                                            0 format pixelType
                           else liftIO noUInt8Array >>=
                                     texImage2DUInt gl_TEXTURE_2D 0
                                                    internalFormat w h
                                                    0 format pixelType
                           framebufferTexture2D gl_FRAMEBUFFER attachment
                                                gl_TEXTURE_2D t 0
                           return (t, fromIntegral attachment, buffer)

        let buffersToDraw = filter (/= fromIntegral gl_DEPTH_ATTACHMENT) attchs
        when drawBufs $ liftIO (encodeInts buffersToDraw) >>= gl . drawBuffers

        (sw, sh) <- viewportSize <$> Draw get
        resizeViewport (fromIntegral w) (fromIntegral h)

        clearBuffers $ concat buffersToClear
        ret <- act

        resizeViewport sw sh
        gl $ deleteFramebuffer fb

        return (ts, ret)

setBlendMode :: GLES => Maybe Blend.Mode -> Draw ()
setBlendMode Nothing = do m <- blendMode <$> Draw get
                          case m of
                               Just _ -> gl $ disable gl_BLEND
                               Nothing -> return ()
                          Draw . modify $ \s -> s { blendMode = Nothing }
setBlendMode (Just newMode) =
        do mOldMode <- blendMode <$> Draw get
           case mOldMode of
                Nothing -> do gl $ enable gl_BLEND
                              changeColor >> changeEquation >> changeFunction
                Just oldMode ->
                     do when (Blend.constantColor oldMode /= constantColor)
                                changeColor
                        when (Blend.equation oldMode /= equation)
                                changeEquation
                        when (Blend.function oldMode /= function)
                                changeFunction
           Draw . modify $ \s -> s { blendMode = Just newMode }
        where constantColor = Blend.constantColor newMode
              equation@(rgbEq, alphaEq) = Blend.equation newMode
              function@(rgbs, rgbd, alphas, alphad) = Blend.function newMode
              changeColor = case constantColor of
                                 Just (Vec4 r g b a) -> gl $ blendColor r g b a
                                 Nothing -> return ()
              changeEquation = gl $ blendEquationSeparate rgbEq alphaEq
              changeFunction = gl $ blendFuncSeparate rgbs rgbd
                                                      alphas alphad

setStencilMode :: GLES => Maybe Stencil.Mode -> Draw ()
setStencilMode Nothing = do m <- stencilMode <$> Draw get
                            case m of
                                 Just _ -> gl $ disable gl_STENCIL_TEST
                                 Nothing -> return ()
                            Draw . modify $ \s -> s { stencilMode = Nothing }
setStencilMode (Just newMode@(Stencil.Mode newFun newOp)) =
        do mOldMode <- stencilMode <$> Draw get
           case mOldMode of
                Nothing -> do gl $ enable gl_STENCIL_TEST
                              sides newFun changeFunction
                              sides newOp changeOperation
                Just (Stencil.Mode oldFun oldOp) ->
                        do when (oldFun /= newFun) $
                                sides newFun changeFunction
                           when (oldOp /= newOp) $
                                sides newOp changeOperation
           Draw . modify $ \s -> s { stencilMode = Just newMode }
        where changeFunction face f = let (t, v, m) = Stencil.function f
                                      in gl $ stencilFuncSeparate face t v m
              changeOperation face o = let (s, d, n) = Stencil.operation o
                                       in gl $ stencilOpSeparate face s d n
              sides (Stencil.FrontBack x) f = f gl_FRONT_AND_BACK x
              sides (Stencil.Separate x y) f = f gl_FRONT x >> f gl_BACK y

setCullFace :: GLES => Maybe CullFace -> Draw ()
setCullFace Nothing = do old <- cullFace <$> Draw get
                         case old of
                              Just _ -> gl $ disable gl_CULL_FACE
                              Nothing -> return ()
                         Draw . modify $ \s -> s { cullFace = Nothing }
setCullFace (Just newFace) =
        do old <- cullFace <$> Draw get
           when (old == Nothing) . gl $ enable gl_CULL_FACE
           case old of
                Just oldFace | oldFace == newFace -> return ()
                _ -> gl . GL.cullFace $ case newFace of
                                             CullFront -> gl_FRONT
                                             CullBack -> gl_BACK
                                             CullFrontBack -> gl_FRONT_AND_BACK
           Draw . modify $ \s -> s { cullFace = Just newFace }
                   
setDepthTest :: GLES => Bool -> Draw ()
setDepthTest = setFlag depthTest (\x s -> s { depthTest = x })
                       (gl $ enable gl_DEPTH_TEST) (gl $ disable gl_DEPTH_TEST)
                   
setDepthMask :: GLES => Bool -> Draw ()
setDepthMask = setFlag depthMask (\x s -> s { depthMask = x })
                       (gl $ GL.depthMask true) (gl $ GL.depthMask false)

setFlag :: (DrawState -> Bool)
        -> (Bool -> DrawState -> DrawState)
        -> Draw ()
        -> Draw ()
        -> Bool
        -> Draw ()
setFlag getF setF enable disable new =
        do old <- getF <$> Draw get
           case (old, new) of
                   (False, True) -> enable
                   (True, False) -> disable
                   _ -> return ()
           Draw . modify $ setF new

setColorMask :: GLES => (Bool, Bool, Bool, Bool) -> Draw ()
setColorMask new@(r, g, b, a) = do old <- colorMask <$> Draw get
                                   when (old /= new) . gl $
                                           GL.colorMask r' g' b' a'
                                   Draw . modify $ \s -> s { colorMask = new }
        where (r', g', b', a') = (bool r, bool g, bool b, bool a)
              bool True = true
              bool False = false

getDrawResource :: Resource i r m
                => (m (Either String r) -> Draw (Either String r))
                -> (DrawState -> ResMap r)
                -> i
                -> Draw (Either String r)
getDrawResource lft mg i = do
        map <- mg <$> Draw get
        lft $ getResource i map

removeDrawResource :: (Resource i r m, Hashable i)
                   => (m () -> Draw ())
                   -> (DrawState -> ResMap r)
                   -> i
                   -> Draw ()
removeDrawResource lft mg i = do
        s <- mg <$> Draw get
        lft $ removeResource i s

drawGeometry :: GLES => LoadedGeometry -> Draw ()
drawGeometry (LoadedGeometry ec vao) = currentProgram <$> Draw get >>=
        \mcp -> case mcp of
                     Just _ -> gl $ do bindVertexArray vao
                                       drawElements gl_TRIANGLES
                                                    (fromIntegral ec)
                                                    gl_UNSIGNED_SHORT
                                                    nullGLPtr
                                       bindVertexArray noVAO
                     Nothing -> return ()

instance GLES => Resource (LoadedProgram, String) UniformLocation GL where
        loadResource (LoadedProgram prg _ _, g) =
                do loc <- getUniformLocation prg $ toGLString g
                   return . Right $ UniformLocation loc
        unloadResource _ _ = return ()

instance GLES => Resource (Geometry is) LoadedGeometry Draw where
        loadResource = runExceptT .
                       loadGeometry (ExceptT . getDrawResource gl attributes)
                                    (ExceptT . getDrawResource gl elemBuffers)
                                    (lift . gl)
        unloadResource _ = gl . deleteGeometry

-- | Perform a 'GL' action in the 'Draw' monad.
gl :: GL a -> Draw a
gl = Draw . lift

-- | Get the 'DrawState'.
drawGet :: Draw DrawState
drawGet = Draw get
