{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses, KindSignatures,
             GeneralizedNewtypeDeriving, RankNTypes, TypeOperators, CPP,
             ScopedTypeVariables, UndecidableInstances, TypeFamilies #-}

module Graphics.Rendering.Ombra.Draw.Monad (
        Draw,
        DrawState,
        ResStatus(..),
        Buffer(..),
        drawState,
        drawInit,
        clearColor,
        clearDepth,
        clearStencil,
        preloadGeometry,
        preloadTexture,
        preloadProgram,
        removeGeometry,
        removeTexture,
        removeProgram,
        checkGeometry,
        checkTexture,
        checkProgram,
        textureSize,
        setProgram,
        resizeViewport,
        evalDraw,
        gl,
        drawGet
) where

import qualified Graphics.Rendering.Ombra.Blend.Draw as Blend
import qualified Graphics.Rendering.Ombra.Blend.Types as Blend
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Culling.Draw
import Graphics.Rendering.Ombra.Culling.Types
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Geometry.Draw
import Graphics.Rendering.Ombra.OutBuffer.Types
import Graphics.Rendering.Ombra.Texture
import Graphics.Rendering.Ombra.Texture.Draw
import Graphics.Rendering.Ombra.Backend (GLES)
import qualified Graphics.Rendering.Ombra.Backend as GL
import Graphics.Rendering.Ombra.Internal.GL hiding (Texture, Program, Buffer,
                                                    UniformLocation, cullFace,
                                                    depthMask, colorMask,
                                                    drawBuffers, clearColor,
                                                    clearDepth, clearStencil)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Screen
import Graphics.Rendering.Ombra.Shader.Language.Types
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.Types
import qualified Graphics.Rendering.Ombra.Stencil.Draw as Stencil
import qualified Graphics.Rendering.Ombra.Stencil.Types as Stencil
import Graphics.Rendering.Ombra.Vector

import Data.Hashable
import Data.Proxy
import Data.Word
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- | The state of the 'Draw' monad.
data DrawState = DrawState
        { currentFrameBuffer :: FrameBuffer
        , currentProgram :: Maybe ProgramIndex
        , loadedProgram :: Maybe LoadedProgram
        , programs :: ResMap LoadedProgram
        , uniforms :: ResMap UniformLocation
        , elemBuffers :: ResMap LoadedBuffer
        , attributes :: ResMap LoadedAttribute
        , geometries :: ResMap LoadedGeometry
        , textureImages :: ResMap LoadedTexture
        , activeTextures :: Int
        -- , textureCache :: [LoadedTexture]
        , viewportSize :: ((Int, Int), (Int, Int))
        , blendMode :: Maybe Blend.Mode
        , stencilMode :: Maybe Stencil.Mode
        , cullFace :: Maybe CullFace
        , depthTest :: Bool
        , depthMask :: Bool
        , colorMask :: (Bool, Bool, Bool, Bool)
        }

data Buffer = ColorBuffer
            | DepthBuffer
            | StencilBuffer

-- | An implementation of 'MonadDraw' and 'MonadDrawBuffers'.
newtype Draw o a = Draw { unDraw :: StateT DrawState GL a }
        deriving ( Functor
                 , Applicative
                 , Monad
                 , MonadIO
                 , MonadBase IO
#if __GLASGOW_HASKELL__ >= 802
                 , MonadBaseControl IO
                 )
#else
                 )

instance MonadBaseControl IO (Draw o) where
        type StM (Draw o) a = ComposeSt (StateT DrawState) GL a
        liftBaseWith f = Draw $ liftBaseWith $ \tf -> f (tf . unDraw)
        restoreM = Draw . restoreM
#endif

instance (FragmentShaderOutput o, GLES) => MonadDraw o Draw where
        withColorMask m a = stateReset colorMask setColorMask m a
        withDepthTest d a = stateReset depthTest setDepthTest d a
        withDepthMask m a = stateReset depthMask setDepthMask m a

instance GLES => MonadDrawBuffers Draw where
        {-
        drawBuffers w h gBuffer depthBuffer draw cont =
                do (ret, (newGBuffer, gBuffer'), (newDepthBuffer, depthBuffer'))
                        <- permanentDrawBuffers w h gBuffer depthBuffer draw
                   ret' <- cont gBuffer' depthBuffer' ret
                   when newGBuffer $ unusedTextures (textures gBuffer')
                   when newDepthBuffer $ unusedTextures (textures depthBuffer')
                   return ret'
        -}
        createBuffers w h gBufferInfo depthBufferInfo draw =
                do (ret, gBuffer, depthBuffer) <-
                        drawBuffers' w h
                                     True
                                     (Right gBufferInfo)
                                     (Right depthBufferInfo)
                                     draw
                   return (ret, BufferPair gBuffer depthBuffer)
        createGBuffer gBufferInfo depthBuffer draw =
                do let (w, h) = bufferSize depthBuffer
                   (ret, gBuffer, _) <-
                        drawBuffers' w h
                                     True
                                     (Right gBufferInfo)
                                     (Left depthBuffer)
                                     draw
                   return (ret, BufferPair gBuffer depthBuffer)
        createDepthBuffer gBuffer depthBufferInfo draw =
                do let (w, h) = bufferSize gBuffer
                   (ret, _, depthBuffer) <-
                        drawBuffers' w h
                                     True
                                     (Left gBuffer)
                                     (Right depthBufferInfo)
                                     draw
                   return (ret, BufferPair gBuffer depthBuffer)
        drawBuffers (BufferPair gBuffer depthBuffer) draw =
                do let (w, h) = bufferSize gBuffer
                   (ret, _, _) <- drawBuffers' w h
                                               True
                                               (Left gBuffer)
                                               (Left depthBuffer)
                                               draw
                   return ret

instance GLES => MonadRead GVec4 Draw where
        readColor = flip readPixels gl_RGBA
        readColorFloat = flip readPixels gl_RGBA
        readDepth = flip readPixels gl_DEPTH_COMPONENT
        readDepthFloat = flip readPixels gl_DEPTH_COMPONENT
        readStencil = flip readPixels gl_STENCIL_INDEX

instance GLES => MonadScreen (Draw o) where
        currentViewport = viewportSize <$> Draw get
        resizeViewport p w = do setViewport p w
                                Draw . modify $ \s ->
                                        s { viewportSize = (p, w) }

instance GLES => MonadProgram (Draw o) where
        withProgram p act =
                do current <- currentProgram <$> Draw get
                   when (current /= Just (programIndex p)) $
                           getProgram p >>= \elp ->
                                case elp of
                                     Right lp -> do Draw . modify $ \s ->
                                                     s { currentProgram = Just $
                                                             programIndex p
                                                       , loadedProgram = Just lp
                                                       , activeTextures = 0
                                                       }
                                                    act lp
                                     Left _ -> return ()
        getUniform id = do mprg <- loadedProgram <$> Draw get
                           case mprg of
                                Just prg -> do map <- uniforms <$> Draw get
                                               gl $ getResource' (Just prg)
                                                                 (prg, id)
                                                                 map
                                Nothing -> return $ Left "No loaded program."

instance GLES => MonadCulling (Draw o) where
        withCulling face a = stateReset cullFace setCullFace face a

instance GLES => Blend.MonadBlend (Draw o) where
        withBlendMode m a = stateReset blendMode setBlendMode m a

instance GLES => Stencil.MonadStencil (Draw o) where
        withStencilMode m a = stateReset stencilMode setStencilMode m a

instance GLES => MonadTexture (Draw o) where
        getTexture (TextureLoaded l) = return $ Right l
        getTexture (TextureImage t) = getTextureImage t
        getActiveTexturesCount = activeTextures <$> Draw get
        setActiveTexturesCount n = Draw . modify  $ \s ->
                                        s { activeTextures = n }
        newTexture w h params i initialize =
                gl $ do t <- emptyTexture params
                        initialize t
                        return $ LoadedTexture w' h' i t
                {-
                do cache <- textureCache <$> Draw get
                   let (c1, c2) = flip break cache $
                                        \(LoadedTexture cw ch i' t) ->
                                                w' == cw && h' == ch && i == i'
                   case c2 of
                        [] -> gl $ do t <- emptyTexture params
                                      initialize t
                                      return $ LoadedTexture w' h' i t
                        (lt : c2') -> do Draw . modify $ \s ->
                                                s { textureCache = c1 ++ c2' }
                                         return lt
                -}
                where (w', h') = (fromIntegral w, fromIntegral h)
        {-
        unusedTextures ts =
                do cache <- textureCache <$> Draw get
                   let (cache', excess) = splitAt textureCacheMaxSize
                                                  (ts ++ cache)
                   Draw . modify $ \s -> s { textureCache = cache' }
                   mapM_ (removeTexture . TextureLoaded) excess
        -}

instance GLES => MonadGeometry (Draw o) where
        getAttribute = getDrawResource gl attributes
        getElementBuffer = getDrawResource gl elemBuffers
        getGeometry = getDrawResource id geometries

instance MonadGL (Draw o) where
        gl = Draw . lift

-- | Create a 'DrawState'.
drawState :: GLES
          => Int         -- ^ Viewport width
          -> Int         -- ^ Viewport height
          -> DrawState
drawState w h = DrawState { currentFrameBuffer = noFramebuffer
                          , currentProgram = Nothing
                          , loadedProgram = Nothing
                          -- , textureCache = []
                          , activeTextures = 0
                          , viewportSize = ((0, 0), (w, h))
                          , blendMode = Nothing
                          , depthTest = True
                          , depthMask = True
                          , stencilMode = Nothing
                          , cullFace = Nothing
                          , colorMask = (True, True, True, True)
                          , programs = err
                          , elemBuffers = err
                          , attributes = err
                          , geometries = err
                          , uniforms = err
                          , textureImages = err
                          }
        where err = error "Call drawInit first"

-- | Initialize the render engine.
drawInit :: GLES => Draw GVec4 ()
drawInit = do programs <- liftIO newResMap
              elemBuffers <- liftIO newResMap
              attributes <- liftIO newResMap
              geometries <- liftIO newResMap
              uniforms <- liftIO newResMap
              textureImages <- liftIO newResMap

              ((x, y), (w, h)) <- viewportSize <$> Draw get
              gl $ do GL.clearColor 0.0 0.0 0.0 1.0
                      enable gl_DEPTH_TEST
                      depthFunc gl_LESS
                      viewport (fromIntegral x) (fromIntegral y)
                               (fromIntegral w) (fromIntegral h)

              Draw . modify $ \s -> s { programs = programs
                                      , elemBuffers = elemBuffers
                                      , attributes = attributes
                                      , geometries = geometries
                                      , uniforms = uniforms
                                      , textureImages = textureImages
                                      }

{-
maxTexs :: (Integral a, GLES) => a
maxTexs = fromIntegral gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS
-}

{-
-- | Run a 'Draw' action.
runDraw :: Draw GVec4 a
        -> DrawState
        -> GL (a, DrawState)
runDraw (Draw a) = runStateT a

-- | Execute a 'Draw' action.
execDraw :: Draw GVec4 a
         -> DrawState
         -> GL DrawState
execDraw (Draw a) = execStateT a
-}

-- | Evaluate a 'Draw' action.
evalDraw :: Draw GVec4 a
         -> DrawState
         -> GL a
evalDraw (Draw a) = evalStateT a

left :: Either String a -> Maybe String
left (Left x) = Just x
left _ = Nothing

-- | Manually allocate a 'Geometry' in the GPU. Eventually returns an error
-- string.
preloadGeometry :: (GLES, GeometryVertex g)
                => Geometry g
                -> Draw o (Maybe String)
preloadGeometry g = left <$> getGeometry g

-- | Manually allocate a 'Texture' in the GPU.
preloadTexture :: GLES => Texture -> Draw o (Maybe String)
preloadTexture t = left <$> getTexture t

-- | Manually allocate a 'Program' in the GPU.
preloadProgram :: GLES => Program gs is -> Draw o (Maybe String)
preloadProgram p = left <$> getProgram p

-- | Manually delete a 'Geometry' from the GPU.
removeGeometry :: (GLES, GeometryVertex g) => Geometry g -> Draw o ()
removeGeometry g = removeDrawResource id geometries g

-- | Manually delete a 'Texture' from the GPU.
removeTexture :: GLES => Texture -> Draw o ()
removeTexture (TextureImage i) = removeDrawResource gl textureImages i
removeTexture (TextureLoaded l) = gl $ unloadResource
                                        (Nothing :: Maybe TextureImage) l

-- | Manually delete a 'Program' from the GPU.
removeProgram :: GLES => Program gs is -> Draw o ()
removeProgram = removeDrawResource gl programs

-- | Check if a 'Geometry' failed to load.
checkGeometry :: (GLES, GeometryVertex g)
              => Geometry g
              -> Draw o (ResStatus ())
checkGeometry g = fmap (const ()) <$> checkDrawResource id geometries g

-- | Check if a 'Texture' failed to load. Eventually returns the texture width
-- and height.
checkTexture :: (GLES, Num a) => Texture -> Draw o (ResStatus (a, a))
checkTexture (TextureImage i) =
        fmap loadedTextureSize <$> checkDrawResource gl textureImages i
checkTexture (TextureLoaded l) = return $ Loaded (loadedTextureSize l)

loadedTextureSize :: (GLES, Num a) => LoadedTexture -> (a, a)
loadedTextureSize (LoadedTexture w h _ _) = (fromIntegral w, fromIntegral h)

-- | Check if a 'Program' failed to load.
checkProgram :: GLES => Program gs is -> Draw o (ResStatus ())
checkProgram p = fmap (const ()) <$> checkDrawResource gl programs p

stateReset :: (DrawState -> a)
           -> (a -> Draw o ())
           -> a
           -> Draw o b
           -> Draw o b
stateReset getOld set new act = do old <- getOld <$> Draw get
                                   set new
                                   b <- act
                                   set old
                                   return b

getTextureImage :: GLES => TextureImage
                -> Draw o (Either String LoadedTexture)
getTextureImage = getDrawResource gl textureImages

getProgram :: GLES => Program gs is -> Draw o (Either String LoadedProgram)
getProgram = getDrawResource' gl programs Nothing

setBlendMode :: GLES => Maybe Blend.Mode -> Draw o ()
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

setStencilMode :: GLES => Maybe Stencil.Mode -> Draw o ()
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

setCullFace :: GLES => Maybe CullFace -> Draw o ()
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
                   
setDepthTest :: GLES => Bool -> Draw o ()
setDepthTest = setFlag depthTest (\x s -> s { depthTest = x })
                       (gl $ enable gl_DEPTH_TEST) (gl $ disable gl_DEPTH_TEST)
                   
setDepthMask :: GLES => Bool -> Draw o ()
setDepthMask = setFlag depthMask (\x s -> s { depthMask = x })
                       (gl $ GL.depthMask true) (gl $ GL.depthMask false)

setFlag :: (DrawState -> Bool)
        -> (Bool -> DrawState -> DrawState)
        -> Draw o ()
        -> Draw o ()
        -> Bool
        -> Draw o ()
setFlag getF setF enable disable new =
        do old <- getF <$> Draw get
           case (old, new) of
                   (False, True) -> enable
                   (True, False) -> disable
                   _ -> return ()
           Draw . modify $ setF new

setColorMask :: GLES => (Bool, Bool, Bool, Bool) -> Draw o ()
setColorMask new@(r, g, b, a) = do old <- colorMask <$> Draw get
                                   when (old /= new) . gl $
                                           GL.colorMask r' g' b' a'
                                   Draw . modify $ \s -> s { colorMask = new }
        where (r', g', b', a') = (bool r, bool g, bool b, bool a)
              bool True = true
              bool False = false

getDrawResource :: Resource i r m
                => (m (Either String r) -> Draw o (Either String r))
                -> (DrawState -> ResMap r)
                -> i
                -> Draw o (Either String r)
getDrawResource lft mg i = do
        map <- mg <$> Draw get
        lft $ getResource i map

getDrawResource' :: Resource i r m
                => (m (Either String r) -> Draw o (Either String r))
                -> (DrawState -> ResMap r)
                -> Maybe k
                -> i
                -> Draw o (Either String r)
getDrawResource' lft mg k i = do
        map <- mg <$> Draw get
        lft $ getResource' k i map

checkDrawResource :: Resource i r m
                  => (m (ResStatus r) -> Draw o (ResStatus r))
                  -> (DrawState -> ResMap r)
                  -> i
                  -> Draw o (ResStatus r)
checkDrawResource lft mg i = do
        map <- mg <$> Draw get
        lft $ checkResource i map

removeDrawResource :: (Resource i r m, Hashable i)
                   => (m () -> Draw o ())
                   -> (DrawState -> ResMap r)
                   -> i
                   -> Draw o ()
removeDrawResource lft mg i = do
        s <- mg <$> Draw get
        lft $ removeResource i s

textureCacheMaxSize :: Num a => a
textureCacheMaxSize = 16

-- | Clear the color buffer.
clearColor :: (GLES, MonadGL m) => m ()
clearColor = clearBuffers [ColorBuffer]

-- | Clear the depth buffer.
clearDepth :: (GLES, MonadGL m) => m ()
clearDepth = clearBuffers [DepthBuffer]

-- | Clear the stencil buffer.
clearStencil :: (GLES, MonadGL m) => m ()
clearStencil = clearBuffers [StencilBuffer]

clearBuffers :: (GLES, MonadGL m) => [Buffer] -> m ()
clearBuffers = mapM_ $ gl . GL.clear . buffer
        where buffer ColorBuffer = gl_COLOR_BUFFER_BIT
              buffer DepthBuffer = gl_DEPTH_BUFFER_BIT
              buffer StencilBuffer = gl_STENCIL_BUFFER_BIT

createOutBuffer :: forall m o. (GLES, MonadTexture m)
                => Int
                -> Int
                -> OutBufferInfo o
                -> m (OutBuffer o)
createOutBuffer w h empty = 
        do let loader t = do bindTexture gl_TEXTURE_2D t
                             if pixelType == gl_FLOAT
                             then liftIO noFloat32Array >>=
                                          texImage2DFloat gl_TEXTURE_2D 0
                                                          internalFormat w' h'
                                                          0 format pixelType
                             else liftIO noUInt8Array >>=
                                          texImage2DUInt gl_TEXTURE_2D 0
                                                         internalFormat w' h'
                                                         0 format pixelType

           textures <- replicateM (fromIntegral texNum)
                                  (newTexture w h params cacheIdentifier loader)
           return $ case empty of
                         EmptyFloatGBuffer _ -> TextureFloatGBuffer w h textures
                         EmptyByteGBuffer _ -> TextureByteGBuffer w h textures
                         EmptyDepthBuffer _ ->
                                 TextureDepthBuffer w h $ head textures
                         EmptyDepthStencilBuffer _ ->
                                 TextureDepthStencilBuffer w h $ head textures
        where (w', h') = (fromIntegral w, fromIntegral h)
              cacheIdentifier = hash ( fromIntegral internalFormat :: Int
                                     , fromIntegral format :: Int
                                     , fromIntegral pixelType :: Int
                                     , params
                                     )
              (internalFormat, format, pixelType, params, texNum) =
                      case empty of
                           EmptyByteGBuffer params ->
                                   ( fromIntegral gl_RGBA
                                   , gl_RGBA
                                   , gl_UNSIGNED_BYTE
                                   , params
                                   , textureCount (Proxy :: Proxy o)
                                   )
                           EmptyFloatGBuffer params ->
                                   ( fromIntegral gl_RGBA32F
                                   , gl_RGBA
                                   , gl_FLOAT
                                   , params
                                   , textureCount (Proxy :: Proxy o)
                                   )
                           EmptyDepthBuffer params ->
                                   ( fromIntegral gl_DEPTH_COMPONENT
                                   , gl_DEPTH_COMPONENT
                                   , gl_UNSIGNED_SHORT
                                   , params
                                   , 1
                                   )
                           EmptyDepthStencilBuffer params ->
                                   ( fromIntegral gl_DEPTH_STENCIL
                                   , gl_DEPTH_STENCIL
                                   , gl_UNSIGNED_INT_24_8
                                   , params
                                   , 1
                                   )

drawBuffers' :: GLES
             => Int
             -> Int
             -> Bool
             -> Either (GBuffer o) (GBufferInfo o)
             -> Either DepthBuffer DepthBufferInfo
             -> Draw o a
             -> Draw o' (a, GBuffer o, DepthBuffer)
drawBuffers' w h addUnloader gBuffer depthBuffer draw =
        do (newColor, gBuffer') <-
                case gBuffer of
                     Right b -> (,) True <$> createOutBuffer w h b
                     Left b -> return (False, b)
           (newDepth, shouldClearStencil, depthBuffer') <-
                case depthBuffer of
                     Right b@(EmptyDepthBuffer _) ->
                             (,,) True False <$> createOutBuffer w h b
                     Right b@(EmptyDepthStencilBuffer _) ->
                             (,,) True True <$> createOutBuffer w h b
                     Left b -> return (False, False, b)
           ret <- drawUsedBuffers w h gBuffer' depthBuffer' $
                   do when newColor clearColor
                      when newDepth clearDepth
                      when shouldClearStencil clearStencil
                      draw
           gl $ do when (addUnloader && newColor) $ bufferUnloader gBuffer'
                   when (addUnloader && newDepth) $ bufferUnloader depthBuffer'
           return (ret, gBuffer', depthBuffer')
        where bufferUnloader buf = 
                        mapM_ (unloader buf (Nothing :: Maybe TextureImage))
                              (textures buf)

drawUsedBuffers :: GLES
                => Int
                -> Int
                -> GBuffer o
                -> DepthBuffer
                -> Draw o a
                -> Draw o' a
drawUsedBuffers w h gBuffer depthBuffer draw =
        do oldFb <- currentFrameBuffer <$> Draw get
           ret <- drawToTextures useDrawBuffers attachments w h oldFb $ \fb -> 
                do Draw . modify $ \s -> s { currentFrameBuffer = fb }
                   castDraw draw
           Draw . modify $ \s -> s { currentFrameBuffer = oldFb }
           return ret
        where colorAttachments = zipWith (\(LoadedTexture _ _ _ t) n ->
                                                (t, gl_COLOR_ATTACHMENT0 + n)
                                         )
                                         (textures gBuffer)
                                         [0 ..]
              depthAttachment =
                      case depthBuffer of
                           TextureDepthBuffer _ _ (LoadedTexture _ _ _ t) ->
                                (t, gl_DEPTH_ATTACHMENT)
                           TextureDepthStencilBuffer _ _ (LoadedTexture _ _ _ t) ->
                                (t, gl_DEPTH_STENCIL_ATTACHMENT)
              attachments = depthAttachment : colorAttachments
              useDrawBuffers | (_ : _ : _) <- colorAttachments = True
                             | otherwise = False

drawToTextures :: (GLES, MonadScreen m, MonadGL m)
               => Bool
               -> [(GL.Texture, GLEnum)]
               -> Int
               -> Int
               -> FrameBuffer
               -> (FrameBuffer -> m a)
               -> m a
drawToTextures useDrawBuffers atts w h oldFb draw =
        do fb <- gl createFramebuffer 
           gl $ bindFramebuffer gl_FRAMEBUFFER fb

           buffersToDraw <- fmap concat . flip mapM atts $
                   \(t, attach) ->
                           do let drawAttachment =
                                   [ fromIntegral attach
                                   | attach /= gl_DEPTH_ATTACHMENT
                                   , attach /= gl_DEPTH_STENCIL_ATTACHMENT
                                   ]
                                        
                              gl $ framebufferTexture2D gl_FRAMEBUFFER attach
                                                        gl_TEXTURE_2D t 0
                              return drawAttachment

           when useDrawBuffers $
                   liftIO (encodeInts buffersToDraw) >>= gl . GL.drawBuffers

           (sp, ss) <- currentViewport
           resizeViewport (0, 0) (fromIntegral w, fromIntegral h)

           ret <- draw fb

           resizeViewport sp ss
           gl $ do deleteFramebuffer fb
                   bindFramebuffer gl_FRAMEBUFFER oldFb

           return ret

class ReadPixels r where
        readPixels :: MonadGL m => (Int, Int, Int, Int) -> GLEnum -> m r

instance GLES => ReadPixels [Color] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newUInt8Array $
                                        fromIntegral rw * fromIntegral rh * 4
                           gl $ readPixelsUInt8 (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_UNSIGNED_BYTE arr
                           liftIO $ fmap wordsToColors (decodeUInt8s arr)
                where wordsToColors (r : g : b : a : xs) =
                                Color r g b a : wordsToColors xs
                      wordsToColors _ = []

instance GLES => ReadPixels [Vec4] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newFloat32Array $
                                        fromIntegral rw * fromIntegral rh * 4
                           gl $ readPixelsFloat (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_FLOAT arr
                           liftIO $ fmap floatsToVecs (decodeFloat32s arr)
                where floatsToVecs (r : g : b : a : xs) =
                                Vec4 r g b a : floatsToVecs xs
                      floatsToVecs _ = []

instance GLES => ReadPixels [Word8] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newUInt8Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsUInt8 (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_UNSIGNED_BYTE arr
                           liftIO $ decodeUInt8s arr

instance GLES => ReadPixels [Word16] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newUInt16Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsUInt16 (fromIntegral x)
                                                 (fromIntegral y)
                                                 (fromIntegral rw)
                                                 (fromIntegral rh)
                                                 format gl_UNSIGNED_SHORT arr
                           liftIO $ decodeUInt16s arr

instance GLES => ReadPixels [Float] where
        readPixels (x, y, rw, rh) format =
                        do arr <- liftIO . newFloat32Array $
                                        fromIntegral rw * fromIntegral rh
                           gl $ readPixelsFloat (fromIntegral x)
                                                (fromIntegral y)
                                                (fromIntegral rw)
                                                (fromIntegral rh)
                                                format gl_FLOAT arr
                           liftIO $ decodeFloat32s arr

castDraw :: Draw o a -> Draw o' a
castDraw (Draw x) = Draw x


-- | Get the 'DrawState'.
drawGet :: Draw o DrawState
drawGet = Draw get
