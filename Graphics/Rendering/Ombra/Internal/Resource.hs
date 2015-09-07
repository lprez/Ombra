{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
             FunctionalDependencies #-}

module Graphics.Rendering.Ombra.Internal.Resource (
        ResMap,
        ResStatus(..),
        Resource(..),
        newResMap,
        addResource,
        getResource,
        getResource',
        removeResource
) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Hashable

data ResMap i r = forall m. (Resource i r m, Hashable i) =>
                            ResMap (H.HashMap i (IORef (ResStatus r)))

data ResStatus r = Loaded r | Unloaded | Loading | Error String

class (Eq i, Applicative m, MonadIO m) =>
      Resource i r m | i r -> m where
        loadResource :: i -> (Either String r -> m ()) -> m ()
        unloadResource :: Maybe i -> r -> m ()

newResMap :: Hashable i => Resource i r m => ResMap i r
newResMap = ResMap H.empty

addResource :: (Resource i r m, Hashable i) => i -> ResMap i r -> m (ResMap i r)
addResource i m = snd <$> getResource i m

checkResource :: (Resource i r m, Hashable i)
              => i -> ResMap i r -> m (ResStatus r)
checkResource i (ResMap map) = case H.lookup i map of
                                        Just ref -> liftIO $ readIORef ref
                                        Nothing -> return $ Unloaded

getResource :: (Resource i r m, Hashable i)
            => i -> ResMap i r -> m (ResStatus r, ResMap i r)
getResource i r = getResource' i r $ const (return ())

getResource' :: (Resource i r m, Hashable i)
             => i -> ResMap i r
             -> (Either String r -> m ())
             -> m (ResStatus r, ResMap i r)
getResource' i rmap@(ResMap map) f =
        do status <- checkResource i rmap
           case status of
                   Unloaded ->
                        do ref <- liftIO $ newIORef Loading
                           loadResource i $ \e -> f e >> case e of
                                 Left s -> liftIO . writeIORef ref $ Error s
                                 Right r -> liftIO . writeIORef ref $ Loaded r
                           status' <- liftIO $ readIORef ref
                           return (status', ResMap $ H.insert i ref map)
                   Loaded r -> f (Right r) >> return (status, rmap)
                   _ -> return (status, rmap)

removeResource :: (Resource i r m, Hashable i)
               => i -> ResMap i r -> m (Bool, ResMap i r)
removeResource i rmap@(ResMap map) = 
        do status <- checkResource i rmap
           res <- case status of
                       Loaded r -> unloadResource (Just i) r >> return True
                       Loading -> return False
                       _ -> return True
           return (res, ResMap $ H.delete i map)
