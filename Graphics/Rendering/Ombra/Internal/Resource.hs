{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
             FunctionalDependencies, ScopedTypeVariables #-}

module Graphics.Rendering.Ombra.Internal.Resource (
        ResMap,
        ResStatus(..),
        Resource(..),
        EmbedIO(..),
        newResMap,
        addResource,
        getResource,
        getResource',
        removeResource
) where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.Functor
import Data.Hashable
import System.Mem.Weak

data ResMap i r = forall m. (Resource i r m, Hashable i) =>
                            ResMap (H.BasicHashTable Int (ResStatus r))

data ResStatus r = Loaded r
                 | Unloaded
                 | forall i m . Resource i r m =>
                         Loading (i, IORef (Either String r -> m ()))
                 | Error String

class (Eq i, Applicative m, EmbedIO m) =>
      Resource i r m | i r -> m where
        loadResource :: i -> (Either String r -> m ()) -> m ()
        unloadResource :: Maybe i -> r -> m ()

class MonadIO m => EmbedIO m where
        embedIO :: (IO a -> IO b) -> m a -> m b

newResMap :: (Hashable i, MonadIO io) => Resource i r m => io (ResMap i r)
newResMap = ResMap <$> liftIO H.new

addResource :: (Resource i r m, Hashable i) => i -> ResMap i r -> m ()
addResource i m = () <$ getResource i m

checkResource :: (Resource i r m, Hashable i)
              => i -> ResMap i r -> m (ResStatus r)
checkResource i = checkResource' $ hash i

checkResource' :: (Resource i r m, Hashable i)
               => Int -> ResMap i r -> m (ResStatus r)
checkResource' i (ResMap map) = do m <- liftIO $ H.lookup map i
                                   case m of
                                         Just status -> return status
                                         Nothing -> return $ Unloaded

getResource :: (Resource i r m, Hashable i)
            => i -> ResMap i r -> m (ResStatus r)
getResource i r = getResource' i r $ const (return ())

getResource' :: (Resource i r m, Hashable i)
             => i -> ResMap i r
             -> (Either String r -> m ())
             -> m (ResStatus r)
getResource' i rmap@(ResMap map) f =
        do status <- checkResource i rmap
           case status of
                   Unloaded ->
                        do loadCbRef <- liftIO . newIORef $
                                \e -> (>> f e) . liftIO $ case e of
                                    Left s -> H.insert map ihash $ Error s
                                    Right r -> H.insert map ihash $ Loaded r

                           liftIO . H.insert map (hash i) $
                                   Loading (i, loadCbRef)

                           loadResource i $
                                   \r -> do f <- liftIO $ readIORef loadCbRef
                                            f r

                           embedIO (addFinalizer i) $ removeResource' ihash rmap
                           Just status' <- liftIO . H.lookup map $ hash i
                           return status'
                   Loaded r -> f (Right r) >> return status
                   _ -> return status
        where ihash = hash i

removeResource :: (Resource i r m, Hashable i) => i -> ResMap i r -> m ()
removeResource i = removeResource' $ hash i

removeResource' :: (Resource i r m, Hashable i) => Int -> ResMap i r -> m ()
removeResource' i rmap@(ResMap map :: ResMap i r) = 
        do status <- checkResource' i rmap
           case status of
                Loaded r -> unloadResource (Nothing :: Maybe i) r
                Loading (ir, loadCbRef) ->
                        liftIO . modifyIORef loadCbRef $
                                \cbRef e ->
                                        do cbRef e
                                           case e of
                                                Right r ->
                                                        unloadResource
                                                          (Just ir) r
                                                Left _ -> return ()
                _ -> return ()
           liftIO $ H.delete map i
