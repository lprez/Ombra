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
        removeResource
) where

import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import Data.Hashable
import System.Mem.Weak

data ResMap i r = forall m. (Resource i r m, Hashable i) =>
                            ResMap (H.BasicHashTable Int (Either String r))

data ResStatus r = Loaded r
                 | Unloaded
                 | Error String

class (Eq i, Applicative m, EmbedIO m) =>
      Resource i r m | i r -> m where
        loadResource :: i -> m (Either String r)
        unloadResource :: Maybe i -> r -> m ()

class MonadIO m => EmbedIO m where
        embedIO :: (IO a -> IO b) -> m a -> m b

newResMap :: (Hashable i, MonadIO io) => Resource i r m => io (ResMap i r)
newResMap = ResMap <$> liftIO H.new

addResource :: (Resource i r m, Hashable i) => i -> ResMap i r -> m ()
addResource i m = () <$ getResource i m

checkResource :: Hashable i
              => Resource i r m
              => i
              -> ResMap i r
              -> m (ResStatus r)
checkResource i = checkResource' $ hash i

checkResource' :: Resource i r m => Int -> ResMap i r -> m (ResStatus r)
checkResource' i (ResMap map) = do m <- liftIO $ H.lookup map i
                                   return $ case m of
                                                 Just (Right r) -> Loaded r
                                                 Just (Left e) -> Error e
                                                 Nothing ->Unloaded

getResource :: (Resource i r m, Hashable i)
            => i -> ResMap i r
            -> m (Either String r)
getResource i rmap@(ResMap map) =
        do status <- checkResource i rmap
           case status of
                   Unloaded ->
                        do r <- loadResource i

                           liftIO $ case r of
                                         Left s -> H.insert map ihash $ Left s
                                         Right r -> H.insert map ihash $ Right r

                           embedIO (addFinalizer i) $ removeResource' ihash rmap
                           Just eRes <- liftIO . H.lookup map $ hash i
                           return eRes
                   Error s -> return $ Left s
                   Loaded r -> return $ Right r
        where ihash = hash i

-- reloadResource

removeResource :: (Resource i r m, Hashable i) => i -> ResMap i r -> m ()
removeResource i = removeResource' $ hash i

removeResource' :: (Resource i r m, Hashable i) => Int -> ResMap i r -> m ()
removeResource' i rmap@(ResMap map :: ResMap i r) = 
        do status <- checkResource' i rmap
           case status of
                Loaded r -> unloadResource (Nothing :: Maybe i) r
                _ -> return ()
           liftIO $ H.delete map i
