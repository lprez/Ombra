{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, TypeFamilies,
             FunctionalDependencies, ScopedTypeVariables, FlexibleContexts #-}

module Graphics.Rendering.Ombra.Internal.Resource (
        ResMap,
        ResStatus(..),
        Resource(..),
        EmbedIO(..),
        newResMap,
        addResource,
        getResource,
        checkResource,
        removeResource,
        unloader
) where

import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import Data.Hashable
import System.Mem.Weak

data ResMap r = ResMap (H.LinearHashTable Int (Either String r)) 

data ResStatus r = Loaded r
                 | Unloaded
                 | Error String

class (Eq i, Applicative m, EmbedIO m, Hashable i) =>
        Resource i r m where
        loadResource :: i -> m (Either String r)
        unloadResource :: Maybe i -> r -> m ()

class MonadIO m => EmbedIO m where
        embedIO :: (IO a -> IO b) -> m a -> m b

newResMap :: MonadIO m => m (ResMap r)
newResMap = ResMap <$> liftIO H.new

addResource :: Resource i r m => i -> ResMap r -> m ()
addResource i m = () <$ getResource i m

checkResource :: Resource i r m
              => i
              -> ResMap r
              -> m (ResStatus r)
checkResource i = checkResource' (Just i) $ hash i

checkResource' :: Resource i r m
               => Maybe i
               -> Int
               -> ResMap r
               -> m (ResStatus r)
checkResource' _ i (ResMap map) = do m <- liftIO $ H.lookup map i
                                     return $ case m of
                                                   Just (Right r) -> Loaded r
                                                   Just (Left e) -> Error e
                                                   Nothing -> Unloaded

getResource :: Resource i r m => i -> ResMap r -> m (Either String r)
getResource (i :: i) rmap@(ResMap map) =
        do status <- checkResource i rmap
           case status of
                   Unloaded ->
                        do r <- loadResource i

                           liftIO $ case r of
                                         Left s -> H.insert map ihash $ Left s
                                         Right r -> H.insert map ihash $ Right r

                           embedIO (addFinalizer i) $
                                   removeResource' (Nothing :: Maybe i)
                                                   ihash rmap
                           meRes <- liftIO . H.lookup map $ ihash
                           return $ case meRes of
                                         Just eRes -> eRes
                                         Nothing -> Left "Resource finalized"
                   Error s -> return $ Left s
                   Loaded r -> return $ Right r
        where ihash = hash i

-- reloadResource

removeResource :: Resource i r m => i -> ResMap r -> m ()
removeResource i = removeResource' (Just i) $ hash i

removeResource' :: Resource i r m => Maybe i -> Int -> ResMap r -> m ()
removeResource' mi i rmap@(ResMap map) = 
        do status <- checkResource' mi i rmap
           case status of
                Loaded r -> unloadResource mi r
                _ -> return ()
           liftIO $ H.delete map i

unloader :: (Resource i r m, EmbedIO m) => k -> Maybe i -> r -> m ()
unloader k i r = embedIO (addFinalizer k) $ unloadResource i r

instance Functor ResStatus where
        fmap f (Loaded r) = Loaded (f r)
        fmap _ Unloaded = Unloaded
        fmap _ (Error s) = Error s
