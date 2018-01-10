{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, TypeFamilies,
             FunctionalDependencies, ScopedTypeVariables, FlexibleContexts,
             KindSignatures, FlexibleInstances, GeneralizedNewtypeDeriving,
             DefaultSignatures, StandaloneDeriving, UndecidableInstances #-}

module Graphics.Rendering.Ombra.Internal.Resource (
        ResMap,
        ResStatus(..),
        MonadLoad(..),
        MonadResource(..),
        getResource,
        ResourceT,
        runResourceT,
        runNewResourceT,
        mapResourceT,
        Resource3T,
        runResource3T,
        runNewResource3T,
        mapResource3T,
        newResMap,
        addResMap,
        getResMap,
        getResMap',
        checkResMap,
        removeResMap,
        unloader
) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Cont
-- import Control.Monad.Error
import Control.Monad.Fail
import Control.Monad.Zip
import Control.Monad.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Reader as Reader
import Control.Monad.RWS
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import qualified Data.HashTable.IO as H
import Data.Hashable
import System.Mem.Weak

data ResMap r = ResMap (H.LinearHashTable Int (Either String r)) 

data ResStatus r = Loaded r
                 | Unloaded
                 | Error String

class (Eq i, Hashable i, Monad m) => MonadLoad i r m where
        loadResource :: i -> m (Either String r)
        default loadResource :: (MonadLoad i r m', MonadTrans t, t m' ~ m)
                             => i
                             -> m (Either String r)
        loadResource = lift . loadResource

        default unloadResource :: (MonadLoad i r m', MonadTrans t, t m' ~ m)
                               => Maybe i
                               -> r
                               -> m ()
        unloadResource :: Maybe i -> r -> m ()
        unloadResource i r = lift $ unloadResource i r

class Monad m => MonadResource i r m where
        getResource' :: Maybe k -> i -> m (Either String r)
        default getResource' :: (MonadResource i r m', MonadTrans t, m ~ t m')
                             => Maybe k
                             -> i
                             -> m (Either String r)
        getResource' k i = lift $ getResource' k i

getResource :: MonadResource i r m => i -> m (Either String r)
getResource i = getResource' (Just i) i

newtype ResourceT r m a = ResourceT { unResourceT :: ReaderT (ResMap r) m a }
        deriving ( Functor
                 , Applicative
                 , Alternative
                 , Monad
                 , MonadFix
                 , MonadFail
                 , MonadZip
                 , MonadIO
                 , MonadPlus
                 )

deriving instance MonadCont m => MonadCont (ResourceT r m)
deriving instance MonadRWS r w s m => MonadRWS r w s (ResourceT r' m)
deriving instance MonadState s m => MonadState s (ResourceT r m)
deriving instance (MonadWriter w m, Monoid w) => MonadWriter w (ResourceT r m)
deriving instance MonadError e m => MonadError e (ResourceT r m)

runResourceT :: ResourceT r m a -> ResMap r -> m a
runResourceT (ResourceT m) = runReaderT m

runNewResourceT :: MonadBase IO m => ResourceT r m a -> m a
runNewResourceT (ResourceT m) = liftBase newResMap >>= runReaderT m

mapResourceT :: (m a -> n b) -> ResourceT r m a -> ResourceT r n b
mapResourceT f = ResourceT . mapReaderT f . unResourceT

instance MonadReader r' m => MonadReader r' (ResourceT r m) where
        ask = ResourceT $ lift Reader.ask
        local = mapResourceT . Reader.local
        reader = ResourceT . lift . Reader.reader

instance (Monad m, MonadLoad i r m, MonadBaseControl IO m) =>
        MonadResource i r (ResourceT r m) where
        getResource' k i = ResourceT $ Reader.ask >>= lift . getResMap' k i

instance MonadBase b m => MonadBase b (ResourceT r m) where
        liftBase = liftBaseDefault

instance MonadTrans (ResourceT r) where
        lift = ResourceT . lift

instance MonadTransControl (ResourceT r) where
        type StT (ResourceT r) a = StT (ReaderT (ResMap r)) a
        liftWith = defaultLiftWith ResourceT unResourceT
        restoreT = defaultRestoreT ResourceT

instance MonadBaseControl b m => MonadBaseControl b (ResourceT r m) where
        type StM (ResourceT r m) a = ComposeSt (ResourceT r) m a
        liftBaseWith = defaultLiftBaseWith
        restoreM = defaultRestoreM

newtype Resource3T r1 r2 r3 m a = Resource3T {
        unResource3T :: ReaderT (ResMap r1, ResMap r2, ResMap r3) m a
        }
        deriving ( Functor
                 , Applicative
                 , Alternative
                 , Monad
                 , MonadFix
                 , MonadFail
                 , MonadZip
                 , MonadIO
                 , MonadPlus
                 )

deriving instance MonadCont m => MonadCont (Resource3T r1 r2 r3 m)
deriving instance MonadRWS r w s m => MonadRWS r w s (Resource3T r1 r2 r3 m)
deriving instance MonadState s m => MonadState s (Resource3T r1 r2 r3 m)
deriving instance (MonadWriter w m, Monoid w) =>
        MonadWriter w (Resource3T r1 r2 r3 m)
deriving instance MonadError e m => MonadError e (Resource3T r1 r2 r3 m)

runResource3T :: Resource3T r1 r2 r3 m a
              -> ResMap r1
              -> ResMap r2
              -> ResMap r3
              -> m a
runResource3T (Resource3T m) r1 r2 r3 = runReaderT m (r1, r2, r3)

runNewResource3T :: MonadBase IO m => Resource3T r1 r2 r3 m a -> m a
runNewResource3T (Resource3T m) = (,,) <$> liftBase newResMap
                                       <*> liftBase newResMap
                                       <*> liftBase newResMap
                                  >>= runReaderT m

mapResource3T :: (m a -> n b)
              -> Resource3T r1 r2 r3 m a
              -> Resource3T r1 r2 r3 n b
mapResource3T f = Resource3T . mapReaderT f . unResource3T

instance MonadReader r m => MonadReader r (Resource3T r1 r2 r3 m) where
        ask = Resource3T $ lift Reader.ask
        local = mapResource3T . Reader.local
        reader = Resource3T . lift . Reader.reader

instance (Monad m, MonadLoad i r1 m, MonadBaseControl IO m) =>
        MonadResource i r1 (Resource3T r1 r2 r3 m) where
        getResource' k i = Resource3T $ Reader.ask >>= lift . getResMap' k i . m
                where m (m1, m2, m3) = m1

instance (Monad m, MonadLoad i r2 m, MonadBaseControl IO m) =>
        MonadResource i r2 (Resource3T r1 r2 r3 m) where
        getResource' k i = Resource3T $ Reader.ask >>= lift . getResMap' k i . m
                where m (m1, m2, m3) = m2

instance (Monad m, MonadLoad i r3 m, MonadBaseControl IO m) =>
        MonadResource i r3 (Resource3T r1 r2 r3 m) where
        getResource' k i = Resource3T $ Reader.ask >>= lift . getResMap' k i . m
                where m (m1, m2, m3) = m3

instance MonadBase b m => MonadBase b (Resource3T r1 r2 r3 m) where
        liftBase = liftBaseDefault

instance MonadTrans (Resource3T r1 r2 r3) where
        lift = Resource3T . lift

instance MonadTransControl (Resource3T r1 r2 r3) where
        type StT (Resource3T r1 r2 r3) a =
                StT (ReaderT (ResMap r1, ResMap r2, ResMap r3)) a
        liftWith = defaultLiftWith Resource3T unResource3T
        restoreT = defaultRestoreT Resource3T

instance MonadBaseControl b m =>
        MonadBaseControl b (Resource3T r1 r2 r3 m) where
        type StM (Resource3T r1 r2 r3 m) a = ComposeSt (Resource3T r1 r2 r3) m a
        liftBaseWith = defaultLiftBaseWith
        restoreM = defaultRestoreM

newResMap :: MonadBase IO m => m (ResMap r)
newResMap = ResMap <$> liftBase H.new

addResMap :: (MonadLoad i r m, MonadBaseControl IO m) => i -> ResMap r -> m ()
addResMap i m = () <$ getResMap i m

checkResMap :: (MonadLoad i r m, MonadBase IO m)
            => i
            -> ResMap r
            -> m (ResStatus r)
checkResMap i = checkResMap' (Just i) $ hash i

checkResMap' :: (MonadLoad i r m, MonadBase IO m)
             => Maybe i
             -> Int
             -> ResMap r
             -> m (ResStatus r)
checkResMap' _ i (ResMap map) = do m <- liftBase $ H.lookup map i
                                   return $ case m of
                                                 Just (Right r) -> Loaded r
                                                 Just (Left e) -> Error e
                                                 Nothing -> Unloaded


getResMap :: (MonadLoad i r m, MonadBaseControl IO m)
          => i
          -> ResMap r
          -> m (Either String r)
getResMap i = getResMap' (Just i) i

getResMap' :: (MonadLoad i r m, MonadBaseControl IO m)
           => Maybe k
           -> i
           -> ResMap r
           -> m (Either String r)
getResMap' mk (i :: i) rmap@(ResMap map) =
        do status <- checkResMap i rmap
           case status of
                   Unloaded ->
                        do r <- loadResource i

                           liftBase $
                                   case r of
                                        Left s -> H.insert map ihash $ Left s
                                        Right r -> H.insert map ihash $ Right r

                           case mk of
                                Just k -> liftBaseDiscard (addFinalizer k) $
                                        removeResMap' (Nothing :: Maybe i)
                                                      ihash rmap
                                Nothing -> return ()

                           meRes <- liftBase . H.lookup map $ ihash
                           return $ case meRes of
                                         Just eRes -> eRes
                                         Nothing -> Left "Resource finalized"
                   Error s -> return $ Left s
                   Loaded r -> return $ Right r
        where ihash = hash i

-- reloadResource

removeResMap :: (MonadLoad i r m, MonadBase IO m) => i -> ResMap r -> m ()
removeResMap i = removeResMap' (Just i) $ hash i

removeResMap' :: (MonadLoad i r m, MonadBase IO m)
              => Maybe i
              -> Int
              -> ResMap r
              -> m ()
removeResMap' mi i rmap@(ResMap map) = 
        do status <- checkResMap' mi i rmap
           case status of
                Loaded r -> unloadResource mi r
                _ -> return ()
           liftBase $ H.delete map i

unloader :: (MonadLoad i r m, MonadBaseControl IO m)
         => k
         -> Maybe i
         -> r
         -> m ()
unloader k i r = liftBaseDiscard (addFinalizer k) $ unloadResource i r

instance Functor ResStatus where
        fmap f (Loaded r) = Loaded (f r)
        fmap _ Unloaded = Unloaded
        fmap _ (Error s) = Error s

instance MonadResource i r m => MonadResource i r (ContT r' m)
-- instance (MonadResource i r m, Error e) => MonadResource i r (ErrorT e m)
instance MonadResource i r m => MonadResource i r (ExceptT e m)
instance MonadResource i r m => MonadResource i r (MaybeT m)
instance MonadResource i r m => MonadResource i r (ReaderT r' m)
instance (MonadResource i r m, Monoid w) => MonadResource i r (Strict.WriterT w m)
instance (MonadResource i r m, Monoid w) => MonadResource i r (Lazy.WriterT w m)
instance MonadResource i r m => MonadResource i r (Strict.StateT s m)
instance MonadResource i r m => MonadResource i r (Lazy.StateT s m)
instance MonadResource i r m => MonadResource i r (ListT m)
instance (MonadResource i r m, Monoid w) => MonadResource i r (Lazy.RWST r' w s m)
instance (MonadResource i r m, Monoid w) => MonadResource i r (Strict.RWST r' w s m)
instance MonadResource i r m => MonadResource i r (IdentityT m)
