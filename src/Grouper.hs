{-# LANGUAGE RecordWildCards   #-}

module Grouper
    ( SynchronousBatch(..)
    , newSynchronousBatch
    , writeItem
    , readBatch
    ) where

import qualified Control.Concurrent.STM as STM


drainTBQueue :: STM.TBQueue a -> STM.STM [a]
drainTBQueue q = reverse <$> loop [] q
    where loop acc q = do
            val <- STM.tryReadTBQueue q
            case val of
                Nothing -> return acc
                Just x ->
                    loop (x:acc) q

data SynchronousBatch a = SynchronousBatch
    { batch :: STM.TBQueue a
    , signal :: STM.TVar (STM.TMVar ())
    }

newSynchronousBatch :: Int -> IO (SynchronousBatch a)
newSynchronousBatch queueSize = STM.atomically $ do
    q <- STM.newTBQueue queueSize
    sig <- STM.newEmptyTMVar
    sigHolder <- STM.newTVar sig
    return (SynchronousBatch q sigHolder)

writeItem :: SynchronousBatch a -> a -> IO ()
writeItem SynchronousBatch{..} x = do
    sig <- STM.atomically $ do
            s <- STM.readTVar signal
            STM.writeTBQueue batch x
            return s
    STM.atomically (STM.readTMVar sig)

readBatch :: SynchronousBatch a -> IO ([a], STM.TMVar ())
readBatch SynchronousBatch{..} = STM.atomically $ do
    newSig <- STM.newEmptyTMVar
    oldSignal <- STM.readTVar signal
    STM.swapTVar signal newSig
    items <- drainTBQueue batch
    return (items, oldSignal)
