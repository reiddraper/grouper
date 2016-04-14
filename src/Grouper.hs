module Grouper
    ( Item(..)
    , SynchronousBatch(..)
    , writeItem
    ) where

import qualified Control.Concurrent.STM as STM

data Item a = Item
    { item :: a
    , signal :: STM.TMVar ()
    }

data SynchronousBatch a = SynchronousBatch
    { batch :: STM.TBQueue (Item a)
    }

writeItem :: SynchronousBatch a -> a -> IO ()
writeItem b x = do
    i <- STM.atomically (writeItem' b x)
    STM.atomically (STM.takeTMVar (signal i))

writeItem' :: SynchronousBatch a -> a -> STM.STM (Item a)
writeItem' b x = do
    signal <- STM.newEmptyTMVar
    let i = Item x signal
    STM.writeTBQueue (batch b) i
    return i
