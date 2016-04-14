{-# LANGUAGE RecordWildCards   #-}

module Main where

import Grouper
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, replicateM_, void)
import Data.Monoid ((<>))

drainTBQueue :: STM.TBQueue a -> STM.STM [a]
drainTBQueue q = reverse <$> loop [] q
    where loop acc q = do
            val <- STM.tryReadTBQueue q
            case val of
                Nothing -> return acc
                Just x ->
                    loop (x:acc) q

reader :: SynchronousBatch a -> IO ()
reader SynchronousBatch{..} = forever $ do
    items <- STM.atomically (drainTBQueue batch)
    mapM_ (STM.atomically . flip STM.putTMVar ()) (signal <$> items)

worker :: SynchronousBatch Bool -> Int -> IO ()
worker sb _ =
    replicateM_ 10000 (writeItem sb True)

test :: IO ()
test = do
    q <- STM.newTBQueueIO 256
    let sBatch = SynchronousBatch q
    withAsync (reader sBatch) $ \rdr ->
        void (mapConcurrently (worker sBatch) [1..128])

main :: IO ()
main =
   async test >>= wait
