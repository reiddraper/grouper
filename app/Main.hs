module Main where

import Grouper
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, replicateM_, void)
import Data.Monoid ((<>))

reader :: SynchronousBatch a -> IO ()
reader sBatch = forever $ do
    (items, sig) <- readBatch sBatch
    STM.atomically (STM.putTMVar sig ())

worker :: SynchronousBatch Bool -> Int -> IO ()
worker sb _ =
    replicateM_ 10000 (writeItem sb True)

test :: IO ()
test = do
    sBatch <- newSynchronousBatch 256
    withAsync (reader sBatch) $ \rdr ->
        void (mapConcurrently (worker sBatch) [1..128])

main :: IO ()
main =
   async test >>= wait
