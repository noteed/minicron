{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import Control.Concurrent.MVar (modifyMVar, newMVar, takeMVar, putMVar, MVar)
import Data.AffineSpace ((.+^), (.-.))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme.Clock (fromSeconds, getCurrentTime, toSeconds, UTCTime)
import Data.Thyme.Format (formatTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)

import System.Cron.Mini

main :: IO ()
main = do
  putStrLn "minicron"

  now <- getCurrentTime
  let task n = Task (now .+^ fromSeconds n) "do-something" $
        Just (Nothing, 24 * 60 * 60)

  cron $ map task [5, 10]
