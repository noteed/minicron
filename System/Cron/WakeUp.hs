{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Wakeup service.
module System.Cron.WakeUp where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)

data Client = Client { wakeup :: IO (Maybe Int) }

wakeupService :: Bool -> ((Int -> IO ()) -> IO Client) -> IO ()
wakeupService continue client = do
  chan <- newChan
  let request n = writeChan chan $ RequestWakeUp n
  tasks <- client request
  mainThread chan tasks Nothing continue

mainThread :: Chan Event -> Client -> Maybe ThreadId -> Bool -> IO ()
mainThread chan tasks msleep continue = do
  ev <- readChan chan
  case ev of
    WakeUp -> do
      mw <- wakeup tasks
      mainThread' chan mw tasks continue

    RequestWakeUp amount -> do
      putStrLn "Wakeup requested."
      maybe (return ()) killThread msleep
      mainThread' chan (Just amount) tasks continue

mainThread' :: Chan Event -> Maybe Int -> Client -> Bool -> IO ()
mainThread' chan mw tasks continue = do
  case mw of
    Nothing | continue -> do
      mainThread chan tasks Nothing continue
    Nothing -> do
      putStrLn "No more tasks - Exiting."
    Just w -> do
      s <- forkIO $ sleepThread chan w
      mainThread chan tasks (Just s) continue

sleepThread :: Chan Event -> Int -> IO ()
sleepThread chan amount = do
  threadDelay amount
  writeChan chan WakeUp

data Event =
    WakeUp
  -- ^ Sent by the sleep thread.
  | RequestWakeUp Int
  -- ^ Cancel any previously requested wakeup, and request another one
  -- n seconds later.
