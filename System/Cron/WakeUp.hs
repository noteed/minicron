{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Wakeup service.
module System.Cron.WakeUp where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)

-- | A generation is used to identify a particular state in the tasks thread.
-- It can be viewed as a request ID from the client to the wakeup service. The
-- wakeup service will answer with it, allowing the client to discard spurious
-- wakeups. (A spurious wakeup can arise when a RequestWakeUp cancels an
-- already sent, but not yet processed wakeup).
newtype Generation = Generation Int
  deriving (Eq, Num)

data Client = Client { wakeup :: Generation -> IO (Maybe (Generation, Int)) }

wakeupService :: ((Generation -> Int -> IO ()) -> IO () -> IO Client) -> IO ()
wakeupService client = do
  chan <- newChan
  let request g n = writeChan chan $ RequestWakeUp g n
      cancel = writeChan chan CancelWakeUp
  tasks <- client request cancel
  mainThread chan tasks Nothing

mainThread :: Chan Event -> Client -> Maybe ThreadId -> IO ()
mainThread chan tasks msleep = do
  ev <- readChan chan
  case ev of
    WakeUp g -> do
      mw <- wakeup tasks g
      mainThread' chan mw tasks

    RequestWakeUp g amount -> do
      putStrLn "Wakeup requested."
      maybe (return ()) killThread msleep
      mainThread' chan (Just (g, amount)) tasks

    CancelWakeUp -> do
      putStrLn "Canceling wakeup."
      maybe (return ()) killThread msleep
      mainThread' chan Nothing tasks

mainThread' :: Chan Event -> Maybe (Generation, Int) -> Client -> IO ()
mainThread' chan mw tasks = do
  case mw of
    Nothing -> do
      putStrLn "No more tasks - Exiting."
    Just w -> do
      s <- forkIO $ sleepThread chan w
      mainThread chan tasks (Just s)

sleepThread :: Chan Event -> (Generation, Int) -> IO ()
sleepThread chan (g, amount) = do
  threadDelay amount
  writeChan chan $ WakeUp g

data Event =
    WakeUp Generation
  -- ^ Sent by the sleep thread for a particular generation.
  | RequestWakeUp Generation Int
  -- ^ Cancel any previously requested wakeup, and request another one.
  -- First the generation, then the amount of time in seconds to wait.
  | CancelWakeUp
  -- ^ Cancel any previously requested wakeup.
