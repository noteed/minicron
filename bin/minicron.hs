{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

main :: IO ()
main = do
  putStrLn "minicron"
  wakeupService makeTasks

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

mainThread' chan mw tasks = do
  msleep <- maybe (return Nothing) (fmap Just . forkIO . sleepThread chan) mw
  mainThread chan tasks msleep

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

-- | A generation is used to identify a particular state in the tasks thread.
-- It can be viewed as a request ID from the client to the wakeup service. The
-- wakeup service will answer with it, allowing the client to discard spurious
-- wakeups.
newtype Generation = Generation Int
  deriving (Eq, Num)

data Client = Client { wakeup :: Generation -> IO (Maybe (Generation, Int)) }

-- | Get a reference to tasks. Doing so, let them communicate asynchronously
-- if a new wakeup must be rescheduled, or completely canceled.
makeTasks :: (Generation -> Int -> IO ()) -> (IO ()) -> IO Client
makeTasks request cancel = do
  -- Initial task set.
  now <- getCurrentTime
  let task n = Task (now .+^ fromSeconds n) "do-something" $
        Just (Nothing, 24 * 60 * 60)

  ref <- newMVar (0, map task [5, 10])
  _ <- forkIO $ do
    request 0 5000000
  return . Client . runTasks $ Tasks ref

-- | Give a chance to tasks to be run. Return possibly a request to receive
-- another wakeup n seconds later. The second argument is a generation (i.e. a
-- identifier to discard wakeups received while they were cancelled).
-- To request a wakeup without waiting for `runTasks` to be called, the
-- `request` argument in `makeTasks` above should be used.
runTasks :: Tasks -> Generation -> IO (Maybe (Generation, Int))
runTasks (Tasks ref) g = do
  (g', tasks) <- takeMVar ref
  case tasks of
    _ | g /= g' -> do
      -- This is a wakeup from a cancel request. Ignore it.
      putMVar ref (g', tasks)
      return Nothing
    [] -> do
      putStrLn $ "Error: no task after wakeup."
      putMVar ref (g', tasks)
      return Nothing
    task : tasks' -> do
      putStrLn $ T.unpack (taskMethod task) ++ " @ " ++
        formatTime locale format (taskWhen task)
      let tasks'' = reschedule task tasks'
      putMVar ref (g + 1, tasks'')
      case tasks'' of
        [] -> return Nothing
        task':_ -> do
          amount <- amountToSleep task'
          return $ Just (g + 1, amount)
  where
  locale = defaultTimeLocale
  format = iso8601DateFormat $ Just "%H:%M:%S"

amountToSleep Task{..} = do
  now <- getCurrentTime
  -- TODO In a recent version of Thyme, we could use `microseconds` instead
  -- of second * 10^6.
  return $ ceiling . (* 1000) . (* 1000) . toSeconds $ taskWhen .-. now

reschedule Task{..} tasks =
  case taskRepetition of
    Just (count, interval) | maybe 1 id count > 0 ->
      let task = Task (taskWhen .+^ fromSeconds interval) taskMethod $
            Just (fmap pred count, interval)
      in reorder $ task : tasks
    _ -> tasks

reorder = sortBy (comparing taskWhen)

data Task = Task
  { taskWhen :: UTCTime
  , taskMethod :: Text
  , taskRepetition :: Maybe (Maybe Int, Int)
  -- ^ Possibly repeat the task, possibly a finite number of times, every n
  -- seconds.
  }
  deriving Show

data Tasks = Tasks (MVar (Generation, [Task]))
  -- ^ Generation (used to discard WakeUps from explicitely killed sleep),
  -- set of tasks.
