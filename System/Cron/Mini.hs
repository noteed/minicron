{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.Cron.Mini where

import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, MVar)
import Data.AffineSpace ((.+^), (.-.))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme.Clock (fromSeconds, getCurrentTime, toSeconds, UTCTime)
import Data.Thyme.Format (formatTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)

import System.Cron.WakeUp

cron :: [Task] -> IO ()
cron tasks = wakeupService $ \request _ -> do
  ref <- newMVar (0, reorder tasks)
  request 0 0
  return . Client . runTasks $ Tasks ref

-- | Give a chance to tasks to be run. Return possibly a request to receive
-- another wakeup n seconds later. The second argument is a generation (i.e. a
-- identifier to discard wakeups received while they were cancelled).
runTasks :: Tasks -> Generation -> IO (Maybe (Generation, Int))
runTasks (Tasks ref) g = do
  (g', tasks) <- takeMVar ref
  case tasks of
    _ | g /= g' -> do
      -- This is a wakeup from a cancel request. Ignore it.
      putMVar ref (g', tasks)
      return Nothing
    [] -> do
      putMVar ref (g', tasks)
      return Nothing
    task : tasks' -> do
      amount_ <- amountToSleep task
      if amount_ > 0
        then do
          -- Wakeup is too early (e.g. the first wakeup to initialize the
          -- system).
          putStrLn $ "Next task: " ++ T.unpack (taskMethod task) ++ " @ " ++
            formatTime locale format (taskWhen task)
          putMVar ref (g + 1, tasks)
          return $ Just (g + 1, amount_)
        else do
          putStrLn $ "Running: " ++ T.unpack (taskMethod task) ++ " @ " ++
            formatTime locale format (taskWhen task)
          let tasks'' = reschedule task tasks'
          putMVar ref (g + 1, tasks'')

          -- Select next task and return how long to wait.
          case tasks'' of
            [] -> return Nothing
            task':_ -> do
              amount <- amountToSleep task'
              putStrLn $ "Next task: " ++ T.unpack (taskMethod task') ++ " @ " ++
                formatTime locale format (taskWhen task')
              return $ Just (g + 1, amount)
  where
  locale = defaultTimeLocale
  format = iso8601DateFormat $ Just "%H:%M:%S"

amountToSleep :: Task -> IO Int
amountToSleep Task{..} = do
  now <- getCurrentTime
  -- TODO In a recent version of Thyme, we could use `microseconds` instead
  -- of second * 10^6.
  return $ ceiling . (* (1000 :: Double)) . (* 1000) . toSeconds $ taskWhen .-. now

reschedule :: Task -> [Task] -> [Task]
reschedule Task{..} tasks =
  case taskRepetition of
    Just (count, interval) | maybe 1 id count > 0 ->
      let task = Task (taskWhen .+^ fromSeconds interval) taskMethod $
            Just (fmap pred count, interval)
      in reorder $ task : tasks
    _ -> tasks

reorder :: [Task] -> [Task]
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
