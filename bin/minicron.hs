module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)

main :: IO ()
main = do
  putStrLn "minicron"

  chan <- newChan
  -- Simulate a thread that modifies the tasks set.
  _ <- forkIO $ do
    writeChan chan $ ReplaceTasks [5, 6, 7, 8]
    threadDelay $ 13 * 1000000
    writeChan chan $ ReplaceTasks [1, 2]
    threadDelay $ 10 * 1000000
    writeChan chan $ ReplaceTasks [1, 2]
  mainThread chan (Tasks 0 []) Nothing

mainThread :: Chan Event -> Tasks -> Maybe ThreadId -> IO ()
mainThread chan (Tasks g tasks) msleep = do
  ev <- readChan chan
  case ev of
    WakeUp g' | g /= g' ->
      -- This is a wakeup from a killed sleep thread. Ignore it.
      return ()

    WakeUp _ -> case tasks of
      task : tasks' -> do
        putStrLn $ "Running task: " ++ show task
        mainThread' chan g tasks'
      _ -> do
        putStrLn $ "Error: no task after wakeup."
        mainThread' chan g tasks

    ReplaceTasks tasks' -> do
      putStrLn "Tasks set has been replaced."
      maybe (return ()) killThread msleep
      mainThread' chan g tasks'

mainThread' chan g tasks = do
  msleep <- scheduleNextTask chan (g + 1) tasks
  mainThread chan (Tasks (g + 1) tasks) msleep

scheduleNextTask _ _ [] = do
  putStrLn "No task to schedule."
  return Nothing
scheduleNextTask chan g (task : _) = do
  fmap Just . forkIO $ sleepThread chan g task

sleepThread chan g task = do
  threadDelay $ task * 1000000
  writeChan chan $ WakeUp g

data Event =
    WakeUp Int
  -- ^ Sent by the sleep thread for a particular generation.
  | ReplaceTasks [Task]
  -- ^ This means the list of tasks is replaced by these.

-- In this simple cron modelling, this is the number of seconds to wait
-- after the previous task has been run (or the system has beend started).
type Task = Int

data Tasks = Tasks
  Int
  -- ^ Generation (used to discard WakeUps from explicitely killed sleep).
  [Task]
  -- ^ The set of tasks.
