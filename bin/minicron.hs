{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.AffineSpace ((.+^))
import Data.Thyme.Clock (fromSeconds, getCurrentTime)

import System.Cron.Mini

main :: IO ()
main = do
  putStrLn "minicron"

  now <- getCurrentTime
  let task n = Task (now .+^ fromSeconds n) "do-something" $
        Just (Nothing, 24 * 60 * 60)

  cron $ map task [5, 10]
