module Main (main)
where

import System.Environment
import qualified BCode

main = do
  [fname] <- getArgs
  torrent <- readFile fname
  let bcoded = BCode.decode torrent
  case bcoded of
    Left pe -> print $ pe
    Right bcoded -> putStr $ BCode.prettyPrint bcoded