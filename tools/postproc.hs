#!/usr/bin/env runhaskell

import System

main = do
    args <- getArgs
    case args of
        ["gather", rtsStat, combinatorrentStat, timeStat] ->
            gatherStats rtsStat combinatorrentStat timeStat
        ["present",  database] -> presentStats database


gatherStats rtsStat combinatorrentStat timeStat = do
--    start, end <- readTimes timeStat
--    cs         <- readCombinatorrentStat combinatorrentStat
--    rtsStat    <- readRtsStat rtsStat
    putStrLn "Gathering stats"


presentStats db = do
    putStrLn "Presenting stats"
