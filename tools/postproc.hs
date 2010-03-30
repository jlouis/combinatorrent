#!/usr/bin/env runhaskell

import Data.List
import System

main = do
    args <- getArgs
    case args of
        ["gather", rtsStat, combinatorrentStat, timeStat] ->
            gatherStats rtsStat combinatorrentStat timeStat
        ["present",  database] -> presentStats database


gatherStats rtsStat combinatorrentStat timeStat = do
    tStat <- readTimes timeStat
    cStat <- readCombinatorrentStat combinatorrentStat
    rStat <- readRtsStat rtsStat
    putStrLn $ show (tStat ++ cStat ++ rStat)


readRtsStat :: FilePath -> IO [(String, String)]
readRtsStat fp = do
    cts <- readFile fp
    return $ read . unlines . tail . lines $ cts

readCombinatorrentStat :: FilePath -> IO [(String, String)]
readCombinatorrentStat fp = do
    cts <- readFile fp
    let d = read cts :: [(String, Integer)]
    return $ map (\(k, v) -> (k, show v)) d

readTimes :: FilePath -> IO [(String, String)]
readTimes timeStat = do
    contents <- readFile timeStat
    let [s, e] = (map read . lines $ contents) :: [Integer]
    return [("start_time", show s)
           ,("end_time"  , show e)]

presentStats db = do
    cts <- readFile db
    let ls = map read . lines $ cts
    putStrLn "#Start\tEnd\tMaxBytesUsed\tPeakMegabytesAlloc\tMutCPU\tGCCPU"
    let formatted = map (format ["start_time", "end_time", "max_bytes_used",
                                 "peak_megabytes_allocated",
                                 "mutator_cpu_seconds",
                                 "GC_cpu_seconds"]) ls
    mapM_ putStrLn formatted

format :: [String] -> [(String, String)] -> String
format cols row = concat $ intersperse "\t" entries
    where entries = map (\c -> case find ((==c) . fst) row of
                                    Nothing -> error "Column doesn't exist"
                                    Just x -> snd x) cols

