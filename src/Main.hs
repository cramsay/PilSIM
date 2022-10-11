module Main where

import Examples

main = do putStrLn $ "Running sumO2 [0,1,2,3]..."
          reportPipelining (coreSumO2)
          putStrLn $ "Running fibO2 20..."
          reportPipelining (coreFibO2 20)
          putStrLn $ "Running OrdList 4..."
          reportPipelining (coreOrdList 4)
          putStrLn $ "Running MSS -12 to 12..."
          reportPipelining (coreMSS 12)
