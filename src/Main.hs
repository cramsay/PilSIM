module Main where

import Examples
import Simulate
import qualified Data.Map.Lazy as M

guessASIC prog
  = guessPerf prog 10 1e9   (M.fromList [(CDBranch,6.0),(CDHazard 1,0.7*3),(CDHazard 2,0.7*3-1)])
guessSlowASIC prog
  = guessPerf prog 10 500e6 (M.fromList [(CDBranch,6.0),(CDHazard 1,0.7*3),(CDHazard 2,0.7*3-1)])
guess5Stage prog
  = guessPerf prog 5  400e6 (M.fromList [(CDBranch,3.0),(CDHazard 1,0.7*2)])
guess3Stage prog
  = guessPerf prog 3  350e6 (M.fromList [(CDBranch,1.5),(CDHazard 1,0.7*1)])

main = do putStrLn $ "Running sumO2 [0,1,2,3]..."
          guess3Stage (coreSumO2)
          putStrLn $ "Running fibO2 20..."
          guess3Stage (coreFibO2 20)
          putStrLn $ "Running OrdList 4..."
          guess3Stage (coreOrdList 4)
          putStrLn $ "Running MSS -12 to 12..."
          guess3Stage (coreMSS 12)

