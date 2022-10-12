module Main where

import Examples
import Simulate
import qualified Data.Map.Lazy as M

ratioShortBranch = 0.42
reorderScaleFactor = 0.8
weightBranch s l = ratioShortBranch * s + (1-ratioShortBranch) * l

guessASIC prog
  = guessPerf prog 10 1e9 reorderScaleFactor
      (M.fromList [(CDBranch  , weightBranch 5.0 0.7)
                  ,(CDJump    , 4.0)
                  ,(CDHazard 1, 1.0)
                  ,(CDHazard 2, 0)])
guessSlowASIC prog
  = guessPerf prog 10 1e9 reorderScaleFactor
      (M.fromList [(CDBranch  , weightBranch 5.0 0.7)
                  ,(CDJump    , 4.0)
                  ,(CDHazard 1, 1.0)
                  ,(CDHazard 2, 0)])
guess5Stage prog
  = guessPerf prog 5 400e6 0.70
      (M.fromList [(CDBranch  , weightBranch 2.0 3.0)
                  ,(CDJump    , 2.0)
                  ,(CDHazard 1, 1.0)])
guess3Stage prog
  = guessPerf prog 3 350e6 0.65
      (M.fromList [(CDBranch  , weightBranch 1.0 2.0)
                  ,(CDJump    , 1.0)
                  ,(CDHazard 1, 0.5)])

main = do putStrLn $ "Running sumO2 [0,1,2,3]..."
          reportPipelining (coreSumO2)
          putStrLn $ "Running fibO2 20..."
          reportPipelining (coreFibO2 20)
          putStrLn $ "Running OrdList 4..."
          reportPipelining (coreOrdList 4)
          putStrLn $ "Running MSS -12 to 12..."
          reportPipelining (coreMSS 12)

