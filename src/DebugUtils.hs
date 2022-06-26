module DebugUtils where

import Debug.Trace

debugInfo :: Show a => String -> a -> a
debugInfo lab x = traceShow ("DEBUG: " ++ lab ++ " " ++ show x) x
