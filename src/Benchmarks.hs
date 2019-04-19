{-# LANGUAGE OverloadedStrings #-}
module Benchmarks
  where
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import Undup

bm1 =
  do start <- getTime Monotonic
     evaluate $ unique $ concat (replicate 1000 vs) ++ [10,11,12]
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end

bm2 =
  do start <- getTime Monotonic
     unique' $ concat (replicate 1000 vs) ++ [10,11,12]
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end
