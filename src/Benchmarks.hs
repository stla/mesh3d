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
     evaluate $ unique vslong
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end

bm2 =
  do start <- getTime Monotonic
     unique' vslong
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end

bm3 =
  do start <- getTime Monotonic
     unique'' vslong'
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end
