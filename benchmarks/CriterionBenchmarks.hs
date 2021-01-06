module Main where
import Criterion.Main
import Criterion.Types
import Enumeration

main :: IO ()
main = defaultMainWith
  defaultConfig { resamples = 10 }
  [ bench "syt"  $ nf standardYoungTableaux [5,3,2]
  ]

-- stack bench standardYoungTableaux:criterion-benchmarks