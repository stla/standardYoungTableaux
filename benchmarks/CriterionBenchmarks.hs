module Main where
import           Criterion.Main
import           Criterion.Types
import           Enumeration
import           Math.Combinat.Partitions       ( Partition(..) )
import qualified Math.Combinat.Tableaux        as T


main :: IO ()
main = defaultMainWith
  defaultConfig { resamples = 3 }
  [ bench "syt" $ nf standardYoungTableaux [7, 4, 3, 2]
  , bench "combinat" $ nf T.standardYoungTableaux (Partition [7, 4, 3, 2])
  ]

-- stack bench standardYoungTableaux:criterion-benchmarks
