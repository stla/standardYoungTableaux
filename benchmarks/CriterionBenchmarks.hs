module Main where
import           Criterion.Main
import           Criterion.Types
import           Enumeration
import           Math.Combinat.Partitions       ( Partition(..) )
import qualified Math.Combinat.Tableaux        as T


main :: IO ()
main = defaultMainWith
  defaultConfig { resamples = 3 }
  [ bench "syt" $ nf standardYoungTableaux [6, 6, 2, 2, 1]
  , bench "ballot" $ nf ballotSequences [6, 6, 2, 2, 1]
  , bench "combinat" $ nf T.standardYoungTableaux (Partition [6, 6, 2, 2, 1])
  ]

-- stack bench standardYoungTableaux:criterion-benchmarks
