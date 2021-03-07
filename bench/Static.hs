module Main where

import           Control.Monad
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.VEBLayout as VEB
import           System.Random
import           Criterion.Main

--------------------------------------------------------------------------------

randomIOs   :: Random a => Int -> IO [a]
randomIOs n = replicateM n randomIO

genKeys   :: Int -> IO [Int]
genKeys n = List.sort <$> randomIOs n

genInput   :: Int -> IO [(Int,Int)]
genInput n = map (\k -> (k,k)) <$> genKeys n

-- benchmark searches static trees

main :: IO ()
main = defaultMain
  [ build 1000
  , runQueries 1000 2000
  ]

build   :: Int -> Benchmark
build n = env (genInput n) $ \xs ->
            bgroup ("building a tree of size " <> show n) $
              [ bench "VEBLayout"  $ nf VEB.fromAscList    xs
              , bench "Map"        $ nf Map.fromAscList    xs
              , bench "IntMap"     $ nf IntMap.fromAscList xs
              ]


preprocess n m = do xs <- genInput n
                    qs <- randomIOs m
                    print "woei"
                    print qs
                    pure $ ( VEB.fromAscList xs
                           , Map.fromAscList xs
                           , IntMap.fromAscList xs
                           , qs
                           )


runAllQueries     :: (Int -> Maybe (Int,Int)) -> [Int] -> Int
runAllQueries qry = List.foldl' (\a q -> case qry q of
                                           Nothing    -> a
                                           Just (_,v) -> a + v
                                ) 0





runQueries     :: Int -- ^ input size
               -> Int -- ^ Number of queries
               -> Benchmark
runQueries n m = env (preprocess n m) $ \(vebT, map', intMap, qs) ->
                   bgroup ("querying " <> show m <> " queries on size " <> show n)
                     [ bench "VEBLayout"  $ nf (query VEB.lookupGE vebT)      qs
                     , bench "Map"        $ nf (query Map.lookupGE map')      qs
                     , bench "IntMap"     $ nf (query IntMap.lookupGE intMap) qs
                     ]
  where
    query f t = runAllQueries (flip f t)
