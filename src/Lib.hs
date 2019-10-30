{-# Language ScopedTypeVariables #-}
module Lib where

import           Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import           Numeric.Natural
import           Streamly (SerialT, runStream, serially, IsStream, Serial, MonadAsync)
import           Streamly.Prelude (uncons, nil, (.:))
import qualified Streamly.Prelude as Streamly
import           System.IO
import           System.Random (randomIO, Random)

-- main = runStream . serially $ test
--   where
--     odds = Streamly.filter (\x -> x `mod` 2 /= 0) $ myStream 10
--     evens = Streamly.filter (\x -> x `mod` 2 == 0) $ myStream 10

--     test = mergeAll [odds, evens]




myStream   :: Int -> Serial Int
myStream n = Streamly.mapM (\i -> print i >> pure i ) $ Streamly.fromList [1..n]




data Sized a = Sized !Natural !a deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

type SizedS m a = Sized (SerialT m a)


data SP a b = SP !a !b deriving (Show,Eq)


type Stream = SerialT Identity

split' :: Word -> Natural -> Stream a -> Stream (Sized (Stream a))
split' = split

data Acc s m a = Acc !Natural -- ^ number of items in current stream
                     !(SerialT m a) -- ^ current output stream
                     !(s m (SizedS m a)) -- ^ completed streams


-- TODO: Verify that this runs in constant space
split        :: (Monad m, IsStream s, Monad (s m), Semigroup (s m (SizedS m a)))
             => Word -- ^ number of streams to split in
             -> Natural -- ^ number of items in the input stream
             -> SerialT m a -- ^ the input stream
             -> s m (SizedS m a)
split k n is = do
    Acc i s cs <- Streamly.yieldM $ Streamly.foldl' f (Acc 0 Streamly.nil Streamly.nil) is
    cs |> Sized i s
  where
    h = (n + 1) `div` (fromIntegral k)
    f (Acc i s cs) x | i < h     = Acc (i + 1) (s <> Streamly.yield x) cs
                     | otherwise = Acc 1       (Streamly.yield x) (cs |> Sized i s)

    cs |> s = cs <> Streamly.yield s


memSize = 3

fitsInMemory             :: SizedS m a -> Bool
fitsInMemory (Sized n _) = n < memSize


mergeSort                                    :: (Ord a, MonadAsync m)
                                             => Word -> SizedS m a -> m (SizedS m a)
mergeSort k ss@(Sized n s) | fitsInMemory ss = fmap (Sized n . Streamly.fromList . List.sort)
                                             . Streamly.toList $ s
                                               -- load fully in memory and sort it
                           | otherwise       = fmap mergeAll
                                             . Streamly.toList
                                             . Streamly.mapM (mergeSort k)
                                             . split k n $ s




run mss = do Sized _ s <- mss
             runStream $ Streamly.mapM print s


test   :: (Show a, Ord a) => SizedS IO a -> IO ()
test ss = run $ mergeSort 2 ss

sized   :: Monad m => SerialT m a -> m (SizedS m a)
sized s = do n <- Streamly.foldl' (\a _ -> a + 1) 0 s
             pure $ Sized n s
  -- TODO: I'm guessing this loads the stream into memory. I don't really want that.



replicateM'     :: (MonadAsync m, IsStream s, Semigroup (s m a)) => Natural -> m a -> s m a
replicateM' n a = go n
  where
    m = fromIntegral (maxBound :: Int)

    go i | i <= m    = Streamly.replicateM (fromIntegral i) a
         | otherwise = Streamly.replicateM maxBound a <> go (i - m)

genInput :: Natural -> SizedS IO Int
genInput = genInputWith randomIO



genInputWith    :: IO a -> Natural -> SizedS IO a
genInputWith g n = Sized n $ replicateM' n g




gen :: (Show a, Random a) => IO (IO a)
gen = do x <- randomIO
         pure $ do putStrLn $ "accessing " <> show x
                   pure x



withFileStreaming         :: BufferMode -> FilePath -> (SerialT IO String -> IO a)
                          -> IO a
withFileStreaming bm fp k = withFile fp ReadMode $ \h -> do
                               hSetBuffering h bm
                               k $ Streamly.fromHandle h


withFileSized :: BufferMode -> FilePath -> (SizedS IO String -> IO a)
              -> IO a
withFileSized bm fp k = withFileStreaming bm fp $ \ss ->
                          uncons ss >>= \case
                            Nothing    -> k $ Sized 0 nil
                            Just (l,s) -> k $ Sized (read l) s

-- -- TODO: This does not make sense, as we close the handle here
-- fromFile       :: Read a => BufferMode -> FilePath -> IO (SizedS IO a)
-- fromFile bm fp = withFile fp ReadMode $ \h -> do
--                    hSetBuffering h LineBuffering
--                    ms <- uncons (Streamly.fromHandle h)
--                    -- hSetBuffering h bm
--                    pure $ case ms of
--                      Nothing    -> Sized 0 nil
--                      Just (l,s) -> Sized (read l) (Streamly.map read s)


toFileWith                   :: Show a => BufferMode -> FilePath -> SizedS IO a -> IO ()
toFileWith bm fp (Sized n s) = withFile fp WriteMode $ \h -> do
                                 hSetBuffering h bm
                                 Streamly.toHandle h (show n .: Streamly.map show s)

toFile :: Show a => FilePath -> SizedS IO a -> IO ()
toFile = toFileWith (BlockBuffering $ Just 100)

-- toList = traverse (Streamly.toList)

testWrite = toFile "/tmp/test.txt" (genInput 100)


-- testRead = do (ss :: SizedS IO Int) <- fromFile (BlockBuffering $ Just 100) "/tmp/test.txt"
--               traverse Streamly.toList ss






testRead = withFileSized (BlockBuffering $ Just 100) "/tmp/test.txt" $ \(Sized n s) ->
             do print $ "Number of elements " <> show n
                Streamly.toList s

-- we need to seek for this to work.
-- testRead = do (ss :: SizedS IO Int) <- fromFile (BlockBuffering $ Just 100) "/tmp/test.txt"
--               test ss



-- mergeSort'            :: (MonadAsync m, Ord a)
--                       => Word -> SP Natural (SerialT m a) -> m (SerialT m a)
-- mergeSort' k (SP n s) =

-- SerialT m (SerialT m a)
-- m [SerialT m a]

-- m (SerialT m a)
-- SerialT m a

mergeAll     :: forall m a. (Monad m, Ord a) => [SizedS m a] -> SizedS m a
mergeAll sss = Sized n $ do m <- initialize <$> mapM (Streamly.yieldM . uncons) sss'
                            go m
  where
    n    = List.foldl' (\a (Sized i _) -> i + a) 0 sss
    sss' = map (\(Sized _ s) -> s) sss

    initialize sses = Map.fromList [ (x,s) | Just (x,s) <- sses ]

    go   :: Map.Map a (SerialT m a) -> SerialT m a
    go m = case Map.minViewWithKey m of
             Nothing          -> Streamly.nil -- all streams are done
             Just ((x,s), m') -> x .: (Streamly.yieldM (uncons s) >>= \case
                                          Nothing     -> go m' -- stream s is done
                                          Just (y,s') -> go (Map.insert y s' m')
                                      )
