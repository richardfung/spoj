import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Data.Array.ST
import Data.Array.Unboxed

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \_ -> do
        [m,n] <- (map read) <$> words <$> getLine :: IO [Int]
        print genPrimes

genPrimes :: [Int]
genPrimes = filter (xs !) [2..1000000000]
    where
          xs = helper 1000000000
          helper :: Int -> UArray Int Bool
          helper end = runSTUArray $ do
              nums <- newArray (2,end) True
              forM_ [2..end] $ \i -> do
                  isPrime <- readArray nums i
                  when isPrime $ do
                      forM_ [2*i,3*i..end] $ \j -> do
                          writeArray nums j False
              return nums
