{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import Control.Monad.Reader
import Control.Monad.Random.Lazy
import Control.Monad.Random.Class
import System.Random.Shuffle

type Survivor = Int

type Group = [Survivor]

-- | Survivors keep watch in pairs
type Shift = (Survivor, Survivor)

-- | One night's schedule has three shifts
type Schedule = (Shift, Shift, Shift)

-- | An infinite stream of random possible shifts
shifts :: (MonadReader Group m, MonadRandom m) => m [Shift]
shifts = concat . repeat <$> step
    where step = do
            people <- ask
            possibleShifts <- shuffleM $ pairs people
            pure possibleShifts

          pairs :: [a] -> [(a, a)]
          pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- | Construct a randomised schedule
buildSchedule :: (MonadReader Group m, MonadRandom m) => m Schedule
buildSchedule = do
    [x, y, z] <- take 3 <$> shifts
    pure $ (x, y, z)

main :: IO ()
main = do
    let people = [1..5]
    schedule <- evalRandTIO (runReaderT buildSchedule people)
    putStrLn $ "Watch schedule for this night: " ++ show schedule
