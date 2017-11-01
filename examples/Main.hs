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

groupSize :: Group -> Int
groupSize = length

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

possibleShifts :: (MonadReader Group m) => m [Shift]
possibleShifts = do
    people <- ask
    return (pairs people)

randomisedShifts :: ( MonadReader Group m
                    , MonadRandom m) => m [Shift]
randomisedShifts = do
    shifts <- possibleShifts
    randomised <- shuffleM shifts
    return randomised

shifts' :: (MonadReader Group m, MonadRandom m) => m [Shift]
shifts' = fmap (concat . repeat) randomisedShifts

-- | An infinite stream of random possible shifts
shifts :: (MonadReader Group m, MonadRandom m) => m [Shift]
shifts = fmap (concat . repeat) step
    where step = fmap pairs ask >>= shuffleM


-- | Construct a randomised schedule
buildSchedule :: (MonadReader Group m, MonadRandom m) => m Schedule
buildSchedule = do
    [x, y, z] <- fmap (take 3) shifts
    return (x, y, z)

main :: IO ()
main = do
    let people = [1..5]
    schedule <- evalRandTIO (runReaderT buildSchedule people)
    putStrLn $ "Watch schedule for this night: " ++ show schedule
