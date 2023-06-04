module Main where

import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (MonadIO)
import System.Random (getStdRandom, uniformR)

-- ref: https://en.wikipedia.org/wiki/Perceptron#Learning_algorithm

type Weight = Float

genWeight :: MonadIO m => m Weight
genWeight = getStdRandom $ uniformR (0 :: Float, 1 :: Float)

genWeights :: MonadIO m => Int -> m [Weight]
genWeights size = replicateM size genWeight

type Input = Float

type Expected = Float

type Training = ([Input], Expected)

type DataSet = [Training]

type LearningRate = Float

type Actual = Float

type Calculated = ([Input], [Weight], Expected, Actual)

calculate :: [Weight] -> Training -> Calculated
calculate ws (is, e) = (is, ws, e, foldr (\(w, i) acc -> acc + i * w) 0 (zip ws is))

adjustWeight :: LearningRate -> Calculated -> [Weight]
adjustWeight r (is, ws, e, a) = zipWith adjust ws is
  where
    adjust w i = w + r * (e - a) * i

data Neuron = Neuron
  { neuronWeights :: [Weight],
    neuronLearningRate :: LearningRate
  }
  deriving (Show)

learn :: Neuron -> DataSet -> Neuron
learn (Neuron ws r) = go ws
  where
    go :: [Weight] -> DataSet -> Neuron
    go ws' [] = Neuron ws' r
    go ws' (t : rest) =
      let calculated = calculate ws' t
       in go (adjustWeight r calculated) rest

learnRepeatedly :: Int -> Neuron -> DataSet -> Neuron
learnRepeatedly 0 n _ = n
learnRepeatedly c n d = learnRepeatedly (c - 1) (learn n d) d

think :: Neuron -> [Input] -> Float
think (Neuron ws _) = foldr (\(w, i) acc -> acc + w * i) 0 . zip ws

twice :: Int -> IO ()
twice n = do
  let sample =
        [ ([0], 0),
          ([1], 2),
          ([2], 4),
          ([3], 6),
          ([4], 8)
        ]
  ws <- genWeights 1
  let learned = learnRepeatedly n (Neuron ws 1) sample
  forM_ [5 .. 10] (\x -> print $ "Input " <> show x <> " then: " <> show (think learned [x]))

main :: IO ()
main = do
  print "twice"
  twice 100
