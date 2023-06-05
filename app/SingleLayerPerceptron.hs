module Main where

import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (MonadIO)
import System.Random (getStdRandom, uniformR)

-- ref: https://en.wikipedia.org/wiki/Perceptron#Learning_algorithm

genWeight :: MonadIO m => m Float
genWeight = getStdRandom $ uniformR (0 :: Float, 1 :: Float)

genWeights :: MonadIO m => Int -> m [Float]
genWeights size = replicateM size genWeight

calculate :: [Float] -> ([Float], Float) -> ([Float], [Float], Float, Float)
calculate ws (is, e) = (is, ws, e, foldr (\(w, i) acc -> acc + i * w) 0 (zip ws is))

adjustWeight :: Float -> ([Float], [Float], Float, Float) -> [Float]
adjustWeight r (is, ws, e, a) = zipWith adjust ws is
  where
    adjust w i = w + r * (e - a) * i

data Neuron = Neuron
  { neuronWeight :: [Float],
    neuronLearningRate :: Float
  }
  deriving (Show)

learn :: Neuron -> [([Float], Float)] -> Neuron
learn (Neuron ws r) = go ws
  where
    go :: [Float] -> [([Float], Float)] -> Neuron
    go ws' [] = Neuron ws' r
    go ws' (t : rest) =
      let calculated = calculate ws' t
       in go (adjustWeight r calculated) rest

learnRepeatedly :: Int -> Neuron -> [([Float], Float)] -> Neuron
learnRepeatedly 0 n _ = n
learnRepeatedly c n d = learnRepeatedly (c - 1) (learn n d) d

think :: Neuron -> [Float] -> Float
think (Neuron ws _) = sum . zipWith (*) ws

run :: Int -> [[Float]] -> Float -> [([Float], Float)] -> IO ()
run _ _ _ [] = fail "dataset must be provided"
run n xs r dataset = do
  let weightSize = length . fst . head $ dataset
  ws <- genWeights weightSize
  let learned = learnRepeatedly n (Neuron ws r) dataset
  print learned
  forM_ xs (\x -> print $ "Input " <> show x <> " then: " <> show (think learned x))

twice :: Int -> Float -> IO ()
twice n r =
  let sample =
        [ ([0], 0),
          ([1], 2),
          ([2], 4),
          ([3], 6),
          ([4], 8)
        ]
   in run n [[x] | x <- [5 .. 10]] r sample

main :: IO ()
main = do
  print "twice"
  twice 50 1
