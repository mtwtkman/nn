module Main where

import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (MonadIO)
import System.Random (getStdRandom, uniformR)

-- ref: https://en.wikipedia.org/wiki/Perceptron#Learning_algorithm

genFloat :: MonadIO m => m Float
genFloat = getStdRandom $ uniformR (0 :: Float, 1 :: Float)

genFloats :: MonadIO m => Int -> m [Float]
genFloats size = replicateM size genFloat

calculate :: [Float] -> ([Float], Float) -> ([Float], [Float], Float, Float)
calculate ws (is, e) = (is, ws, e, foldr (\(w, i) acc -> acc + i * w) 0 (zip ws is))

adjustFloat :: Float -> ([Float], [Float], Float, Float) -> [Float]
adjustFloat r (is, ws, e, a) = zipWith adjust ws is
  where
    adjust w i = w + r * (e - a) * i

data Neuron = Neuron
  { neuronFloats :: [Float],
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
       in go (adjustFloat r calculated) rest

learnRepeatedly :: Int -> Neuron -> [([Float], Float)] -> Neuron
learnRepeatedly 0 n _ = n
learnRepeatedly c n d = learnRepeatedly (c - 1) (learn n d) d

think :: Neuron -> [Float] -> Float
think (Neuron ws _) = sum . zipWith (*) ws

run :: Int -> [[Float]] -> Float -> [([Float], Float)] -> IO ()
run _ _ _ [] = fail "dataset must be provided"
run n xs r dataset = do
  let weightSize = length . fst . head $ dataset
  ws <- genFloats weightSize
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
