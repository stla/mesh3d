{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Undup
  where
import           Control.Monad               ((>>=), when)
import           Data.List
import           Data.Maybe
import           Data.Vector.Unboxed         (Unbox, Vector, freeze, (!), unsafeFreeze)
import           Data.Vector.Unboxed.Mutable (IOVector, new, write, unsafeRead, unsafeWrite)
import qualified Data.Vector.Unboxed.Mutable as VM

unique :: Eq a => [a] -> ([a], [Int])
unique vs = (vsnub, indices)
  where
  vsnub = nub vs
  indices = map (\v -> fromJust $ elemIndex v vsnub) vs

unique' :: forall a . (Unbox a, Eq a) => [a] -> IO (Vector a, Vector Int)
unique' vs = do
  let n = length vs
  idx <- VM.replicate n 0 :: IO (IOVector Int)
  visited <- VM.replicate n False :: IO (IOVector Bool)
  nvs <- new n :: IO (IOVector a)
  let inner :: a -> Int -> Int -> IO ()
      inner !v j !count | j == n = return ()
                        | otherwise = do
                          when (v == vs !! j) $ do
                            unsafeWrite visited j True
                            unsafeWrite idx j count
                          inner v (j+1) count
  let go :: Int -> Int -> IO (IOVector a)
      go i !count | i == n = return $ VM.take count nvs
                  | otherwise = do
                    vst <- unsafeRead visited i
                    if not vst
                      then do
                        let v = vs !! i
                        unsafeWrite nvs count v
                        unsafeWrite idx i count
                        unsafeWrite visited i True
                        _ <- inner v (i+1) count
                        go (i+1) (count + 1)
                      else go (i+1) count
  nvs' <- go 0 0 >>= unsafeFreeze
  idx' <- unsafeFreeze idx
  return (nvs', idx')

undupMesh :: forall a . (Unbox a, Eq a) => ([a], [[Int]]) -> IO (Vector a, [[Int]])
undupMesh (vs, faces) = do
  (nvs, idx) <- unique' vs
  let nfaces = map (map (idx !)) faces
  return (nvs, nfaces)

vs :: [Double]
vs = [1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4]

faces :: [[Int]]
faces = [[0,1,2],[3,4,5],[6,7,8],[9,10,11]]
