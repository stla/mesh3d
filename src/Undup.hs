{-# LANGUAGE ScopedTypeVariables #-}
module Undup
  where
import           Data.List
import           Data.Maybe
import           Data.Vector.Unboxed         (Vector, freeze, Unbox, (!))
import           Data.Vector.Unboxed.Mutable (IOVector, new, write)
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
  let inner :: Int -> Int -> Int -> IO ()
      inner i j count | j == n = return ()
                      | otherwise =
                        if vs !! i == vs !! j
                          then do
                            write visited j True
                            write idx j count
                            inner i (j+1) count
                          else inner i (j+1) count

  let go :: Int -> Int -> IO (IOVector a)
      go i count | i == n = return $ VM.take count nvs
                 | otherwise = do
                   vst <- VM.read visited i
                   if not vst
                     then do
                       write nvs count (vs !! i)
                       write idx i count
                       write visited i True
                       _ <- inner i (i+1) count
                       go (i+1) (count + 1)
                     else go (i+1) count
  nvs' <- go 0 0
  nvs'' <- freeze nvs'
  idx' <- freeze idx
  return (nvs'', idx')

undupMesh :: forall a . (Unbox a, Eq a) => ([a], [[Int]]) -> IO (Vector a, [[Int]])
undupMesh (vs, faces) = do
  (nvs, idx) <- unique' vs
  let nfaces = map (map (idx !)) faces
  return (nvs, nfaces)

vs :: [Double]
vs = [1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4]

faces :: [[Int]]
faces = [[0,1,2],[3,4,5],[6,7,8],[9,10,11]]
