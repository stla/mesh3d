module ConnectedComponents
  where
import           Data.Graph      (flattenSCC, stronglyConnComp)
import           Data.List       (findIndices, intersect, elemIndex)
import           Data.List.Index (imap)

type Faces = [[Int]]

connectedComponents :: Faces -> [Faces]
connectedComponents faces = map flattenSCC (stronglyConnComp x)
  where
  x = imap (\i face -> (face, i, findIndices (connectedFaces face) faces)) faces
  connectedFaces face1 face2 = length(face1 `intersect` face2) == 2

faces :: Faces
faces = [[0,1,2], [0,1,3], [0,2,3], [1,2,3], [3,4,5]]

biggestComponent :: Faces -> Faces
biggestComponent faces = connComps !! i
  where
  connComps = connectedComponents faces
  lengths = map length connComps
  maxlength = maximum lengths
  Just i = elemIndex maxlength lengths
