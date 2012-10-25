data Vertex a = Vertex a [Vertex a] deriving (Show, Read, Eq, Ord)
 
walkGraph (Vertex v (e:es)) = v : walkGraph e ++ walkGraph (Vertex v es)
walkGraph (Vertex v []) = [v]