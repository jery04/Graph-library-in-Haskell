type Vertice = Int
type Arista = (Vertice, Vertice, Int)
data Grafo = Grafo [Vertice] [Arista] Bool deriving (Show, Eq)

-- Implementación de BFS
bfs :: Grafo -> Vertice -> [Vertice]
bfs (Grafo vs as dir) start = bfsAux [start] []
    where
        vecinos v
          | dir = [ y | (x, y, _) <- as, x == v ]
          | otherwise = [ y | (x, y, _) <- as, x == v ] ++ [ x | (x, y, _) <- as, y == v ]

        bfsAux [] visitados = visitados
        bfsAux (q:qs) visitados
            | q `elem` visitados = bfsAux qs visitados
            | otherwise = bfsAux (qs ++ nuevos) (visitados ++ [q])
            where nuevos = filter (`notElem` (visitados ++ [q])) (vecinos q)


-- Verifica si un grafo no dirigido es conexo usando BFS
isConexo :: Grafo -> Bool
isConexo (Grafo [] _ _) = True
isConexo g@(Grafo (v:vs) _ _) =
    let visitados = bfs g v
    in all (`elem` visitados) (v:vs)

-- Implementación de DFS
-- dfs :: Grafo -> Vertice -> [Vertice]
-- dfs (Grafo vs as dir) start = dfsAux [start] []

main :: IO ()
main = do
    let vertices = [1,2,3,4,5,6]
        aristas = [(1,2,1), (2,4,1), (2,5,1), (5,4,1), (1,3,1), (3,6,1)]
        grafo = Grafo vertices aristas True
        resultadoBFS = bfs grafo 5
        conexo = isConexo grafo
    putStrLn $ "Recorrido BFS desde 5: " ++ show resultadoBFS
    putStrLn $ "¿El grafo es conexo? " ++ show conexo