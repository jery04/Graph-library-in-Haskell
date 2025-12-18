type Vertice = Int
type Arista = (Vertice, Vertice, Int)
data Grafo = Grafo [Vertice] [Arista] Bool deriving (Show, Eq)

-- Vecinos de un vértice
vecinos :: Grafo -> Vertice -> [Vertice]
vecinos (Grafo _ as dir) v
    | dir = [ y | (x, y, _) <- as, x == v ]
    | otherwise = [ y | (x, y, _) <- as, x == v ] ++ [ x | (x, y, _) <- as, y == v ]

-- Implementación de BFS
bfs :: Grafo -> Vertice -> [Vertice]
bfs (Grafo vs as dir) start = bfsAux [start] []
    where
        bfsAux [] visitados = visitados
        bfsAux (q:qs) visitados
            | q `elem` visitados = bfsAux qs visitados
            | otherwise = bfsAux (qs ++ nuevos) (visitados ++ [q])
            where nuevos = filter (`notElem` (visitados ++ [q])) (vecinos (Grafo vs as dir) q)


-- Verifica si un grafo no dirigido es conexo usando BFS
isConexo :: Grafo -> Bool
isConexo (Grafo [] _ _) = True
isConexo g@(Grafo (v:vs) _ _) =
    let visitados = bfs g v
    in all (`elem` visitados) (v:vs)

-- Implementación de DFS
dfs :: Grafo -> Vertice -> [Vertice]
dfs (Grafo vs as dir) start = dfsAux [start] []
    where
        dfsAux [] visitados = visitados
        dfsAux (s:ss) visitados
            | s `elem` visitados = dfsAux ss visitados
            | otherwise = dfsAux (vecinos (Grafo vs as dir) s ++ ss) (visitados ++ [s])

main :: IO ()
main = do
    let vertices = [1,2,3,4,5,6]
        aristas = [(1,2,1), (2,4,1), (2,5,1), (5,4,1), (1,3,1), (3,6,1)]
        grafo = Grafo vertices aristas False
        resultadoBFS = bfs grafo 5
        resultadoDFS = dfs grafo 5
        conexo = isConexo grafo
    putStrLn $ "Recorrido BFS desde 5: " ++ show resultadoBFS
    putStrLn $ "Recorrido DFS desde 5: " ++ show resultadoDFS
    putStrLn $ "¿El grafo es conexo? " ++ show conexo