type Vertice = Int
data Arista = Arista Vertice Vertice (Maybe Int) 
data Grafo = Grafo [Vertice] [Arista] Bool deriving (Show, Eq)

-- Implementación de BFS
bfs :: Grafo -> Arista -> [Arista]
bfs (Grafo vs as) start = bfsAux [start] []
    where
        vecinos v = [y | (x, y) <- as, x == v] ++ [x | (x, y) <- as, y == v]
        bfsAux [] visitados = visitados
        bfsAux (q:qs) visitados
            | q `elem` visitados = bfsAux qs visitados
            | otherwise = bfsAux (qs ++ nuevos) (visitados ++ [q])
            where nuevos = filter (`notElem` visitados) (vecinos q)


-- Verifica si un grafo es conexo usando BFS
isConexo :: Grafo -> Bool
isConexo (Grafo [] _) = True
isConexo g@(Grafo (v:vs) _) =
    let visitados = bfs g v
    in all (`elem` visitados) (v:vs)

main :: IO ()
main = do
    let vertices = [1,2,3,4,5,6]
        aristas = [(1,2), (2,4), (2,5), (5,4), (1,3), (3,6)]
        grafo = Grafo vertices aristas
        resultadoBFS = bfs grafo 5
        conexo = isConexo grafo
    putStrLn $ "Recorrido BFS desde 5: " ++ show resultadoBFS
    putStrLn $ "¿El grafo es conexo? " ++ show conexo