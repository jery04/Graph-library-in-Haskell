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
bfs g@(Grafo vs as dir) start = bfsAux [start] []
    where
        bfsAux [] visitados = visitados
        bfsAux (q:qs) visitados
            | q `elem` visitados = bfsAux qs visitados
            | otherwise = bfsAux (qs ++ nuevos) (visitados ++ [q])
            where nuevos = filter (`notElem` (visitados ++ [q])) (vecinos g q)


-- Verifica si un grafo no dirigido es conexo usando BFS
isConexo :: Grafo -> Bool
isConexo (Grafo [] _ _) = True
isConexo g@(Grafo (v:vs) _ _) =
    let visitados = bfs g v
    in all (`elem` visitados) (v:vs)

-- Implementación de DFS
dfs :: Grafo -> Vertice -> [Vertice]
dfs g@(Grafo vs as dir) start = dfsAux [start] []
    where
        dfsAux [] visitados = visitados
        dfsAux (s:ss) visitados
            | s `elem` visitados = dfsAux ss visitados
            | otherwise = dfsAux (vecinos g s ++ ss) (visitados ++ [s])

-- Halla el grado mínimo y máximo de un grafo no dirigido
gradoMinMax :: Grafo -> (Int, Int)
gradoMinMax g@(Grafo vs as dir) = (minimum grados, maximum grados)
    where
        grados = [length (vecinos g v) | v <- vs]

-- Verifica si un grafo no dirigido es regular
isRegular :: Grafo -> Bool
isRegular grafo = 
    let (minG, maxG) = gradoMinMax grafo
    in minG == maxG

-- Halla el número de independencia de un grafo no dirigido (número máximo de vértices sin aristas entre ellos)
numeroIndependencia :: Grafo -> Int
numeroIndependencia g@(Grafo vs as dir) = numeroIndependenciaAux g vs
    where
        numeroIndependenciaAux _ [] = 0
        numeroIndependenciaAux grafo (v:vs) = 
            let sinVecinos = filter (`notElem` vecinos grafo v) vs
            in 1 + numeroIndependenciaAux grafo sinVecinos


-- Implementacion de árbol de expansión mínima 

main :: IO ()
main = do
    let vertices = [1,2,3,4,5,6,7]
        aristas = [(1,2,1), (2,3,1), (3,4,1), (5,4,1), (7,6,1)]
        grafo = Grafo vertices aristas False
        resultadoBFS = bfs grafo 5
        resultadoDFS = dfs grafo 5
        conexo = isConexo grafo
        regular = isRegular grafo
        numInd = numeroIndependencia grafo
    putStrLn $ "Recorrido BFS desde 5: " ++ show resultadoBFS
    putStrLn $ "Recorrido DFS desde 5: " ++ show resultadoDFS
    putStrLn $ "¿El grafo es conexo? " ++ show conexo
    putStrLn $ "¿El grafo es regular? " ++ show regular
    putStrLn $ "Número de independencia del grafo: " ++ show numInd