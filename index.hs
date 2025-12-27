import Data.List (sortOn, minimumBy)
import Data.Maybe (fromMaybe)

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


-- Implementación de DFS
dfs :: Grafo -> Vertice -> [Vertice]
dfs (Grafo vs as dir) start = dfsAux [start] []
    where
        vecinos v
          | dir = [ y | (x, y, _) <- as, x == v ]
          | otherwise = [ y | (x, y, _) <- as, x == v ] ++ [ x | (x, y, _) <- as, y == v ]

        dfsAux [] visitados = visitados
        dfsAux (s:ss) visitados
            | s `elem` visitados = dfsAux ss visitados
            | otherwise = dfsAux (vecinos s ++ ss) (visitados ++ [s])


-- Verifica si un grafo no dirigido es conexo usando BFS
isConexo :: Grafo -> Bool
isConexo (Grafo [] _ _) = True
isConexo g@(Grafo (v:vs) _ _) =
    let visitados = bfs g v
    in all (`elem` visitados) (v:vs)


-- Detecta si un grafo tiene ciclos usando DFS
hasCiclos :: Grafo -> Bool
hasCiclos (Grafo [] _ _) = False
hasCiclos g@(Grafo vs as dir) = any (dfsCiclo (-1) []) vsNoVisitados
  where
    vsNoVisitados = filtraNoVisitados vs []
    filtraNoVisitados [] _ = []
    filtraNoVisitados (v:rest) visitados
      | v `elem` visitados = filtraNoVisitados rest visitados
      | otherwise = v : filtraNoVisitados rest (dfs g v ++ visitados)

    -- DFS modificado para detectar ciclos
    dfsCiclo :: Vertice -> [Vertice] -> Vertice -> Bool
    dfsCiclo padre visitados v =
      let vecinosV = if dir
                        then [ y | (x, y, _) <- as, x == v ]
                        else [ y | (x, y, _) <- as, x == v ] ++ [ x | (x, y, _) <- as, y == v ]
      in go vecinosV (v:visitados)
      where
        go [] _ = False
        go (u:us) visitados'
          | u `notElem` visitados' =
              if dfsCiclo v visitados' u then True else go us visitados'
          | dir = True  -- En grafos dirigidos, ciclo si se reencuentra un visitado
          | u /= padre = True  -- En no dirigidos, ciclo si se reencuentra un visitado que no es el padre
          | otherwise = go us visitados'


-- Calcula el número de componentes conexas usando DFS
componentesConexas :: Grafo -> Int
componentesConexas (Grafo [] _ _) = 0
componentesConexas g@(Grafo vs _ _) = go vs [] 0
  where
    go [] _ count = count
    go (v:rest) visitados count
      | v `elem` visitados = go rest visitados count
      | otherwise = let visitados' = dfs g v ++ visitados
                    in go rest visitados' (count + 1)


-- Retorna la cardinalidad y los nodos de la componente conexa más grande usando DFS
componenteConexaMaxima :: Grafo -> (Int, [Vertice])
componenteConexaMaxima (Grafo [] _ _) = (0, [])
componenteConexaMaxima g@(Grafo vs _ _) = go vs [] []
  where
    go [] _ maxComp = (length maxComp, maxComp)
    go (v:rest) visitados maxComp
      | v `elem` visitados = go rest visitados maxComp
      | otherwise =
          let comp = dfs g v
              visitados' = comp ++ visitados
              maxComp' = if length comp > length maxComp then comp else maxComp
          in go rest visitados' maxComp'


-- Retorna la cardinalidad y los nodos de la componente conexa más pequeña usando DFS
componenteConexaMinima :: Grafo -> (Int, [Vertice])
componenteConexaMinima (Grafo [] _ _) = (0, [])
componenteConexaMinima g@(Grafo vs _ _) = go vs [] Nothing
  where
    go [] _ Nothing = (0, [])
    go [] _ (Just minComp) = (length minComp, minComp)
    go (v:rest) visitados minComp =
      if v `elem` visitados then go rest visitados minComp
      else
        let comp = dfs g v
            visitados' = comp ++ visitados
            minComp' = case minComp of
                         Nothing -> Just comp
                         Just c  -> if length comp < length c then Just comp else minComp
        in go rest visitados' minComp'


-- Verifica si un grafo es bipartito
esBipartito :: Grafo -> (Bool, [Vertice], [Vertice])
esBipartito (Grafo [] _ _) = (True, [], [])
esBipartito g@(Grafo vs as dir) = verificarComponentes vs []
  where
    -- Obtiene los vecinos de un vértice
    vecinos v
      | dir = [ y | (x, y, _) <- as, x == v ]
      | otherwise = [ y | (x, y, _) <- as, x == v ] ++ [ x | (x, y, _) <- as, y == v ]
    
    -- Busca el color asignado a un vértice en la lista de colores
    buscarColor :: Vertice -> [(Vertice, Int)] -> Maybe Int
    buscarColor _ [] = Nothing
    buscarColor v ((w, c):resto)
      | v == w = Just c
      | otherwise = buscarColor v resto
    
    -- BFS para colorear el grafo, retorna Nothing si hay conflicto, Just colores si es válido
    bfsColorear :: [Vertice] -> [(Vertice, Int)] -> Maybe [(Vertice, Int)]
    bfsColorear [] colores = Just colores
    bfsColorear (v:cola) colores =
      case buscarColor v colores of
        Nothing -> bfsColorear cola colores  -- Ya procesado en otra iteración
        Just colorV ->
          let vecinosV = vecinos v
              colorNuevo = 1 - colorV
          in procesarVecinos vecinosV colorNuevo cola colores
      where
        procesarVecinos [] _ cola' colores' = bfsColorear cola' colores'
        procesarVecinos (u:us) colorU cola' colores' =
          case buscarColor u colores' of
            Nothing ->  -- No coloreado aún, asignar color opuesto
              procesarVecinos us colorU (cola' ++ [u]) ((u, colorU):colores')
            Just c ->
              if c == colorU
              then procesarVecinos us colorU cola' colores'  -- Color correcto
              else Nothing  -- Conflicto de color, no es bipartito
    
    -- Colorea una componente conexa empezando desde un vértice
    colorearComponente :: Vertice -> [(Vertice, Int)] -> Maybe [(Vertice, Int)]
    colorearComponente inicio coloresActuales =
      bfsColorear [inicio] ((inicio, 0):coloresActuales)
    
    -- Verifica todas las componentes conexas
    verificarComponentes :: [Vertice] -> [(Vertice, Int)] -> (Bool, [Vertice], [Vertice])
    verificarComponentes [] colores = 
      let conjunto0 = [v | (v, c) <- colores, c == 0]
          conjunto1 = [v | (v, c) <- colores, c == 1]
      in (True, conjunto0, conjunto1)
    verificarComponentes (v:resto) colores =
      case buscarColor v colores of
        Just _ -> verificarComponentes resto colores  -- Ya coloreado
        Nothing -> 
          case colorearComponente v colores of
            Nothing -> (False, [], [])  -- No es bipartito
            Just nuevosColores -> verificarComponentes resto nuevosColores


-- Verifica si un grafo es un árbol: conexo y sin ciclos
esArbol :: Grafo -> Bool
esArbol g = isConexo g && not (hasCiclos g)


-- Verifica si un grafo es un bosque (todas sus componentes conexas son árboles)
esBosque :: Grafo -> Bool
esBosque (Grafo [] _ _) = True
esBosque g@(Grafo vs _ _) = all esArbol (componentes g)
  where
    -- Obtiene las componentes conexas como subgrafos
    componentes :: Grafo -> [Grafo]
    componentes (Grafo [] _ _) = []
    componentes gr@(Grafo vs as dir) = go vs []
      where
        go [] _ = []
        go (v:rest) visitados
          | v `elem` visitados = go rest visitados
          | otherwise =
              let comp = dfs gr v
                  subg = Grafo comp [a | a@(x,y,_) <- as, x `elem` comp && y `elem` comp] dir
                  visitados' = comp ++ visitados
              in subg : go rest visitados'


-- Retorna la arista con menor peso en el grafo
aristaMenor :: Grafo -> Maybe Arista
aristaMenor (Grafo _ [] _) = Nothing
aristaMenor (Grafo _ as _) = Just $ foldl1 minPorPeso as
  where
    minPorPeso a@(_,_,wa) b@(_,_,wb) = if wa <= wb then a else b


-- Retorna la arista con mayor peso en el grafo 
aristaMayor :: Grafo -> Maybe Arista
aristaMayor (Grafo _ [] _) = Nothing
aristaMayor (Grafo _ as _) = Just $ foldl1 maxPorPeso as
  where
    maxPorPeso a@(_,_,wa) b@(_,_,wb) = if wa >= wb then a else b


-- Kruskal: retorna Just árbol de expansión mínima si existe 
kruskal :: Grafo -> Maybe Grafo
kruskal (Grafo vs as dir)
  | dir = Nothing
  | null vs = Just (Grafo [] [] False)
  | otherwise =
      let sorted = sortOn (\(_,_,w) -> w) as
          initialSets = map (\v -> [v]) vs
          (mstEdges, _) = foldl step ([], initialSets) sorted
          tree = Grafo vs mstEdges False
      in if length mstEdges == (length vs - 1) && esArbol tree
           then Just tree
           else Nothing
  where
    step :: ([Arista], [[Vertice]]) -> Arista -> ([Arista], [[Vertice]])
    step (mst, sets) e@(u,v,_) =
      let setU = findSet u sets
          setV = findSet v sets
      in if setU /= setV
           then (mst ++ [e], unionSets setU setV sets)
           else (mst, sets)

    findSet :: Vertice -> [[Vertice]] -> [Vertice]
    findSet x sets = head [s | s <- sets, x `elem` s]

    unionSets :: [Vertice] -> [Vertice] -> [[Vertice]] -> [[Vertice]]
    unionSets s1 s2 sets = (s1 ++ s2) : filter (\s -> not (s == s1 || s == s2)) sets


-- Calcula el peso total de un grafo (suma de pesos de sus aristas)
pesoTotal :: Grafo -> Int
pesoTotal (Grafo _ as _) = sum [w | (_,_,w) <- as]


-- Dijkstra
dijkstra :: Grafo -> Vertice -> Maybe [(Vertice, Maybe Int)]
dijkstra (Grafo vs as dir) source
  | source `notElem` vs = Nothing
  | any (\(_,_,w) -> w < 0) as = Nothing
  | otherwise = Just $ map (\v -> (v, fromMaybe Nothing (lookup v finalDists))) vs
  where
    -- Adyacencia que respeta si el grafo es dirigido o no
    adj :: Vertice -> [(Vertice, Int)]
    adj v
      | dir = [ (y,w) | (x,y,w) <- as, x == v ]
      | otherwise = [ (y,w) | (x,y,w) <- as, x == v ] ++ [ (x,w) | (x,y,w) <- as, y == v ]

    -- Distancias iniciales (Nothing = infinito)
    initDists :: [(Vertice, Maybe Int)]
    initDists = [ (v, if v == source then Just 0 else Nothing) | v <- vs ]

    finalDists :: [(Vertice, Maybe Int)]
    finalDists = dijkstraLoop [] initDists

    dijkstraLoop :: [Vertice] -> [(Vertice, Maybe Int)] -> [(Vertice, Maybe Int)]
    dijkstraLoop visited dists =
      let unvisited = filter (`notElem` visited) vs
          candidate = selectMin unvisited dists
      in case candidate of
           Nothing -> dists
           Just u ->
             let distU = case lookup u dists of
                           Just (Just val) -> Just val
                           _ -> Nothing
                 neighbors = adj u
                 dists' = case distU of
                            Nothing -> dists
                            Just du -> foldl (relax du) dists neighbors
             in dijkstraLoop (u:visited) dists'

    selectMin :: [Vertice] -> [(Vertice, Maybe Int)] -> Maybe Vertice
    selectMin unvis ds =
      let candidates = [ (v,d) | (v,d) <- ds, v `elem` unvis, d /= Nothing ]
      in if null candidates then Nothing else Just $ fst $ minimumBy cmp candidates
      where
        cmp (_, Just a) (_, Just b) = compare a b
        cmp _ _ = EQ

    relax :: Int -> [(Vertice, Maybe Int)] -> (Vertice, Int) -> [(Vertice, Maybe Int)]
    relax du dists (v,w) = updateDistance dists v (Just (du + w))

    updateDistance :: [(Vertice, Maybe Int)] -> Vertice -> Maybe Int -> [(Vertice, Maybe Int)]
    updateDistance ds v newD = map update ds
      where
        update (x,d)
          | x /= v = (x,d)
          | otherwise = case (d, newD) of
                          (Nothing, nd) -> (x, nd)
                          (Just old, Nothing) -> (x, Just old)
                          (Just old, Just nd) -> if nd < old then (x, Just nd) else (x, Just old)


-- Función principal para probar las implementaciones
main :: IO ()
main = do
    let vertices = [1,2,3,4,5,6]
    let aristas = [(1,2,1), (2,4,2), (2,5,3), (5,4,4), (1,3,5), (3,6,6)]
    let grafo = Grafo vertices aristas False
    let resultadoBFS = bfs grafo 5
    let resultadoDFS = dfs grafo 5
    let conexo = isConexo grafo
    let numComponentes = componentesConexas grafo
    let (esBip, conjunto1, conjunto2) = esBipartito grafo
    let (tamCompMax, compMax) = componenteConexaMaxima grafo
    let (tamCompMin, compMin) = componenteConexaMinima grafo
    let tieneCiclos = hasCiclos grafo
    let esArbolGrafo = esArbol grafo
    let esBosqueGrafo = esBosque grafo
    let aristaMin = aristaMenor grafo
    let aristaMax = aristaMayor grafo
    let pesoGraph = pesoTotal grafo
    let kruskalResult = kruskal grafo
    let dijkstraResult = dijkstra grafo 1
    putStrLn $ "Recorrido BFS desde 5: " ++ show resultadoBFS
    putStrLn $ "Recorrido DFS desde 5: " ++ show resultadoDFS
    putStrLn $ "¿El grafo es conexo? " ++ show conexo
    putStrLn $ "Número de componentes conexas: " ++ show numComponentes
    putStrLn $ "¿El grafo es bipartito? " ++ show esBip ++ " Conjunto 1: " ++ show conjunto1 ++ " Conjunto 2: " ++ show conjunto2
    putStrLn $ "Tamaño de la componente conexa más grande: " ++ show tamCompMax ++ " " ++ show compMax
    putStrLn $ "Tamaño de la componente conexa más pequeña: " ++ show tamCompMin ++ " " ++ show compMin
    putStrLn $ "¿El grafo tiene ciclos? " ++ show tieneCiclos
    putStrLn $ "¿El grafo es un árbol? " ++ show esArbolGrafo
    putStrLn $ "¿El grafo es un bosque? " ++ show esBosqueGrafo
    putStrLn $ "Arista de menor peso: " ++ show aristaMin
    putStrLn $ "Arista de mayor peso: " ++ show aristaMax
    putStrLn $ "Peso total: " ++ show pesoGraph
    putStrLn $ "Kruskal (MST): " ++ show kruskalResult
    case kruskalResult of
      Just t -> putStrLn $ "Peso MST: " ++ show (pesoTotal t)
      Nothing -> putStrLn "Kruskal: No se pudo construir MST (grafo no conexo)"
    case dijkstraResult of
      Nothing -> putStrLn "Dijkstra: vértice origen no existe o hay pesos negativos"
      Just ds -> putStrLn $ "Dijkstra distancias desde 1: " ++ show ds
