import Data.List (sortBy, minimumBy, maximumBy, delete)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, isJust)

-- Definición de estructuras de datos
type Vertice = Int
type Arista = (Vertice, Vertice, Int)
data Grafo = Grafo [Vertice] [Arista] Bool deriving (Show, Eq)


-- Vecinos de un vértice
vecinos :: Grafo -> Vertice -> [Vertice]
vecinos (Grafo _ as dir) v
    | dir = [ y | (x, y, _) <- as, x == v ]
    | otherwise = [ y | (x, y, _) <- as, x == v ] ++ [ x | (x, y, _) <- as, y == v ]


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


-- Halla el número de clique de un grafo no dirigido (número máximo de vértices con aristas entre todos ellos)
numeroClique :: Grafo -> Int
numeroClique g@(Grafo vs as dir) = numeroCliqueAux g vs
    where
        numeroCliqueAux _ [] = 0
        numeroCliqueAux grafo (v:vs) = 
            let conVecinos = filter (`elem` vecinos grafo v) vs
            in 1 + numeroCliqueAux grafo conVecinos


-- Implementacion de Kruskal
kruskal :: Grafo -> [Arista]
kruskal (Grafo vs aristas _) = kruskalAux sortedAristas initialParent []
  where
    -- Ordenamos las aristas por peso
    sortedAristas = sortBy (\(_,_,w1) (_,_,w2) -> compare w1 w2) aristas
    -- Inicializamos los padres de cada vértice
    initialParent = [(v,v) | v <- vs]

    -- Función auxiliar principal
    kruskalAux [] _ mst = mst
    kruskalAux (e@(u,v,_):es) parent mst
      | find u parent /= find v parent = kruskalAux es (union u v parent) (e:mst)
      | otherwise = kruskalAux es parent mst

    -- Find con path compression
    find v parent = if p == v then v else find p parent
      where
        p = lookupParent v parent

    lookupParent v [] = v
    lookupParent v ((x,p):ps) = if x == v then p else lookupParent v ps

    -- Union de dos componentes
    union u v parent = map (\(x,p) -> if find u parent == p then (x, find v parent) else (x,p)) parent


-- Implementación de PRIM
prim :: Grafo -> [Arista]
prim (Grafo [] _ _) = []
prim (Grafo (v:vs) aristas _) = primAux [v] vs [] 
  where
    -- Función auxiliar principal
    primAux :: [Vertice] -> [Vertice] -> [Arista] -> [Arista]
    primAux _ [] mst = mst  -- No quedan vertices por agregar
    primAux visited remaining mst =
      let
        -- Tomamos las aristas que conectan un vértice visitado con uno no visitado
        candidateEdges = [(u,v,w) | (u,v,w) <- aristas,
                                    (u `elem` visited && v `elem` remaining) ||
                                    (v `elem` visited && u `elem` remaining)]
      in
        if null candidateEdges
          then let newStart = head remaining
               in primAux (newStart : visited) (delete newStart remaining) mst
          else
            let (uMin,vMin,wMin) = minimumBy (comparing (\(_,_,w) -> w)) candidateEdges
                newVertex = if uMin `elem` visited then vMin else uMin
            in primAux (newVertex : visited) (delete newVertex remaining) ((uMin,vMin,wMin):mst)


-- Verifica si un grafo es euleriano
isEuleriano :: Grafo -> Bool
isEuleriano g@(Grafo vs as dir)
    | dir = all even gradosEntrantes && all even gradosSalientes
    | otherwise = all even grados && isConexo g
    where
        grados = [length (vecinos g v) | v <- vs]
        gradosEntrantes = [length [ x | (x, y, _) <- as, y == v ] | v <- vs]
        gradosSalientes = [length [ y | (x, y, _) <- as, x == v ] | v <- vs]


-- Calcula in-degree de un vértice (si el grafo es no dirigido, devuelve el grado)
inDegree :: Grafo -> Vertice -> Int
inDegree g@(Grafo _ as dir) v
  | dir = length [ () | (_, y, _) <- as, y == v ]
  | otherwise = length (vecinos g v)

-- Calcula out-degree de un vértice (si el grafo es no dirigido, devuelve el grado)
outDegree :: Grafo -> Vertice -> Int
outDegree g@(Grafo _ as dir) v
  | dir = length [ () | (x, _, _) <- as, x == v ]
  | otherwise = length (vecinos g v)


-- Encuentra el nodo con mayor out-degree (devuelve Nothing si no hay vértices)
nodoOutDegreeMaximo :: Grafo -> Maybe (Vertice, Int)
nodoOutDegreeMaximo (Grafo [] _ _) = Nothing
nodoOutDegreeMaximo g@(Grafo vs _ _) = Just $ maximumBy (comparing snd) [(v, outDegree g v) | v <- vs]

-- Encuentra el nodo con menor out-degree (devuelve Nothing si no hay vértices)
nodoOutDegreeMinimo :: Grafo -> Maybe (Vertice, Int)
nodoOutDegreeMinimo (Grafo [] _ _) = Nothing
nodoOutDegreeMinimo g@(Grafo vs _ _) = Just $ minimumBy (comparing snd) [(v, outDegree g v) | v <- vs]


-- Encuentra el nodo con mayor in-degree (devuelve Nothing si no hay vértices)
nodoInDegreeMaximo :: Grafo -> Maybe (Vertice, Int)
nodoInDegreeMaximo (Grafo [] _ _) = Nothing
nodoInDegreeMaximo g@(Grafo vs _ _) = Just $ maximumBy (comparing snd) [(v, inDegree g v) | v <- vs]

-- Encuentra el nodo con menor in-degree (devuelve Nothing si no hay vértices)
nodoInDegreeMinimo :: Grafo -> Maybe (Vertice, Int)
nodoInDegreeMinimo (Grafo [] _ _) = Nothing
nodoInDegreeMinimo g@(Grafo vs _ _) = Just $ minimumBy (comparing snd) [(v, inDegree g v) | v <- vs]


-- Verifica si un grafo es hmiltoniano
esHamiltoniano :: Grafo -> Bool
esHamiltoniano (Grafo [] _ _) = False
esHamiltoniano (Grafo vs aristas _) = hamiltonAux [v0] (length vs)
  where
    v0 = head vs

    -- Verifica si hay arista entre dos vértices (no dirigido)
    adyacente u v =
      (u, v, 0) `elem` sinPeso || (v, u, 0) `elem` sinPeso
      where
        sinPeso = [(x,y,0) | (x,y,_) <- aristas]

    -- Backtracking
    hamiltonAux :: [Vertice] -> Int -> Bool
    hamiltonAux camino n
      | length camino == n =
          adyacente (last camino) (head camino)  -- cerrar ciclo
      | otherwise =
          or [ hamiltonAux (camino ++ [v]) n
             | v <- vs
             , v `notElem` camino
             , adyacente (last camino) v
             ]


-- Halla las aristas puentes en un grafo no dirigido
aristasPuente :: Grafo -> [Arista]
aristasPuente (Grafo vs as dir)
    | dir = []  -- No se consideran puentes en grafos dirigidos
    | otherwise = snd (dfsPuente (-1) (head vs) 0 [] [] [])
  where
    -- Vecinos sin peso
    vecinos v = [ y | (x,y,_) <- as, x == v ] ++
                [ x | (x,y,_) <- as, y == v ]

    -- Buscar valor en lista de pares
    buscar v ((x,t):xs)
      | v == x    = t
      | otherwise = buscar v xs
    buscar _ [] = -1

    -- DFS principal
    dfsPuente padre u tiempo disc low puentes =
      let
        discU = tiempo
        lowU  = tiempo
        disc' = (u, discU) : disc
        low'  = (u, lowU) : low
      in
        explorar u padre (vecinos u) (tiempo + 1) disc' low' puentes

    -- Explora vecinos
    explorar _ _ [] _ disc low puentes = (low, puentes)

    explorar u padre (v:vs) tiempo disc low puentes
      | buscar v disc == -1 =
          let
            (low', puentes') = dfsPuente u v tiempo disc low puentes
            lowU = min (buscar u low') (buscar v low')
            low'' = (u, lowU) : filter ((/=u) . fst) low'
            esPuente = buscar v low' > buscar u disc
            puentesFinal =
              if esPuente
              then (u,v,0) : puentes'
              else puentes'
          in explorar u padre vs tiempo disc low'' puentesFinal

      | v /= padre =
          let
            lowU = min (buscar u low) (buscar v disc)
            low' = (u, lowU) : filter ((/=u) . fst) low
          in explorar u padre vs tiempo disc low' puentes

      | otherwise = explorar u padre vs tiempo disc low puentes


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
          | u `notElem` visitados' = dfsCiclo v visitados' u || go us visitados'
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
      let candidates = [ (v,d) | (v,d) <- ds, v `elem` unvis, Data.Maybe.isJust d ]
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


-- Orden topológico (Kahn). Retorna Nothing si no es dirigido o hay ciclos
ordenTopologico :: Grafo -> Maybe [Vertice]
ordenTopologico (Grafo vs as dir)
  | not dir = Nothing
  | otherwise = kahn [] initialQueue as
  where
    -- calcula in-degree de un vértice respecto a la lista de aristas
    inDegree v edges = length [ () | (_, y, _) <- edges, y == v ]

    initialQueue = [ v | v <- vs, inDegree v as == 0 ]

    -- Kahn: sorted acumula vértices en orden, queue es la lista de vértices con in-degree 0
    kahn sorted [] remainingEdges
      | null remainingEdges = Just (reverse sorted)
      | otherwise = Nothing  -- quedan aristas => ciclo
    kahn sorted (q:qs) remainingEdges =
      let sorted' = q : sorted
          -- aristas después de remover las salientes de q
          edges' = filter (\(u,_,_) -> u /= q) remainingEdges
          -- obtener adyacentes de q en la lista original de aristas (antes de filtrar)
          adyacentes = [ v | (u,v,_) <- remainingEdges, u == q ]
          -- nuevos vértices cuyo in-degree llega a 0 tras quitar aristas salientes de q
          nuevos = [ v | v <- adyacentes, inDegree v edges' == 0 ]
          queue' = qs ++ nuevos
      in kahn sorted' queue' edges'


-- Verifica si un grafo es DAG (dirigido y acíclico)
esDAG :: Grafo -> Bool
esDAG g = case ordenTopologico g of
  Just _  -> True
  Nothing -> False


-- Función principal para probar las implementaciones
main :: IO ()
main = do
    let vertices = [1,2,3,4,5,6,7,8]
    let aristas = [(1,2,1), (2,3,1), (3,4,1), (5,4,1), (5,6,1), (7,6,1), (7,8,1), (8,4,1)]
    let grafo = Grafo vertices aristas False
    
    --Jery
    let resultadoBFS = bfs grafo 5
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

    -- Ejemplo de orden topológico (grafo dirigido acíclico)
    let verticesD = [1,2,3,4,5,6]
    let aristasD = [(1,2,0),(1,3,0),(2,4,0),(3,4,0),(4,5,0),(5,6,0)]
    let grafoDir = Grafo verticesD aristasD True
    let ordenTop = ordenTopologico grafoDir
    let esDagGrafo = esDAG grafo
    let esDagGrafoDir = esDAG grafoDir
    putStrLn $ "Recorrido BFS desde 5: " ++ show resultadoBFS
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
    let mstGraph = Grafo vertices kruskalResult False
    putStrLn $ "Peso MST: " ++ show (pesoTotal mstGraph)
    case dijkstraResult of
      Nothing -> putStrLn "Dijkstra: vértice origen no existe o hay pesos negativos"
      Just ds -> putStrLn $ "Dijkstra distancias desde 1: " ++ show ds
    putStrLn $ "¿El grafo (no dirigido) es DAG? " ++ show esDagGrafo
    putStrLn $ "¿El grafo dirigido es DAG? " ++ show esDagGrafoDir
    putStrLn $ "Orden topológico (grafo dirigido): " ++ show ordenTop

    --Alex
    let resultadoDFS = dfs grafo 5
    let regular = isRegular grafo
    let independencia = numeroIndependencia grafo
    let clique = numeroClique grafo
    let (minG, maxG) = gradoMinMax grafo
    let arbolKruskal = kruskal grafo
    let arbolPrim = prim grafo
    let euleriano = isEuleriano grafo
    let hamiltoniano = esHamiltoniano grafo
    let aristaspuente = aristasPuente grafo
    putStrLn $ "Recorrido DFS desde 5: " ++ show resultadoDFS
    putStrLn $ "¿El grafo es regular? " ++ show regular
    putStrLn $ "Número de independencia del grafo: " ++ show independencia
    putStrLn $ "Número de clique del grafo: " ++ show clique
    putStrLn $ "Grado mínimo: " ++ show minG ++ ", Grado máximo: " ++ show maxG
    putStrLn $ "Árbol de expansión mínima (Kruskal): " ++ show arbolKruskal
    putStrLn $ "Árbol de expansión mínima (Prim): " ++ show arbolPrim
    putStrLn $ "¿El grafo es euleriano? " ++ show euleriano
    putStrLn $ "¿El grafo es hamiltoniano? " ++ show hamiltoniano
    putStrLn $ "Aristas puentes en el grafo: " ++ show aristaspuente
    putStrLn $ "inDegree (nodo 4) en grafoDir: " ++ show (inDegree grafoDir 1)
    putStrLn $ "outDegree (nodo 4) en grafoDir: " ++ show (outDegree grafoDir 1)
    let nodoInMax = nodoInDegreeMaximo grafoDir
    let nodoInMin = nodoInDegreeMinimo grafoDir
    putStrLn $ "Nodo con mayor in-degree: " ++ show nodoInMax
    putStrLn $ "Nodo con menor in-degree: " ++ show nodoInMin
    let nodoMax = nodoOutDegreeMaximo grafoDir
    let nodoMin = nodoOutDegreeMinimo grafoDir
    putStrLn $ "Nodo con mayor out-degree: " ++ show nodoMax
    putStrLn $ "Nodo con menor out-degree: " ++ show nodoMin
