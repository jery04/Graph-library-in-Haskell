import Data.List (sortBy, minimumBy, delete)
import Data.Ord (comparing)

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
    putStrLn $ "Recorrido BFS desde 5: " ++ show resultadoBFS
    putStrLn $ "¿El grafo es conexo? " ++ show conexo
    putStrLn $ "Número de componentes conexas: " ++ show numComponentes
    putStrLn $ "¿El grafo es bipartito? " ++ show esBip ++ " Conjunto 1: " ++ show conjunto1 ++ " Conjunto 2: " ++ show conjunto2
    putStrLn $ "Tamaño de la componente conexa más grande: " ++ show tamCompMax ++ " " ++ show compMax
    putStrLn $ "Tamaño de la componente conexa más pequeña: " ++ show tamCompMin ++ " " ++ show compMin
    putStrLn $ "¿El grafo tiene ciclos? " ++ show tieneCiclos
    putStrLn $ "¿El grafo es un árbol? " ++ show esArbolGrafo
    putStrLn $ "¿El grafo es un bosque? " ++ show esBosqueGrafo

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