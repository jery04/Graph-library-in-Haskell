module Main where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG
import Graphics.UI.Threepenny ( (#+), liftIO )
import Graph
import Data.List (nub, intercalate, nubBy)
import Text.Read (readMaybe)
import Control.Monad (forM, forM_, when, void)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Numeric (showFFloat)
import Data.IORef (newIORef, readIORef, writeIORef)

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: UI.Window -> UI.UI ()
setup window = do
    -- Estilos CSS
    let css = "body { font-family: Arial, sans-serif; background-color: #f0f8ff; color: #333; margin: 20px; }" ++
              "h1 { color: #4CAF50; text-align: center; }" ++
              "input, textarea { width: 100%; padding: 8px; margin: 5px 0; box-sizing: border-box; }" ++
              "button { background-color: #4CAF50; color: white; border: none; padding: 10px 20px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer; border-radius: 4px; }" ++
              "button:hover { background-color: #45a049; }" ++
              "svg { border: 1px solid #ddd; background-color: white; margin: 20px auto; display: block; }" ++
              "circle { fill: #FF5722; stroke: #333; stroke-width: 2; }" ++
              "circle:hover { fill: #FF9800; }" ++
              "line { stroke: #333; stroke-width: 2; }" ++
              "text { font-family: Arial; font-size: 14px; fill: white; text-anchor: middle; dominant-baseline: middle; }" ++
              ".form-group { margin-bottom: 15px; }" ++
              "label { display: block; margin-bottom: 5px; font-weight: bold; }" ++
              ".graph-container { position: relative; }" ++
              ".back-button { position: absolute; top: 10px; left: 10px; background-color: #222; color: #fff; padding: 8px 12px; border-radius: 4px; }" ++
              ".back-button:hover { background-color: #444; }" ++
              ".algo-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 10px; margin: 16px 20px; }" ++
              ".algo-btn { background-color: #1976d2; color: #fff; padding: 10px; border-radius: 6px; text-align: center; }" ++
              ".algo-btn:hover { background-color: #145ea8; }" ++
              ".algo-result { margin: 0 20px 20px; padding: 10px; border: 1px solid #ddd; border-radius: 6px; background: #f8f9fb; color: #222; }" ++
              ".start-controls { margin: 10px 20px; display: flex; gap: 10px; align-items: center; justify-content: center; }" ++
              ".start-controls input { width: 90px; padding: 6px; text-align: center; -moz-appearance: textfield; }" ++
              ".start-controls input::-webkit-outer-spin-button, .start-controls input::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }" ++
              ".algo-graph { margin: 0 20px 20px; }"
    
    styleElement <- UI.mkElement "style" UI.# UI.set UI.text css
    
    -- Título
    title <- UI.h1 UI.# UI.set UI.text "Constructor de Grafos"
    
    -- Formulario
    numNodesInput <- UI.input UI.# UI.set UI.type_ "number" UI.# UI.set (UI.attr "placeholder") "Número de nodos"
    
    edgesTextarea <- UI.textarea UI.# UI.set (UI.attr "placeholder") "Aristas (formato: nodo1-nodo2:peso, separadas por coma)\nEjemplo: 1-2:5,2-3:3,1-3:2"
    
    directedCheckbox <- UI.input UI.# UI.set UI.type_ "checkbox"
    directedLabel <- UI.label UI.# UI.set UI.text "Dirigido:"
    
    createButton <- UI.button UI.# UI.set UI.text "Crear Grafo"
    
    messageDiv <- UI.div
    algoResult <- UI.div UI.# UI.set UI.class_ "algo-result"
    miniGraphDiv <- UI.div UI.# UI.set UI.class_ "algo-graph"
    lastGraphRef <- liftIO $ newIORef Nothing
    startNodeInput <- UI.input UI.# UI.set UI.type_ "number" UI.# UI.set (UI.attr "placeholder") "Nodo inicio"
    startLabel <- UI.label UI.# UI.set UI.text "Nodo inicio:"
    startControls <- UI.div UI.# UI.set UI.class_ "start-controls" #+ [return startLabel, return startNodeInput]

    -- Contenedores de vista
    formContainer <- UI.div
    graphContent <- UI.div
    backButton <- UI.button UI.# UI.set UI.text "← Atrás" UI.# UI.set UI.class_ "back-button"
    graphContainer <- UI.div UI.# UI.set UI.class_ "graph-container" UI.# UI.set (UI.attr "style") "display:none" #+ [return backButton, return graphContent]

    -- Botones de algoritmos (dos columnas)
    let showResult msg = do
            UI.element algoResult UI.# UI.set UI.text msg
            UI.element miniGraphDiv UI.# UI.set UI.children []
            return ()
        setMiniGraph el = void $ UI.element miniGraphDiv UI.# UI.set UI.children [el]
        requireGraph k = do
            mg <- liftIO $ readIORef lastGraphRef
            case mg of
                Nothing -> showResult "Primero crea un grafo."
                Just g -> k g
        getStart g@(Grafo vs _ _) = do
            val <- UI.get UI.value startNodeInput
            let mnum = readMaybe val :: Maybe Int
            return $ case mnum of
                Just n | n `elem` vs -> n
                _ -> case vs of
                        (v:_) -> v
                        [] -> 1
        sameEdge (a,b,_) (c,d,_) = (min a b == min c d) && (max a b == max c d)
        dedupEdges = nubBy sameEdge
        showEdge (a,b,w) = show a ++ "-" ++ show b ++ ":" ++ show w
        showEdges es = intercalate "; " (map showEdge es)
        showMaybeEdge Nothing = "N/A"
        showMaybeEdge (Just e) = showEdge e
        makeBtn label action = do
            btn <- UI.button UI.# UI.set UI.text label UI.# UI.set UI.class_ "algo-btn"
            UI.on UI.click btn $ \_ -> action
            return btn

    bfsBtn <- makeBtn "BFS" $ requireGraph $ \g -> do
        let Grafo vs _ _ = g
        case vs of
            [] -> showResult "Grafo vacío"
            _  -> do s <- getStart g
                     let recorrido = bfs g s
                     showResult ("BFS: " ++ show recorrido)

    dfsBtn <- makeBtn "DFS" $ requireGraph $ \g -> do
        let Grafo vs _ _ = g
        case vs of
            [] -> showResult "Grafo vacío"
            _  -> do s <- getStart g
                     let recorrido = dfs g s
                     showResult ("DFS: " ++ show recorrido)

    dijkstraBtn <- makeBtn "Dijkstra" $ requireGraph $ \g -> do
        let Grafo vs _ _ = g
        case vs of
            [] -> showResult "Grafo vacío"
            _  -> do s <- getStart g
                     case dijkstra g s of
                        Nothing -> showResult "Dijkstra: nodo origen no existe o hay pesos negativos"
                        Just ds -> do
                            showResult ("Dijkstra distancias: " ++ show ds)
                            view <- renderDistancesView ds
                            setMiniGraph view

    kruskalBtn <- makeBtn "Kruskal" $ requireGraph $ \g -> do
        let arbol = kruskal g
        showResult ("Kruskal: " ++ showEdges arbol)
        view <- renderEdgeGraphView g arbol
        setMiniGraph view

    primBtn <- makeBtn "PRIM" $ requireGraph $ \g -> do
        let arbol = prim g
        showResult ("PRIM: " ++ showEdges arbol)
        view <- renderEdgeGraphView g arbol
        setMiniGraph view

    bipartitoBtn <- makeBtn "Es Bipartito" $ requireGraph $ \g -> do
        let (ok,a,b) = esBipartito g
        showResult ("Es Bipartito: " ++ show ok ++ " | L1=" ++ show a ++ " L2=" ++ show b)
        bip <- renderBipartiteView g a b
        setMiniGraph bip

    compMaxBtn <- makeBtn "Componente Conexa Maxima" $ requireGraph $ \g ->
        let (n,ns) = componenteConexaMaxima g in showResult ("Componente Conexa Maxima: " ++ show n ++ " " ++ show ns)

    compMinBtn <- makeBtn "Componente Conexa Minima" $ requireGraph $ \g ->
        let (n,ns) = componenteConexaMinima g in showResult ("Componente Conexa Minima: " ++ show n ++ " " ++ show ns)

    puentesBtn <- makeBtn "Aristas Puente" $ requireGraph $ \g ->
        let aps = dedupEdges (aristasPuente g)
        in showResult ("Aristas Puente: " ++ showEdges aps)

    topoBtn <- makeBtn "Orden Topologico" $ requireGraph $ \g ->
        case ordenTopologico g of
            Nothing   -> showResult "Orden Topologico: (ciclo o no dirigido)"
            Just ord  -> showResult ("Orden Topologico: " ++ show ord)

    gradoBtn <- makeBtn "Grado Minimo y Maximo" $ requireGraph $ \g ->
        let (mn,mx) = gradoMinMax g in showResult ("Grado Minimo y Maximo: " ++ show mn ++ " / " ++ show mx)

    regularBtn <- makeBtn "Es Regular" $ requireGraph $ \g ->
        showResult ("Es Regular: " ++ show (isRegular g))

    indepBtn <- makeBtn "Numero Independencia" $ requireGraph $ \g ->
        showResult ("Numero Independencia: " ++ show (numeroIndependencia g))

    cliqueBtn <- makeBtn "Numero Clique" $ requireGraph $ \g ->
        showResult ("Numero Clique: " ++ show (numeroClique g))

    eulerBtn <- makeBtn "Es Euleriano" $ requireGraph $ \g ->
        showResult ("Es Euleriano: " ++ show (isEuleriano g))

    hamBtn <- makeBtn "Es Hamiltoniano" $ requireGraph $ \g ->
        showResult ("Es Hamiltoniano: " ++ show (esHamiltoniano g))

    conexoBtn <- makeBtn "Es Conexo" $ requireGraph $ \g ->
        showResult ("Es Conexo: " ++ show (isConexo g))

    ciclosBtn <- makeBtn "Tiene Ciclos" $ requireGraph $ \g ->
        showResult ("Tiene Ciclos: " ++ show (hasCiclos g))

    compCountBtn <- makeBtn "Componentes Conexas" $ requireGraph $ \g ->
        showResult ("Componentes Conexas: " ++ show (componentesConexas g))

    arbolBtn <- makeBtn "Es Arbol" $ requireGraph $ \g ->
        showResult ("Es Arbol: " ++ show (esArbol g))

    bosqueBtn <- makeBtn "Es Bosque" $ requireGraph $ \g ->
        showResult ("Es Bosque: " ++ show (esBosque g))

    aristaExtBtn <- makeBtn "Arista Menor y Mayor" $ requireGraph $ \g ->
        let mn = aristaMenor g
            mx = aristaMayor g
        in showResult ("Arista Menor: " ++ showMaybeEdge mn ++ " | Mayor: " ++ showMaybeEdge mx)

    pesoBtn <- makeBtn "Peso Total" $ requireGraph $ \g ->
        showResult ("Peso Total: " ++ show (pesoTotal g))

    dagBtn <- makeBtn "Es DAG" $ requireGraph $ \g ->
        showResult ("Es DAG: " ++ show (esDAG g))

    algoGrid <- UI.div UI.# UI.set UI.class_ "algo-grid" #+ map return
        [ bfsBtn, dfsBtn, dijkstraBtn, kruskalBtn, primBtn
        , bipartitoBtn, compMaxBtn, compMinBtn, puentesBtn, topoBtn
        , gradoBtn, regularBtn, indepBtn, cliqueBtn, eulerBtn
        , hamBtn, conexoBtn, ciclosBtn, compCountBtn, arbolBtn
        , bosqueBtn, aristaExtBtn, pesoBtn, dagBtn
        ]
    
    -- Evento del botón
    UI.on UI.click createButton $ \_ -> do
        numNodesStr <- UI.get UI.value numNodesInput
        edgesStr <- UI.get UI.value edgesTextarea
        isDirected <- UI.get UI.checked directedCheckbox
        
        case parseInputs numNodesStr edgesStr isDirected of
            Left err -> UI.element messageDiv UI.# UI.set UI.text ("Error: " ++ err)
            Right grafo -> do
                svg <- renderGraph grafo
                liftIO $ writeIORef lastGraphRef (Just grafo)
                -- mostrar vista de grafo
                UI.element graphContent UI.# UI.set UI.children [svg, startControls, algoGrid, algoResult, miniGraphDiv]
                UI.element formContainer UI.# UI.set (UI.attr "style") "display:none"
                UI.element graphContainer UI.# UI.set (UI.attr "style") "display:block"
                UI.element messageDiv UI.# UI.set UI.text ""
                UI.element algoResult UI.# UI.set UI.text ""

    -- Botón atrás
    UI.on UI.click backButton $ \_ -> do
        UI.element graphContainer UI.# UI.set (UI.attr "style") "display:none"
        UI.element formContainer UI.# UI.set (UI.attr "style") "display:block"
        UI.element graphContent UI.# UI.set UI.children []
        liftIO $ writeIORef lastGraphRef Nothing
        UI.element algoResult UI.# UI.set UI.text ""
        UI.element miniGraphDiv UI.# UI.set UI.children []
    
    -- Layout
    UI.getBody window #+ [
        UI.element styleElement,
        UI.element title,
        UI.element formContainer #+ [
            UI.div UI.# UI.set UI.class_ "form-group" #+ [
                UI.label UI.# UI.set UI.text "Número de nodos:",
                UI.element numNodesInput
            ],
            UI.div UI.# UI.set UI.class_ "form-group" #+ [
                UI.label UI.# UI.set UI.text "Aristas:",
                UI.element edgesTextarea
            ],
            UI.div UI.# UI.set UI.class_ "form-group" #+ [
                UI.element directedLabel,
                UI.element directedCheckbox
            ],
            UI.element createButton,
            UI.element messageDiv
        ],
        UI.element graphContainer
        ]
    return ()

parseInputs :: String -> String -> Bool -> Either String Grafo
parseInputs numNodesStr edgesStr isDirected = do
    numNodes <- case readMaybe numNodesStr of
        Just n | n > 0 -> Right n
        _ -> Left "Número de nodos inválido"
    
    let vertices = [1..numNodes]
    
    edges <- parseEdges edgesStr
    
    let validEdges = filter (\(a,b,_) -> a `elem` vertices && b `elem` vertices) edges
    
    Right (Grafo vertices validEdges isDirected)

parseEdges :: String -> Either String [Arista]
parseEdges str = mapM parseEdge (filter (not . null) (map trim (splitOn ',' str)))
    where
        trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
        splitOn c s = case break (== c) s of
            (a, []) -> [a]
            (a, _:b) -> a : splitOn c b
        
        parseEdge edgeStr = case splitOn ':' edgeStr of
            [nodes, weightStr] -> case splitOn '-' nodes of
                [n1, n2] -> case (readMaybe n1, readMaybe n2, readMaybe weightStr) of
                    (Just a, Just b, Just w) -> Right (a, b, w)
                    _ -> Left ("Arista inválida: " ++ edgeStr)
                _ -> Left ("Formato de nodos inválido: " ++ nodes)
            _ -> Left ("Formato de arista inválido: " ++ edgeStr)

-- Función para calcular layout de grafo usando force-directed
forceDirectedLayout :: [Int] -> [Arista] -> [(Int, (Double, Double))]
forceDirectedLayout vertices edges = 
    let n = length vertices
        centerX = 300.0
        centerY = 225.0
        radius = 150.0
        angleStep = 2 * pi / fromIntegral n
        initialPositions = Map.fromList $ zip vertices $ map (\i -> let angle = fromIntegral i * angleStep
                                                                       in (centerX + radius * cos angle, centerY + radius * sin angle)) [0..n-1]
        -- Parámetros
        k = sqrt (600 * 450 / fromIntegral n)  -- ideal distance
        iterations = 50
        -- Función para calcular fuerzas
        calculateForces pos = Map.mapWithKey (\v p -> 
            let add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
                repulsive = foldl add (0,0) [ let d = dist p p' 
                                                  dx = fst p - fst p'
                                                  dy = snd p - snd p'
                                              in if d > 0 then (dx / d * k*k / d, dy / d * k*k / d) else (0,0)
                                            | v' <- vertices, v' /= v, let Just p' = Map.lookup v' pos ]
                attractive = foldl add (0,0) [ let Just p' = Map.lookup v' pos
                                                   d = dist p p'
                                                   dx = fst p' - fst p
                                                   dy = snd p' - snd p
                                               in if d > 0 then (dx / d * d*d / k, dy / d * d*d / k) else (0,0)
                                             | (a,b,_) <- edges, let (v1,v2) = if a == v then (a,b) else if b == v then (b,a) else (0,0), v' <- [v2 | v1 == v] ]
            in add repulsive attractive
            ) pos
        -- Iterar
        iterateLayout pos 0 = pos
        iterateLayout pos i = 
            let forces = calculateForces pos
                newPos = Map.mapWithKey (\v (x,y) -> 
                    let (fx,fy) = forces Map.! v
                        nx = x + fx * 0.1  -- damping
                        ny = y + fy * 0.1
                    in (max 50 (min 550 nx), max 50 (min 400 ny))  -- bounds
                    ) pos
            in if i >= iterations then newPos else iterateLayout newPos (i+1)
    in Map.toList $ iterateLayout initialPositions 0
    where
        dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

renderGraph :: Grafo -> UI.UI UI.Element
renderGraph (Grafo vertices edges isDirected) = do
    let fmt p = (\s -> if s == "" then "0" else s) . showFFloat (Just 2) p
        showPt (x,y) = fmt x "" ++ " " ++ fmt y ""
    let positions = forceDirectedLayout vertices edges
    
    svg <- SVG.svg UI.# UI.set (UI.attr "width") "600" UI.# UI.set (UI.attr "height") "450"
    
    forM_ edges $ \(a,b,w) -> do
        let Just (x1,y1) = lookup a positions
            Just (x2,y2) = lookup b positions
        let lineAttrs = [UI.set (UI.attr "x1") (show x1), UI.set (UI.attr "y1") (show y1), UI.set (UI.attr "x2") (show x2), UI.set (UI.attr "y2") (show y2), UI.set (UI.attr "stroke") "#222", UI.set (UI.attr "stroke-width") "2", UI.set (UI.attr "stroke-linecap") "round"]
        line <- foldl (UI.#) (SVG.line) lineAttrs
        _ <- UI.element svg #+ [return line]

        -- Dibujar flecha manual si es dirigido
        when (isDirected && (x1 /= x2 || y1 /= y2)) $ do
            let dx = x2 - x1
                dy = y2 - y1
                len = sqrt (dx*dx + dy*dy)
            when (len > 0.1) $ do
                let ux = dx / len
                    uy = dy / len
                    tipBack = 22
                    baseBack = 40
                    tipX = x2 - ux * tipBack  -- mueve la punta más atrás para que se vea completa
                    tipY = y2 - uy * tipBack
                    baseX = tipX - ux * baseBack
                    baseY = tipY - uy * baseBack
                    perp = 9
                    p1 = (tipX, tipY)
                    p2 = (baseX + (-uy)*perp, baseY + ux*perp)
                    p3 = (baseX + uy*perp, baseY - ux*perp)
                    pointsStr = intercalate " " (map showPt [p1, p2, p3])
                arrow <- SVG.polygon UI.# UI.set (UI.attr "points") pointsStr UI.# UI.set (UI.attr "fill") "#d00" UI.# UI.set (UI.attr "stroke") "#d00" UI.# UI.set (UI.attr "stroke-linejoin") "round"
                _ <- UI.element svg #+ [return arrow]
                return ()
        
        -- Agregar etiqueta de peso
        let dx = x2 - x1
            dy = y2 - y1
            len = sqrt (dx*dx + dy*dy)
            (mx,my) = if len > 0.1
                      then let ux = dx / len
                               uy = dy / len
                               offset = 48  -- coloca el peso hacia el extremo b para evitar solapes
                           in (x2 - ux * offset, y2 - uy * offset)
                      else ((x1 + x2) / 2, (y1 + y2) / 2)
        -- Fondo para peso (mejor contraste)
        let labelW = 24 :: Double
            labelH = 16 :: Double
            rx = 4 :: Double
        bg <- SVG.rect UI.# UI.set (UI.attr "x") (show (mx - labelW/2)) UI.# UI.set (UI.attr "y") (show (my - labelH/2)) UI.# UI.set (UI.attr "width") (show labelW) UI.# UI.set (UI.attr "height") (show labelH) UI.# UI.set (UI.attr "rx") (show rx) UI.# UI.set (UI.attr "ry") (show rx) UI.# UI.set (UI.attr "fill") "#ffe9b3" UI.# UI.set (UI.attr "stroke") "#444" UI.# UI.set (UI.attr "stroke-width") "1"
        weightText <- SVG.text UI.# UI.set (UI.attr "x") (show mx) UI.# UI.set (UI.attr "y") (show my) UI.# UI.set UI.text (show w) UI.# UI.set (UI.attr "text-anchor") "middle" UI.# UI.set (UI.attr "font-size") "12" UI.# UI.set (UI.attr "font-weight") "bold" UI.# UI.set (UI.attr "fill") "#111" UI.# UI.set (UI.attr "dominant-baseline") "middle"
        _ <- UI.element svg #+ [return bg, return weightText]
        return ()
    
    forM_ (take (length vertices) positions) $ \(n, (x,y)) -> do
        circle <- SVG.circle UI.# UI.set (UI.attr "cx") (show x) UI.# UI.set (UI.attr "cy") (show y) UI.# UI.set (UI.attr "r") "25" UI.# UI.set (UI.attr "fill") "lightblue" UI.# UI.set (UI.attr "stroke") "blue" UI.# UI.set (UI.attr "stroke-width") "2"
        text <- SVG.text UI.# UI.set (UI.attr "x") (show x) UI.# UI.set (UI.attr "y") (show (y+5)) UI.# UI.set UI.text (show n) UI.# UI.set (UI.attr "text-anchor") "middle" UI.# UI.set (UI.attr "font-size") "12"
        _ <- UI.element svg #+ [return circle, return text]
        return ()
    
    return svg


-- Renderizado bipartito para mostrar particiones
renderBipartiteView :: Grafo -> [Vertice] -> [Vertice] -> UI.UI UI.Element
renderBipartiteView (Grafo _ edges _) leftSet rightSet = do
    let w = 600 :: Double
        h = 320 :: Double
        lx = 140 :: Double
        rx = 460 :: Double
        y0 = 60  :: Double
        gap ns = if ns <= 1 then 0 else (h - 2*y0) / fromIntegral (ns - 1)
        pos sideX vs = zipWith (\i v -> (v, (sideX, y0 + gap (length vs) * fromIntegral i))) [0..] vs
        positions = pos lx leftSet ++ pos rx rightSet
        lookupPos v = lookup v positions
        crossEdges = [ (a,b,wgt) | (a,b,wgt) <- edges
                     , (a `elem` leftSet && b `elem` rightSet) || (a `elem` rightSet && b `elem` leftSet) ]

    svg <- SVG.svg UI.# UI.set (UI.attr "width") (show w) UI.# UI.set (UI.attr "height") (show h)

    -- Draw edges between partitions
    forM_ crossEdges $ \(a,b,_) -> case (lookupPos a, lookupPos b) of
        (Just (x1,y1), Just (x2,y2)) -> do
            line <- SVG.line UI.# UI.set (UI.attr "x1") (show x1) UI.# UI.set (UI.attr "y1") (show y1)
                                  UI.# UI.set (UI.attr "x2") (show x2) UI.# UI.set (UI.attr "y2") (show y2)
                                  UI.# UI.set (UI.attr "stroke") "#888" UI.# UI.set (UI.attr "stroke-width") "2"
            _ <- UI.element svg #+ [return line]
            return ()
        _ -> return ()

    -- Draw nodes as circles (matching main graph look)
    let drawNode isLeft (v,(x,y)) = do
            let r = 18 :: Double
                fillCol = if isLeft then "#e6f2ff" else "#ffe6e6"
                strokeCol = if isLeft then "#3b6ea5" else "#b85c5c"
            circ <- SVG.circle UI.# UI.set (UI.attr "cx") (show x)
                                 UI.# UI.set (UI.attr "cy") (show y)
                                 UI.# UI.set (UI.attr "r") (show r)
                                 UI.# UI.set (UI.attr "fill") fillCol
                                 UI.# UI.set (UI.attr "stroke") strokeCol UI.# UI.set (UI.attr "stroke-width") "2"
            label <- SVG.text UI.# UI.set (UI.attr "x") (show x) UI.# UI.set (UI.attr "y") (show y)
                                 UI.# UI.set UI.text (show v) UI.# UI.set (UI.attr "text-anchor") "middle"
                                 UI.# UI.set (UI.attr "dominant-baseline") "middle"
                                 UI.# UI.set (UI.attr "font-size") "12" UI.# UI.set (UI.attr "fill") "#111"
            _ <- UI.element svg #+ [return circ, return label]
            return ()

    mapM_ (drawNode True) (pos lx leftSet)
    mapM_ (drawNode False) (pos rx rightSet)

    return svg


-- Vista compacta de orden de visita (BFS/DFS/topológico)
renderOrderView :: String -> [Vertice] -> UI.UI UI.Element
renderOrderView _ [] = UI.div UI.# UI.set UI.text "Sin datos"
renderOrderView _ orden = do
    let n = length orden
        w = max 220 (80 * n + 40)
        h = 140 :: Double
        y = 70  :: Double
        pos i = 40 + fromIntegral i * 80
        triPoints x = let a = (x - 12, y - 7)
                          b = (x, y)
                          c = (x - 12, y + 7)
                      in showPoint a ++ " " ++ showPoint b ++ " " ++ showPoint c
        showPoint (px,py) = show px ++ "," ++ show py

    svg <- SVG.svg UI.# UI.set (UI.attr "width") (show w) UI.# UI.set (UI.attr "height") (show h)

    let indexed = zip orden [0..]
    forM_ (zip indexed (tail indexed)) $ \((( _ , i1), (_ , i2))) -> do
        let x1 = pos i1; x2 = pos i2
        line <- SVG.line UI.# UI.set (UI.attr "x1") (show x1) UI.# UI.set (UI.attr "y1") (show y)
                              UI.# UI.set (UI.attr "x2") (show x2) UI.# UI.set (UI.attr "y2") (show y)
                              UI.# UI.set (UI.attr "stroke") "#777" UI.# UI.set (UI.attr "stroke-width") "2"
        arrow <- SVG.polygon UI.# UI.set (UI.attr "points") (triPoints x2)
                                UI.# UI.set (UI.attr "fill") "#777"
        _ <- UI.element svg #+ [return line, return arrow]
        return ()

    forM_ indexed $ \(v,i) -> do
        let x = pos i
        circ <- SVG.circle UI.# UI.set (UI.attr "cx") (show x)
                             UI.# UI.set (UI.attr "cy") (show y)
                             UI.# UI.set (UI.attr "r") "18"
                             UI.# UI.set (UI.attr "fill") "#e8f0ff"
                             UI.# UI.set (UI.attr "stroke") "#2f6fdd" UI.# UI.set (UI.attr "stroke-width") "2"
        stepTxt <- SVG.text UI.# UI.set (UI.attr "x") (show x) UI.# UI.set (UI.attr "y") (show (y - 26))
                               UI.# UI.set UI.text ("#" ++ show (i+1))
                               UI.# UI.set (UI.attr "text-anchor") "middle"
                               UI.# UI.set (UI.attr "font-size") "10" UI.# UI.set (UI.attr "fill") "#444"
        label <- SVG.text UI.# UI.set (UI.attr "x") (show x) UI.# UI.set (UI.attr "y") (show y)
                              UI.# UI.set UI.text (show v) UI.# UI.set (UI.attr "text-anchor") "middle"
                              UI.# UI.set (UI.attr "dominant-baseline") "middle"
                              UI.# UI.set (UI.attr "font-size") "12" UI.# UI.set (UI.attr "fill") "#111"
        _ <- UI.element svg #+ [return circ, return stepTxt, return label]
        return ()

    return svg


-- Lista de aristas con pesos (Kruskal/Prim/Puentes)
renderEdgeListView :: String -> [Arista] -> UI.UI UI.Element
renderEdgeListView _ [] = UI.div UI.# UI.set UI.text "Sin aristas"
renderEdgeListView _ es = do
    let cellStyle = "padding:6px 10px; border-bottom:1px solid #ddd; text-align:left;"
        row (u,v,w) = UI.tr #+ [ UI.td UI.# UI.set UI.text (show u ++ " - " ++ show v) UI.# UI.set (UI.attr "style") cellStyle
                               , UI.td UI.# UI.set UI.text (show w) UI.# UI.set (UI.attr "style") cellStyle
                               ]
    header <- UI.tr #+ [ UI.th UI.# UI.set UI.text "Arista" UI.# UI.set (UI.attr "style") cellStyle
                        , UI.th UI.# UI.set UI.text "Peso"   UI.# UI.set (UI.attr "style") cellStyle
                        ]
    table <- UI.table UI.# UI.set (UI.attr "style") "width:100%; border-collapse:collapse; table-layout:fixed; text-align:left;"
    _ <- UI.element table #+ (return header : map row es)
    return table


-- Distancias de Dijkstra
renderDistancesView :: [(Vertice, Maybe Int)] -> UI.UI UI.Element
renderDistancesView [] = UI.div UI.# UI.set UI.text "Sin distancias"
renderDistancesView ds = do
    let cellStyle = "padding:6px 10px; border-bottom:1px solid #ddd; text-align:left;"
        showD Nothing = "∞"
        showD (Just d) = show d
        row (v,d) = UI.tr #+ [ UI.td UI.# UI.set UI.text (show v) UI.# UI.set (UI.attr "style") cellStyle
                             , UI.td UI.# UI.set UI.text (showD d) UI.# UI.set (UI.attr "style") cellStyle
                             ]
    header <- UI.tr #+ [ UI.th UI.# UI.set UI.text "Nodo"      UI.# UI.set (UI.attr "style") cellStyle
                        , UI.th UI.# UI.set UI.text "Distancia" UI.# UI.set (UI.attr "style") cellStyle
                        ]
    table <- UI.table UI.# UI.set (UI.attr "style") "width:100%; border-collapse:collapse; table-layout:fixed; text-align:left;"
    _ <- UI.element table #+ (return header : map row ds)
    return table


-- Grafo miniatura para aristas (Kruskal/Prim) manteniendo todos los vértices
renderEdgeGraphView :: Grafo -> [Arista] -> UI.UI UI.Element
renderEdgeGraphView gr@(Grafo vs allEdges _) es
    | null vs = UI.div UI.# UI.set UI.text "Sin vértices"
    | null es = UI.div UI.# UI.set UI.text "Sin aristas"
    | otherwise = do
        -- Usa el mismo layout force-directed del grafo principal pero solo dibuja las aristas del MST
        let positions = forceDirectedLayout vs allEdges
        svg <- SVG.svg UI.# UI.set (UI.attr "width") "600" UI.# UI.set (UI.attr "height") "450"

        -- Aristas del MST con etiqueta de peso (estilo del grafo principal)
        forM_ es $ \(u,v,wgt) -> do
            case (lookup u positions, lookup v positions) of
                (Just (x1,y1), Just (x2,y2)) -> do
                    let dx = x2 - x1; dy = y2 - y1
                        len = sqrt (dx*dx + dy*dy)
                        ux = if len > 0 then dx / len else 0
                        uy = if len > 0 then dy / len else 0
                        -- desplazar la etiqueta hacia el extremo v para no solapar
                        offset = 48 :: Double
                        mx = x2 - ux * offset
                        my = y2 - uy * offset
                        labelW = 24 :: Double
                        labelH = 16 :: Double
                        rx = 4 :: Double
                    line <- SVG.line UI.# UI.set (UI.attr "x1") (show x1) UI.# UI.set (UI.attr "y1") (show y1)
                                          UI.# UI.set (UI.attr "x2") (show x2) UI.# UI.set (UI.attr "y2") (show y2)
                                          UI.# UI.set (UI.attr "stroke") "#666" UI.# UI.set (UI.attr "stroke-width") "3"
                    bg <- SVG.rect UI.# UI.set (UI.attr "x") (show (mx - labelW/2)) UI.# UI.set (UI.attr "y") (show (my - labelH/2))
                                    UI.# UI.set (UI.attr "width") (show labelW) UI.# UI.set (UI.attr "height") (show labelH)
                                    UI.# UI.set (UI.attr "rx") (show rx) UI.# UI.set (UI.attr "ry") (show rx)
                                    UI.# UI.set (UI.attr "fill") "#ffe9b3" UI.# UI.set (UI.attr "stroke") "#444" UI.# UI.set (UI.attr "stroke-width") "1"
                    weight <- SVG.text UI.# UI.set (UI.attr "x") (show mx) UI.# UI.set (UI.attr "y") (show my)
                                          UI.# UI.set UI.text (show wgt) UI.# UI.set (UI.attr "text-anchor") "middle"
                                          UI.# UI.set (UI.attr "font-size") "12" UI.# UI.set (UI.attr "font-weight") "bold"
                                          UI.# UI.set (UI.attr "fill") "#111" UI.# UI.set (UI.attr "dominant-baseline") "middle"
                    _ <- UI.element svg #+ [return line, return bg, return weight]
                    return ()
                _ -> return ()

        -- Todos los nodos del grafo original
        forM_ vs $ \v -> do
            case lookup v positions of
                Nothing -> return ()
                Just (x,y) -> do
                    circ <- SVG.circle UI.# UI.set (UI.attr "cx") (show x)
                                         UI.# UI.set (UI.attr "cy") (show y)
                                         UI.# UI.set (UI.attr "r") "20"
                                         UI.# UI.set (UI.attr "fill") "#e6f2ff"
                                         UI.# UI.set (UI.attr "stroke") "#3b6ea5" UI.# UI.set (UI.attr "stroke-width") "2"
                    label <- SVG.text UI.# UI.set (UI.attr "x") (show x) UI.# UI.set (UI.attr "y") (show y)
                                           UI.# UI.set UI.text (show v) UI.# UI.set (UI.attr "text-anchor") "middle"
                                           UI.# UI.set (UI.attr "dominant-baseline") "middle"
                                           UI.# UI.set (UI.attr "font-size") "12" UI.# UI.set (UI.attr "fill") "#111"
                    _ <- UI.element svg #+ [return circ, return label]
                    return ()

        return svg