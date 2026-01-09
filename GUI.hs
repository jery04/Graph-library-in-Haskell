module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny ( (#+), liftIO )
import Graph
import Render
import Data.List (nub, intercalate, nubBy)
import Text.Read (readMaybe)
import Control.Monad (void)
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