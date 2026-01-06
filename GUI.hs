module Main where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG
import Graphics.UI.Threepenny ( (#+) )
import Graph
import Data.List (nub, intercalate)
import Text.Read (readMaybe)
import Control.Monad (forM, forM_, when)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Numeric (showFFloat)

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
              "label { display: block; margin-bottom: 5px; font-weight: bold; }"
    
    styleElement <- UI.mkElement "style" UI.# UI.set UI.text css
    
    -- Título
    title <- UI.h1 UI.# UI.set UI.text "Constructor de Grafos"
    
    -- Formulario
    numNodesInput <- UI.input UI.# UI.set UI.type_ "number" UI.# UI.set (UI.attr "placeholder") "Número de nodos"
    
    edgesTextarea <- UI.textarea UI.# UI.set (UI.attr "placeholder") "Aristas (formato: nodo1-nodo2:peso, separadas por coma)\nEjemplo: 1-2:5,2-3:3,1-3:2"
    
    directedCheckbox <- UI.input UI.# UI.set UI.type_ "checkbox"
    directedLabel <- UI.label UI.# UI.set UI.text "Dirigido:"
    
    createButton <- UI.button UI.# UI.set UI.text "Crear Grafo"
    
    outputDiv <- UI.div
    
    -- Evento del botón
    UI.on UI.click createButton $ \_ -> do
        numNodesStr <- UI.get UI.value numNodesInput
        edgesStr <- UI.get UI.value edgesTextarea
        isDirected <- UI.get UI.checked directedCheckbox
        
        case parseInputs numNodesStr edgesStr isDirected of
            Left err -> UI.element outputDiv UI.# UI.set UI.text ("Error: " ++ err)
            Right grafo -> do
                svg <- renderGraph grafo
                UI.element outputDiv UI.# UI.set UI.children [svg]
    
    -- Layout
    UI.getBody window #+ [
        UI.element styleElement,
        UI.element title,
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
        UI.element outputDiv
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
        let (mx,my) = if isDirected && (x1 /= x2 || y1 /= y2)
                      then
                          let dx = x2 - x1
                              dy = y2 - y1
                              len = sqrt (dx*dx + dy*dy)
                          in if len > 0.1
                             then let ux = dx / len
                                      uy = dy / len
                                      offset = 48  -- coloca el peso justo detrás de la flecha
                                  in (x2 - ux * offset, y2 - uy * offset)
                             else ((x1 + x2) / 2, (y1 + y2) / 2)
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