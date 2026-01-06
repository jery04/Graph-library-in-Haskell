module Main where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG
import Graphics.UI.Threepenny ( (#+) )
import Graph
import Data.List (nub)
import Text.Read (readMaybe)
import Control.Monad (forM, forM_)

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

renderGraph :: Grafo -> UI.UI UI.Element
renderGraph (Grafo vertices edges _) = do
    let positions = case length vertices of
            1 -> [(1, (300, 225))]
            2 -> [(1, (200, 225)), (2, (400, 225))]
            3 -> [(1, (200, 150)), (2, (400, 150)), (3, (300, 300))]
            4 -> [(1, (150, 150)), (2, (450, 150)), (3, (150, 300)), (4, (450, 300))]
            5 -> [(1, (150, 150)), (2, (450, 150)), (3, (100, 300)), (4, (300, 300)), (5, (500, 300))]
            6 -> [(1, (150, 150)), (2, (450, 150)), (3, (100, 225)), (4, (500, 225)), (5, (150, 300)), (6, (450, 300))]
            7 -> [(1, (150, 150)), (2, (300, 150)), (3, (450, 150)), (4, (100, 225)), (5, (500, 225)), (6, (200, 300)), (7, (400, 300))]
            8 -> [(1, (150, 150)), (2, (300, 150)), (3, (450, 150)), (4, (100, 225)), (5, (500, 225)), (6, (150, 300)), (7, (300, 300)), (8, (450, 300))]
            9 -> [(1, (125, 150)), (2, (300, 150)), (3, (475, 150)), (4, (100, 225)), (5, (300, 225)), (6, (500, 225)), (7, (125, 300)), (8, (300, 300)), (9, (475, 300))]
            _ -> [(1, (150, 150)), (2, (450, 150)), (3, (100, 300)), (4, (300, 300)), (5, (500, 300))] ++ [(v, (150 + (v-6)*50, 375)) | v <- [6..length vertices]]
    
    svg <- SVG.svg UI.# UI.set (UI.attr "width") "600" UI.# UI.set (UI.attr "height") "450"
    
    forM_ edges $ \(a,b,_) -> do
        let Just (x1,y1) = lookup a positions
            Just (x2,y2) = lookup b positions
        line <- SVG.line UI.# UI.set (UI.attr "x1") (show x1) UI.# UI.set (UI.attr "y1") (show y1) UI.# UI.set (UI.attr "x2") (show x2) UI.# UI.set (UI.attr "y2") (show y2) UI.# UI.set (UI.attr "stroke") "black" UI.# UI.set (UI.attr "stroke-width") "2"
        UI.element svg #+ [UI.element line]
    
    forM_ (take (length vertices) positions) $ \(n, (x,y)) -> do
        circle <- SVG.circle UI.# UI.set (UI.attr "cx") (show x) UI.# UI.set (UI.attr "cy") (show y) UI.# UI.set (UI.attr "r") "25" UI.# UI.set (UI.attr "fill") "lightblue" UI.# UI.set (UI.attr "stroke") "blue" UI.# UI.set (UI.attr "stroke-width") "2"
        text <- SVG.text UI.# UI.set (UI.attr "x") (show x) UI.# UI.set (UI.attr "y") (show (y+5)) UI.# UI.set UI.text (show n) UI.# UI.set (UI.attr "text-anchor") "middle" UI.# UI.set (UI.attr "font-size") "12"
        UI.element svg #+ [UI.element circle, UI.element text]
    
    return svg