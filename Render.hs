module Render (
    forceDirectedLayout,
    renderGraph,
    renderBipartiteView,
    renderOrderView,
    renderEdgeListView,
    renderDistancesView,
    renderEdgeGraphView
) where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG
import Graphics.UI.Threepenny ((#+))
import Graph
import Data.List (intercalate)
import Control.Monad (forM, forM_, when)
import qualified Data.Map as Map
import Numeric (showFFloat)

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
        k = sqrt (600 * 450 / fromIntegral n)  -- ideal distance
        iterations = 50
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
        iterateLayout pos 0 = pos
        iterateLayout pos i =
            let forces = calculateForces pos
                newPos = Map.mapWithKey (\v (x,y) ->
                    let (fx,fy) = forces Map.! v
                        nx = x + fx * 0.1
                        ny = y + fy * 0.1
                    in (max 50 (min 550 nx), max 50 (min 400 ny))
                    ) pos
            in if i >= iterations then newPos else iterateLayout newPos (i+1)
    in Map.toList $ iterateLayout initialPositions 0
    where
        dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

renderGraph :: Grafo -> UI.UI UI.Element
renderGraph (Grafo vertices edges isDirected) = do
    let fmt p = (\s -> if s == "" then "0" else s) . showFFloat (Just 2) p
        showPt (x,y) = fmt x "" ++ " " ++ fmt y ""
        positions = forceDirectedLayout vertices edges

    svg <- SVG.svg UI.# UI.set (UI.attr "width") "600" UI.# UI.set (UI.attr "height") "450"

    forM_ edges $ \(a,b,w) -> do
        let Just (x1,y1) = lookup a positions
            Just (x2,y2) = lookup b positions
        let lineAttrs = [UI.set (UI.attr "x1") (show x1), UI.set (UI.attr "y1") (show y1), UI.set (UI.attr "x2") (show x2), UI.set (UI.attr "y2") (show y2), UI.set (UI.attr "stroke") "#222", UI.set (UI.attr "stroke-width") "2", UI.set (UI.attr "stroke-linecap") "round"]
        line <- foldl (UI.#) (SVG.line) lineAttrs
        _ <- UI.element svg #+ [return line]

        when (isDirected && (x1 /= x2 || y1 /= y2)) $ do
            let dx = x2 - x1
                dy = y2 - y1
                len = sqrt (dx*dx + dy*dy)
            when (len > 0.1) $ do
                let ux = dx / len
                    uy = dy / len
                    tipBack = 22
                    baseBack = 40
                    tipX = x2 - ux * tipBack
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

        let dx = x2 - x1
            dy = y2 - y1
            len = sqrt (dx*dx + dy*dy)
            (mx,my) = if len > 0.1
                      then let ux = dx / len
                               uy = dy / len
                               offset = 48
                           in (x2 - ux * offset, y2 - uy * offset)
                      else ((x1 + x2) / 2, (y1 + y2) / 2)
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

    forM_ crossEdges $ \(a,b,_) -> case (lookupPos a, lookupPos b) of
        (Just (x1,y1), Just (x2,y2)) -> do
            line <- SVG.line UI.# UI.set (UI.attr "x1") (show x1) UI.# UI.set (UI.attr "y1") (show y1)
                                  UI.# UI.set (UI.attr "x2") (show x2) UI.# UI.set (UI.attr "y2") (show y2)
                                  UI.# UI.set (UI.attr "stroke") "#888" UI.# UI.set (UI.attr "stroke-width") "2"
            _ <- UI.element svg #+ [return line]
            return ()
        _ -> return ()

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


-- Vista compacta de orden de visita
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


renderEdgeGraphView :: Grafo -> [Arista] -> UI.UI UI.Element
renderEdgeGraphView gr@(Grafo vs allEdges _) es
    | null vs = UI.div UI.# UI.set UI.text "Sin vértices"
    | null es = UI.div UI.# UI.set UI.text "Sin aristas"
    | otherwise = do
        let positions = forceDirectedLayout vs allEdges
        svg <- SVG.svg UI.# UI.set (UI.attr "width") "600" UI.# UI.set (UI.attr "height") "450"

        forM_ es $ \(u,v,wgt) -> do
            case (lookup u positions, lookup v positions) of
                (Just (x1,y1), Just (x2,y2)) -> do
                    let dx = x2 - x1; dy = y2 - y1
                        len = sqrt (dx*dx + dy*dy)
                        ux = if len > 0 then dx / len else 0
                        uy = if len > 0 then dy / len else 0
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
