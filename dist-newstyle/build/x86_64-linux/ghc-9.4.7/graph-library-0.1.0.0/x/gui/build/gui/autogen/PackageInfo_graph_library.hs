{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_graph_library (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "graph_library"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A simple graph library in Haskell"
copyright :: String
copyright = "2025 Alex"
homepage :: String
homepage = "https://github.com/jery04/Graph-library-in-Haskell#readme"
