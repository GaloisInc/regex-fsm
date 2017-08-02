module Regex.Graph where

import Regex.Types
import           Data.Graph.Inductive (Gr, mkGraph)
import qualified Data.Graph.Inductive as G

-- | Graph representation of ENFA
type Graph a = Gr Int (Move Int a)

toGraph :: ENFA s a -> Graph a
toGraph = undefined

-- | Show EENFA as Graph
enfa :: Graph Char
enfa = mkGraph nodes edges
  where
    nodes =  [ one, two, three ]
    edges =  [ (1, 2, Move 'a' 2)
             , (2, 3, Move 'b' 3)
             ]
    [one,two,three] = [(1,1),(2,2),(3,3)]
