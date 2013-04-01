-- | 'Node', 'Graph' and related types.
module Sound.SC3.Server.Synthdef.Type where

import qualified Data.ByteString.Lazy as B {- bytestring -}

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

-- | Node identifier.
type NodeId = Int

-- | Port index.
type PortIndex = Int

-- | Type to represent unit generator graph.
data Graph = Graph {nextId :: NodeId
                   ,constants :: [Node]
                   ,controls :: [Node]
                   ,ugens :: [Node]}
            deriving (Eq,Show)

-- | Binary representation of a unit generator graph.
type Graphdef = B.ByteString

-- | Enumeration of the four operating rates for controls.
data KType = K_IR | K_KR | K_TR | K_AR
             deriving (Eq,Show,Ord)

-- | Type to represent the left hand side of an edge in a unit
--   generator graph.
data FromPort = FromPort_C {port_nid :: NodeId}
              | FromPort_K {port_nid :: NodeId,port_kt :: KType}
              | FromPort_U {port_nid :: NodeId,port_idx :: Maybe PortIndex}
                deriving (Eq,Show)

-- | A destination port.
data ToPort = ToPort NodeId PortIndex deriving (Eq,Show)

-- | A connection from 'FromPort' to 'ToPort'.
type Edge = (FromPort,ToPort)

-- | Type to represent nodes in unit generator graph.
data Node = NodeC {node_id :: NodeId
                  ,node_c_value :: Float}
          | NodeK {node_id :: NodeId
                  ,node_k_rate :: Rate
                  ,node_k_name :: String
                  ,node_k_default :: Float
                  ,node_k_type :: KType}
          | NodeU {node_id :: NodeId
                  ,node_u_rate :: Rate
                  ,node_u_name :: String
                  ,node_u_inputs :: [FromPort]
                  ,node_u_outputs :: [Output]
                  ,node_u_special :: Special
                  ,node_u_ugenid :: UGenId}
          | NodeP {node_id :: NodeId
                  ,node_p_node :: Node
                  ,node_p_index :: PortIndex}
            deriving (Eq,Show)

