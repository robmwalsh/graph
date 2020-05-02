package com.github.unclebob418.graph

import com.github.unclebob418.graph.Key.{EdgeKey, VertexKey}
import com.github.unclebob418.graph.Type.{EdgeType, VertexType}


sealed case class Edge[IK, IV, EK, E, OK, OV] private (
  inVK: VertexKey[IK, IV],
  edgeKey: EdgeKey[IK, IV, EK, E, OK, OV],
  edge: E,
  outV: VertexKey[IK, IV]
)(
  implicit iVType: VertexType[IK, IV],
  oVType: VertexType[OK, OV],
  cT: ConnectionType[IK, IV, OK, OV],
  eType: EdgeType[IK, IV, EK, E, OK, OV]
)
