package com.github.unclebob418.graph

sealed case class Edge[+IK, +IV, EK, E, +OK, +OV] private (
  inVK: VertexKey[IK, IV],
  edgeKey: EdgeKey[EK, E],
  edge: E,
  outV: VertexKey[IK, IV]
)(
  implicit iVType: VertexType[IK, IV],
  oVType: VertexType[OK, OV],
  cT: ConnectionType[IK, IV, OK, OV],
  eType: EdgeType[IK, IV, EK, E, OK, OV]
) extends GraphComponent[EK, E]
