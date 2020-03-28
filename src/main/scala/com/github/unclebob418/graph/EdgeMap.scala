package com.github.unclebob418.graph

object EdgeMap {
  def empty[VT[_, _] <: VertexType[_, _]]: EdgeMap[VT] =
    EdgeMap[VT](Map.empty[Any, Any], Map.empty[Any, Set[Any]], Map.empty[Any, Set[Any]])
}

final case class EdgeMap[VT[_, _] <: VertexType[_, _]] private(
  es: Map[Any, Any],
  inVs: Map[Any, Set[Any]],
  outVs: Map[Any, Set[Any]]
) {
  def addE[K, E, IK, IV, OK, OV](
    inVK: VertexKey[IK, IV],
    edgeKey: EdgeKey[K, E],
    e: E,
    outVK: VertexKey[OK, OV]
  ): EdgeMap[VT] = {
    val edge = Edge(inVK, edgeKey, e, outVK)
    EdgeMap[VT](
      es + (edgeKey -> edge),
      inVs.get(inVK) match {
        case Some(set: Set[Any]) =>
          inVs + (inVK -> (set + edge))
        case None =>
          inVs + (inVK -> Set(edge))
      },
      outVs.get(outVK) match {
        case Some(set: Set[Any]) =>
          inVs + (outVK -> (set + edge))
        case None =>
          inVs + (outVK -> Set(edge))
      }
    )
  }
}
