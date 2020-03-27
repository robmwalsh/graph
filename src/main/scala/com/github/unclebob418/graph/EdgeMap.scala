package com.github.unclebob418.graph

trait EdgeSchema[VS[_, _] <: VertexSchema[_, _]] {
  type In <: VS[_,_]
  type Out <: VS[_,_]
  type E
}

trait EdgeKey[+K, +V] {
  val key: K
}
sealed case class Edge[K, E, IK, IV, OK, OV](
  inVK: VertexKey[IK, IV],
  edgeKey: EdgeKey[K, E],
  edge: E,
  outV: VertexKey[IK, IV]
)
final case class EdgeMap[VS[_, _] <: VertexSchema[_, _], ES <: EdgeSchema[VS]] private (
  es: Map[Any, Any],
  inVs: Map[Any, Set[Any]],
  outVs: Map[Any, Set[Any]]
) {
  def addE[K, E, IK, IV, OK, OV](inVK: VertexKey[IK, IV], edgeKey: EdgeKey[K, E], e: E, outVK: VertexKey[OK, OV]): EdgeMap[VS, ES] = {
    val edge = Edge(inVK, edgeKey, e, outVK)
    EdgeMap[VS,ES](
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

object EdgeMap {
  def empty[ES <: EdgeSchema[VS], VS[_, _] <: VertexSchema[_, _]]: EdgeMap[VS,ES] =
    EdgeMap[VS,ES](Map.empty[Any, Any], Map.empty[Any, Set[Any]], Map.empty[Any, Set[Any]])
}
