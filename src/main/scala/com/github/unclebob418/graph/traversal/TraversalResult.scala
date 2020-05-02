package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph.Key.{ EdgeKey, VertexKey }
import com.github.unclebob418.graph.{ Edge, Vertex }

sealed trait TraversalResult[+K, +V] {}

object TraversalResult {
  def identity[K, V](x: TraversalResult[K, V]): TraversalResult[K, V] = x
  case class VertexKeys[VK, V](vs: List[VertexKey[VK, V]]) extends TraversalResult[VK, V]
  case class EdgeKeys[IK, IV, EK, E, OK, OV](es: List[EdgeKey[IK, IV, EK, E, OK, OV]])
      extends TraversalResult[EK, E]
  //case class Aggregate[V](value: V)                                   extends TraversalResult[Any, V]
  //case object Empty                                                   extends TraversalResult[Any, Any]
  //case object Error                                                   extends TraversalResult[Any, Any]
}
