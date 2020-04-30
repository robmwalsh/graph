package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph.{ Edge, GraphComponent, Vertex }

sealed trait TraversalResult[+K, +V]
object TraversalResult {
  case class VertexList[K, V](vs: List[Vertex[K, V]])                 extends TraversalResult[K, V]
  case class EdgeList[K, V](es: List[Edge[Any, Any, K, V, Any, Any]]) extends TraversalResult[K, V]
  case class Aggregate[V](value: V)                                   extends TraversalResult[Any, V]
  case object Empty                                                   extends TraversalResult[Any, Any]
  case object Error                                                   extends TraversalResult[Any, Any]
}
