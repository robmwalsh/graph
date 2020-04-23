package com.github.unclebob418.graph.traversal

import com.github.unclebob418.graph.{ Edge, Vertex }

sealed trait TraversalResult[+A]
object TraversalResult {
  case class VertexList[+A <: List[Vertex[_, _]]](vs: List[A])         extends TraversalResult[A]
  case class EdgeList[+A <: List[Edge[_, _, _, _, _, _]]](es: List[A]) extends TraversalResult[A]
  case class Aggregate[+A](value: A)                                   extends TraversalResult[A]
  case class Path[+A](path: A)                                         extends TraversalResult[A]
}
