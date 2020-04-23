package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.{Edge, EdgeKey, GraphSchema, Vertex, VertexKey, VertexType}
import com.github.unclebob418.graph.traversal.{Traversal, TraversalResult}
import com.github.unclebob418.graph.traversal.Traversal.Source
import com.github.unclebob418.graph.traversal.Traversal.Step.{EdgeTraversal, VertexTraversal}

object SimpleInterpreter {

  def interpret[A, VK, V, IK, IV, EK, E, OK, OV, GS <: GraphSchema](
    traversal: Traversal[VK, V, IK, IV, EK, E, OK, OV, GS]
  ): TraversalResult[A] =
    ???

  //todo make tail-recursive
  def go[VK, V, IK, IV, EK, E, OK, OV, GS <: GraphSchema](
    traversal: Traversal[VK, V, IK, IV, EK, E, OK, OV, GS]
  ): Either[List[Edge[IK, IV, EK, E, OK, OV]], List[Vertex[VK, V]]] =
    traversal match {
      case source: Traversal.Source[GS] =>
        source match {
          case t: Source.GraphTraversalSource[_] => Left(List.empty)
        }
      case step: Traversal.Step[VK, V, IK, IV, EK, E, OK, OV, GS] =>
        step match {
          case t: VertexTraversal.VSource[VK, V, GS] =>
            val res =
              t.tail.graph
                .getVs[VK, V](t.vType)
                .values
                .toList
            Right(res)
          case t: VertexTraversal.VTraversal[VK, V, GS] => ??? //go(t.tail).flatMap()
          case t: VertexTraversal.Has[VK, V, GS] =>
            Right(go(t.tail).getOrElse(List.empty[Vertex[VK, V]]).filter(v => t.p(v.value)))
          case t: EdgeTraversal.ESource[IK, IV, EK, E, OK, OV, GS] =>
            val res = t.tail.graph
              .getEs[IK, IV, EK, E, OK, OV](t.eType)
              .values
              .toList
            Right(res)
          case t: EdgeTraversal.ETraversal[IK, IV, EK, E, OK, OV, GS] => ???
          case t: EdgeTraversal.Has[IK, IV, EK, E, OK, OV, GS]        => ???
        }
    }
}
