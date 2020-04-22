package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.{ Edge, GraphSchema, Vertex, VertexType }
import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Source
import com.github.unclebob418.graph.traversal.Traversal.Step.{ EdgeTraversal, VertexTraversal }

object SimpleInterpreter {

  def interpret[VK, V, EK, E, GS <: GraphSchema](traversal: Traversal[VK, V, EK, E, GS]): Either[List[E], List[V]] = ???

  def go[EK, E, VK, V, GS <: GraphSchema](
    traversal: Traversal[VK, V, EK, E, GS]
  )(result: Either[List[E], List[V]]): Either[List[E], List[V]] =
    traversal match {
      case source: Traversal.Source[GS] =>
        source match {
          case t: Source.GraphTraversalSource[_] => Left(List.empty)
        }
      case step: Traversal.Step[VK, V, EK, E, GS] =>
        step match {
          case t: VertexTraversal.VSource[VK, V, GS] =>
            val x = t.tail.graph.getVs[VK, V](t.vType).getOrElse()

          case t: VertexTraversal.VTraversal[VK, V, GS] => ???
          case t: VertexTraversal.Has[VK, V, GS]        => ???
          case t: EdgeTraversal.ESource[ik, iv, EK, E, ok, ov, GS] =>
            val x = t.tail.graph.getEs[ik, iv, EK, E, ok, ov](t.eType)
          case t: EdgeTraversal.ETraversal[ik, iv, EK, E, ok, ov, GS] => ???
          case t: EdgeTraversal.Has[ik, iv, ek, E, ok, ov, GS]        => ???
        }
    }
}
