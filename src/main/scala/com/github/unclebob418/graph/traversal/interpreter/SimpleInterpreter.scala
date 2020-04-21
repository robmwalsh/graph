package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.{ Edge, GraphSchema, Vertex }
import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Source
import com.github.unclebob418.graph.traversal.Traversal.Step.{ EdgeTraversal, VertexTraversal }

object SimpleInterpreter {

  def interpret[E, V, GS <: GraphSchema](traversal: Traversal[E, V, GS]): Either[List[E], List[V]] = ???

  def go[E, V, GS <: GraphSchema](traversal: Traversal[E, V, GS]): Either[List[E], List[V]] =
    traversal match {
      case source: Traversal.Source[GS] =>
        source match {
          case Source.GraphTraversalSource(graph) => Left(List.empty)
        }
      case step: Traversal.Step[E, V, GS] =>
        step match {
          case t: VertexTraversal.VSource[k, V, GS]                => val x = Right(t.tail.graph.getVs[k,V](t.vType))
          case t: VertexTraversal.VTraversal[k, V, GS]             => ???
          case t: VertexTraversal.Has[k, V, GS]                    => ???
          /*case t: EdgeTraversal.ESource[ik, iv, ok, ov, ik, e, GS] =>
          case EdgeTraversal.ETraversal(eType, tail)               => ???
          case EdgeTraversal.Has(p, eType, tail)                   => ???*/
          case _ => ???
        }
    }
}
