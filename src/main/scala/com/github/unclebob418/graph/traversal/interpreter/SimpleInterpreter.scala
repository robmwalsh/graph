package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.{ Edge, GraphSchema, Vertex }
import com.github.unclebob418.graph.traversal.Traversal
import com.github.unclebob418.graph.traversal.Traversal.Source
import com.github.unclebob418.graph.traversal.Traversal.Step.{ EdgeTraversal, VertexTraversal }

object DescriptionInterpreter extends TraversalInterpreter[List[String]] {

  def interpret[GS <: GraphSchema](traversal: Traversal[GS]): List[String] = ???

  def go[VK, V, EK, E, IK, IV, OK, OV, GS <: GraphSchema](
    traversal: Traversal[GS],
    f: (List[Either[Vertex[VK, V], Edge[EK, E, IK, IV, OK, OV]]])
  ): List[String] =
    traversal match {
      case source: Traversal.Source[_] =>
        source match {
          case Source.Anonymous(_)                => ???
          case Source.GraphTraversalSource(graph) => ???
        }
      case step: Traversal.Step[_] =>
        step match {
          case VertexTraversal.VSource(vType, tail)    => ???
          case VertexTraversal.VTraversal(vType, tail) => ???
          case VertexTraversal.Has(p, vType, tail)     => ???
          case EdgeTraversal.ESource(eType, tail)      => ???
          case EdgeTraversal.ETraversal(eType, tail)   => ???
          case EdgeTraversal.Has(p, eType, tail)       => ???
        }
    }
}
