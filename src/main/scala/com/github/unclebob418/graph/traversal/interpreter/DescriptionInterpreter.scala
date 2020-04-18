package com.github.unclebob418.graph.traversal.interpreter

import com.github.unclebob418.graph.traversal.Traversal.Source
import com.github.unclebob418.graph.traversal.Traversal.Step.{ EdgeTraversal, VertexTraversal }
import com.github.unclebob418.graph.GraphSchema
import com.github.unclebob418.graph.traversal.Traversal

object DescriptionInterpreter extends TraversalInterpreter[List[String]] {

  def interpret[GS <: GraphSchema](traversal: Traversal[GS]): List[String] = go(traversal).reverse

  def go[GS <: GraphSchema](traversal: Traversal[GS]): List[String] =
    traversal match {
      case source: Traversal.Source[_] =>
        source match {
          case Source.Anonymous(_) =>
            List("Anonymous Source")
          case Source.GraphTraversalSource(graph) =>
            List(s"GraphSource")
        }
      case step: Traversal.Step[_] =>
        step match {
          case VertexTraversal.VTraversal(vType, tail) =>
            s"$vType vertex step" :: go(tail)
          case VertexTraversal.Has(p, vType, tail) =>
            s"$vType has vertex step" :: go(tail)
          case EdgeTraversal.ETraversal(eType, tail) =>
            s"$eType edge step" :: go(tail)
          case EdgeTraversal.Has(p, eType, tail) =>
            s"$eType has edge step" :: go(tail)
        }
    }
}
